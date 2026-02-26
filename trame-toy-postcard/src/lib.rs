use facet_core::{ConstTypeId, Def, Facet, Shape, Type, UserType};
use trame::TrameError;

#[cfg(feature = "dynasm-rt")]
use core::ffi::c_void;
#[cfg(feature = "dynasm-rt")]
use core::marker::PhantomData;
#[cfg(feature = "dynasm-rt")]
use core::mem::MaybeUninit;
#[cfg(all(feature = "dynasm-rt", test))]
use core::sync::atomic::{AtomicBool, Ordering};
#[cfg(feature = "dynasm-rt")]
use dynasmrt::{AssemblyOffset, DynasmApi, DynasmLabelApi, dynasm};

const POSTCARD_DECODE_ABI_V1: u32 = 1;

#[cfg(all(feature = "dynasm-rt", test))]
static FORCE_DYNASM_COMPILE_FAIL: AtomicBool = AtomicBool::new(false);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
    RootNotStruct {
        type_name: &'static str,
    },
    RootNotList {
        type_name: &'static str,
    },
    ListElementNotStruct {
        type_name: &'static str,
    },
    UnsupportedFieldType {
        field_index: usize,
        field_name: &'static str,
        type_name: &'static str,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    Compile(CompileError),
    ShapeMismatch,
    UnexpectedEof { offset: usize },
    VarintOverflow { offset: usize },
    InvalidUtf8 { offset: usize },
    InvalidBool { offset: usize, value: u8 },
    TrailingBytes { offset: usize },
    InvalidRegister { reg: usize },
    RegisterUnset { reg: usize },
    Trame(TrameError),
}

impl From<TrameError> for Error {
    fn from(value: TrameError) -> Self {
        Self::Trame(value)
    }
}

impl From<CompileError> for Error {
    fn from(value: CompileError) -> Self {
        Self::Compile(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarKind {
    U32,
    String,
    Bool,
}

impl ScalarKind {
    fn as_symbol(self) -> &'static str {
        match self {
            ScalarKind::U32 => "u32",
            ScalarKind::String => "string",
            ScalarKind::Bool => "bool",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DecodeInstr {
    ReadScalar { kind: ScalarKind, dst: u8 },
    WriteFieldFromReg { field: u32, src: u8 },
    RequireEof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PostcardDecodeProgram {
    pub abi_version: u32,
    pub shape_id: ConstTypeId,
    pub register_count: usize,
    pub instructions: Vec<DecodeInstr>,
}

impl PostcardDecodeProgram {
    pub fn to_sexp(&self) -> String {
        use core::fmt::Write as _;

        let mut out = String::new();
        let _ = writeln!(&mut out, "(postcard-decode-program");
        let _ = writeln!(&mut out, "  (abi {})", self.abi_version);
        let _ = writeln!(&mut out, "  (shape-id {:?})", self.shape_id);
        let _ = writeln!(&mut out, "  (register-count {})", self.register_count);
        let _ = writeln!(&mut out, "  (instructions");
        for instr in &self.instructions {
            match *instr {
                DecodeInstr::ReadScalar { kind, dst } => {
                    let _ = writeln!(
                        &mut out,
                        "    (read-scalar (kind {}) (dst r{}))",
                        kind.as_symbol(),
                        dst
                    );
                }
                DecodeInstr::WriteFieldFromReg { field, src } => {
                    let _ = writeln!(
                        &mut out,
                        "    (write-field-from-reg (field {}) (src r{}))",
                        field, src
                    );
                }
                DecodeInstr::RequireEof => {
                    let _ = writeln!(&mut out, "    (require-eof)");
                }
            }
        }
        let _ = writeln!(&mut out, "  )");
        let _ = write!(&mut out, ")");
        out
    }
}

/// Compile-time validated decode plan for postcard struct slices.
///
/// Supported scalar fields in v0: `u32`, `String`, `bool`.
#[derive(Clone, Debug)]
pub struct PostcardStructPlan {
    shape: &'static Shape,
    program: PostcardDecodeProgram,
    field_plans: Vec<StructFieldPlan>,
}

/// Compile-time validated decode plan for postcard `Vec<Struct>` slices.
#[derive(Clone, Debug)]
pub struct PostcardVecStructPlan {
    shape: &'static Shape,
    element_plan: PostcardStructPlan,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct StructFieldPlan {
    kind: ScalarKind,
    offset: usize,
}

// t[impl format.exec.backend-interface-stable]
pub trait DecodeBackend {
    fn decode<T>(plan: &PostcardStructPlan, input: &[u8]) -> Result<T, Error>
    where
        T: Facet<'static>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackendKind {
    Interpreter,
    #[cfg(feature = "dynasm-rt")]
    DynasmRt,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct InterpreterBackend;

impl DecodeBackend for InterpreterBackend {
    fn decode<T>(plan: &PostcardStructPlan, input: &[u8]) -> Result<T, Error>
    where
        T: Facet<'static>,
    {
        decode_with_interpreter(plan, input)
    }
}

#[cfg(feature = "dynasm-rt")]
#[derive(Debug, Clone, Copy, Default)]
pub struct DynasmRtBackend;

#[cfg(feature = "dynasm-rt")]
impl DecodeBackend for DynasmRtBackend {
    // t[impl format.exec.jit-optional]
    // t[impl format.exec.jit-semantic-equivalence]
    // t[impl format.exec.jit-compile-failure-fallback]
    fn decode<T>(plan: &PostcardStructPlan, input: &[u8]) -> Result<T, Error>
    where
        T: Facet<'static>,
    {
        decode_with_dynasm(plan, input)
    }
}

#[cfg(feature = "dynasm-rt")]
pub struct DynasmPrepared<T>
where
    T: Facet<'static>,
{
    plan: PostcardStructPlan,
    trampoline: DynasmTrampoline,
    _marker: PhantomData<fn() -> T>,
}

#[cfg(feature = "dynasm-rt")]
impl<T> DynasmPrepared<T>
where
    T: Facet<'static>,
{
    pub fn decode(&self, input: &[u8]) -> Result<T, Error> {
        let (value, _) = self.decode_at(input, 0)?;
        Ok(value)
    }

    fn decode_at(&self, input: &[u8], start_pos: usize) -> Result<(T, usize), Error> {
        decode_with_trampoline_at::<T>(&self.plan, &self.trampoline, input, start_pos)
    }
}

#[cfg(feature = "dynasm-rt")]
pub struct DynasmPreparedVec<T>
where
    T: Facet<'static>,
{
    element: DynasmPrepared<T>,
}

#[cfg(feature = "dynasm-rt")]
impl<T> DynasmPreparedVec<T>
where
    T: Facet<'static>,
{
    pub fn decode(&self, input: &[u8]) -> Result<Vec<T>, Error> {
        decode_vec_with_dynasm_prepared(&self.element, input)
    }
}

impl PostcardStructPlan {
    // t[impl format.parse.single-program-lex-and-build]
    // t[impl format.ir.program-self-contained]
    // t[impl format.ir.program-abi-version]
    // t[impl format.ir.program-root-shape]
    pub fn compile(shape: &'static Shape) -> Result<Self, CompileError> {
        compile_struct_plan(shape, true)
    }

    pub fn compile_for<T>() -> Result<Self, CompileError>
    where
        T: Facet<'static>,
    {
        Self::compile(T::SHAPE)
    }

    #[cfg(feature = "dynasm-rt")]
    pub fn prepare_dynasm<T>(&self) -> Option<DynasmPrepared<T>>
    where
        T: Facet<'static>,
    {
        self.ensure_shape::<T>().ok()?;
        let program = DynasmCompiledProgram::from_plan::<T>(self)?;
        let trampoline = DynasmTrampoline::compile(&program)?;
        Some(DynasmPrepared {
            plan: self.clone(),
            trampoline,
            _marker: PhantomData,
        })
    }

    pub fn decode_with<T, B>(&self, input: &[u8]) -> Result<T, Error>
    where
        T: Facet<'static>,
        B: DecodeBackend,
    {
        self.ensure_shape::<T>()?;
        B::decode(self, input)
    }

    pub fn decode_on<T>(&self, input: &[u8], backend: BackendKind) -> Result<T, Error>
    where
        T: Facet<'static>,
    {
        match backend {
            BackendKind::Interpreter => self.decode_with::<T, InterpreterBackend>(input),
            #[cfg(feature = "dynasm-rt")]
            BackendKind::DynasmRt => self.decode_with::<T, DynasmRtBackend>(input),
        }
    }

    // t[impl format.exec.interpreter-is-reference]
    // t[impl format.ir.program-self-contained]
    pub fn decode<T>(&self, input: &[u8]) -> Result<T, Error>
    where
        T: Facet<'static>,
    {
        self.decode_with::<T, InterpreterBackend>(input)
    }

    pub fn program(&self) -> &PostcardDecodeProgram {
        &self.program
    }

    fn ensure_shape<T>(&self) -> Result<(), Error>
    where
        T: Facet<'static>,
    {
        if self.shape.id != T::SHAPE.id || self.program.shape_id != T::SHAPE.id {
            return Err(Error::ShapeMismatch);
        }
        Ok(())
    }
}

impl PostcardVecStructPlan {
    pub fn compile(shape: &'static Shape) -> Result<Self, CompileError> {
        let Def::List(list_def) = shape.def else {
            return Err(CompileError::RootNotList {
                type_name: shape.type_identifier,
            });
        };
        let element_shape = list_def.t;
        if !matches!(element_shape.ty, Type::User(UserType::Struct(_))) {
            return Err(CompileError::ListElementNotStruct {
                type_name: element_shape.type_identifier,
            });
        }
        let element_plan = compile_struct_plan(element_shape, false)?;
        Ok(Self {
            shape,
            element_plan,
        })
    }

    pub fn compile_for<T>() -> Result<Self, CompileError>
    where
        T: Facet<'static>,
    {
        Self::compile(T::SHAPE)
    }

    pub fn decode_vec_on<T>(&self, input: &[u8], backend: BackendKind) -> Result<Vec<T>, Error>
    where
        T: Facet<'static>,
    {
        self.ensure_vec_shape::<T>()?;
        match backend {
            BackendKind::Interpreter => decode_vec_with_interpreter(self, input),
            #[cfg(feature = "dynasm-rt")]
            BackendKind::DynasmRt => decode_vec_with_dynasm(self, input),
        }
    }

    pub fn decode_vec<T>(&self, input: &[u8]) -> Result<Vec<T>, Error>
    where
        T: Facet<'static>,
    {
        self.decode_vec_on::<T>(input, BackendKind::Interpreter)
    }

    #[cfg(feature = "dynasm-rt")]
    pub fn prepare_dynasm<T>(&self) -> Option<DynasmPreparedVec<T>>
    where
        T: Facet<'static>,
    {
        self.ensure_vec_shape::<T>().ok()?;
        let element = self.element_plan.prepare_dynasm::<T>()?;
        Some(DynasmPreparedVec { element })
    }

    fn ensure_vec_shape<T>(&self) -> Result<(), Error>
    where
        T: Facet<'static>,
    {
        if self.shape.id != <Vec<T>>::SHAPE.id || self.element_plan.program.shape_id != T::SHAPE.id
        {
            return Err(Error::ShapeMismatch);
        }
        Ok(())
    }
}

pub type PostcardStructU32StringPlan = PostcardStructPlan;
pub type PostcardVecU32StringPlan = PostcardVecStructPlan;

pub fn compile<T>() -> Result<PostcardStructPlan, CompileError>
where
    T: Facet<'static>,
{
    PostcardStructPlan::compile_for::<T>()
}

pub fn compile_vec<T>() -> Result<PostcardVecStructPlan, CompileError>
where
    T: Facet<'static>,
{
    PostcardVecStructPlan::compile_for::<T>()
}

pub fn from_slice<T>(input: &[u8]) -> Result<T, Error>
where
    T: Facet<'static>,
{
    let plan = compile::<T>()?;
    plan.decode(input)
}

pub fn from_slice_on<T>(input: &[u8], backend: BackendKind) -> Result<T, Error>
where
    T: Facet<'static>,
{
    let plan = compile::<T>()?;
    plan.decode_on::<T>(input, backend)
}

pub fn from_slice_vec<T>(input: &[u8]) -> Result<Vec<T>, Error>
where
    T: Facet<'static>,
{
    let plan = compile_vec::<Vec<T>>()?;
    plan.decode_vec::<T>(input)
}

pub fn from_slice_vec_on<T>(input: &[u8], backend: BackendKind) -> Result<Vec<T>, Error>
where
    T: Facet<'static>,
{
    let plan = compile_vec::<Vec<T>>()?;
    plan.decode_vec_on::<T>(input, backend)
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum RegValue {
    Unset,
    U32(u32),
    String(String),
    Bool(bool),
}

struct Reader<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Reader<'a> {
    fn new(input: &'a [u8]) -> Self {
        Self { input, pos: 0 }
    }

    fn offset(&self) -> usize {
        self.pos
    }

    fn is_eof(&self) -> bool {
        self.pos == self.input.len()
    }

    fn read_byte(&mut self) -> Result<u8, Error> {
        let Some(&byte) = self.input.get(self.pos) else {
            return Err(Error::UnexpectedEof { offset: self.pos });
        };
        self.pos += 1;
        Ok(byte)
    }

    // postcard integer encoding is unsigned LEB128 for u32.
    fn read_u32(&mut self) -> Result<u32, Error> {
        let start = self.pos;
        let mut out = 0u32;

        for shift in [0, 7, 14, 21, 28] {
            let byte = self.read_byte()?;
            let data = (byte & 0x7f) as u32;
            out |= data << shift;

            if (byte & 0x80) == 0 {
                // u32 needs at most 5 bytes; top nibble must be empty.
                if shift == 28 && (data & 0xf0) != 0 {
                    return Err(Error::VarintOverflow { offset: start });
                }
                return Ok(out);
            }
        }

        Err(Error::VarintOverflow { offset: start })
    }

    fn read_bytes(&mut self, len: usize) -> Result<&'a [u8], Error> {
        let end = self
            .pos
            .checked_add(len)
            .ok_or(Error::UnexpectedEof { offset: self.pos })?;
        if end > self.input.len() {
            return Err(Error::UnexpectedEof { offset: self.pos });
        }
        let out = &self.input[self.pos..end];
        self.pos = end;
        Ok(out)
    }

    fn read_string_bytes(&mut self) -> Result<(usize, &'a [u8]), Error> {
        let len = self.read_u32()? as usize;
        let start = self.pos;
        let bytes = self.read_bytes(len)?;
        Ok((start, bytes))
    }

    fn read_bool(&mut self) -> Result<bool, Error> {
        let offset = self.pos;
        let value = self.read_byte()?;
        match value {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(Error::InvalidBool { offset, value }),
        }
    }
}

fn scalar_kind_for_shape(shape: &'static Shape) -> Option<ScalarKind> {
    if shape.type_identifier == "u32" {
        return Some(ScalarKind::U32);
    }
    if shape.type_identifier == "String" {
        return Some(ScalarKind::String);
    }
    if shape.type_identifier == "bool" {
        return Some(ScalarKind::Bool);
    }
    None
}

fn write_field_from_reg(
    out_ptr: *mut u8,
    field_plans: &[StructFieldPlan],
    field_inits: &mut [u8],
    field_idx: usize,
    src_reg_idx: usize,
    reg: &mut RegValue,
) -> Result<(), Error> {
    let field = field_plans
        .get(field_idx)
        .ok_or(Error::InvalidRegister { reg: field_idx })?;
    let init_flag = field_inits
        .get_mut(field_idx)
        .ok_or(Error::InvalidRegister { reg: field_idx })?;
    let dst = unsafe { out_ptr.add(field.offset) };

    match (field.kind, reg) {
        (ScalarKind::U32, RegValue::U32(value)) => {
            unsafe { (dst as *mut u32).write(*value) };
        }
        (ScalarKind::Bool, RegValue::Bool(value)) => {
            unsafe { (dst as *mut bool).write(*value) };
        }
        (ScalarKind::String, RegValue::String(value)) => {
            if *init_flag != 0 {
                unsafe { core::ptr::drop_in_place(dst as *mut String) };
            }
            unsafe { (dst as *mut String).write(core::mem::take(value)) };
        }
        (_, RegValue::Unset) => return Err(Error::RegisterUnset { reg: src_reg_idx }),
        _ => return Err(Error::ShapeMismatch),
    }
    *init_flag = 1;
    Ok(())
}

unsafe fn drop_initialized_fields(
    out_ptr: *mut u8,
    field_plans: &[StructFieldPlan],
    field_inits: &[u8],
) {
    for (field, init) in field_plans.iter().zip(field_inits.iter()) {
        if *init == 0 {
            continue;
        }
        if field.kind == ScalarKind::String {
            let dst = unsafe { out_ptr.add(field.offset) };
            unsafe { core::ptr::drop_in_place(dst as *mut String) };
        }
    }
}

fn compile_struct_plan(
    shape: &'static Shape,
    require_eof: bool,
) -> Result<PostcardStructPlan, CompileError> {
    let Type::User(UserType::Struct(st)) = shape.ty else {
        return Err(CompileError::RootNotStruct {
            type_name: shape.type_identifier,
        });
    };

    let mut instructions = Vec::with_capacity(st.fields.len() * 2 + 1);
    let mut field_plans = Vec::with_capacity(st.fields.len());
    for (idx, field) in st.fields.iter().enumerate() {
        let field_shape = field.shape.get();
        let Some(kind) = scalar_kind_for_shape(field_shape) else {
            return Err(CompileError::UnsupportedFieldType {
                field_index: idx,
                field_name: field.name,
                type_name: field_shape.type_identifier,
            });
        };
        instructions.push(DecodeInstr::ReadScalar {
            kind,
            dst: idx as u8,
        });
        instructions.push(DecodeInstr::WriteFieldFromReg {
            field: idx as u32,
            src: idx as u8,
        });
        field_plans.push(StructFieldPlan {
            kind,
            offset: field.offset,
        });
    }
    if require_eof {
        instructions.push(DecodeInstr::RequireEof);
    }
    let program = PostcardDecodeProgram {
        abi_version: POSTCARD_DECODE_ABI_V1,
        shape_id: shape.id,
        register_count: st.fields.len(),
        instructions,
    };
    Ok(PostcardStructPlan {
        shape,
        program,
        field_plans,
    })
}

fn decode_struct_with_reader<T>(
    plan: &PostcardStructPlan,
    reader: &mut Reader<'_>,
) -> Result<T, Error>
where
    T: Facet<'static>,
{
    let mut regs = vec![RegValue::Unset; plan.program.register_count];
    let mut field_inits = vec![0u8; plan.field_plans.len()];
    let mut out = core::mem::MaybeUninit::<T>::uninit();
    for instr in &plan.program.instructions {
        let step = match *instr {
            DecodeInstr::ReadScalar { kind, dst } => {
                let slot = regs
                    .get_mut(dst as usize)
                    .ok_or(Error::InvalidRegister { reg: dst as usize })?;
                *slot = match kind {
                    ScalarKind::U32 => RegValue::U32(reader.read_u32()?),
                    ScalarKind::String => {
                        let (str_offset, str_bytes) = reader.read_string_bytes()?;
                        let value = core::str::from_utf8(str_bytes)
                            .map_err(|_| Error::InvalidUtf8 { offset: str_offset })?
                            .to_owned();
                        RegValue::String(value)
                    }
                    ScalarKind::Bool => RegValue::Bool(reader.read_bool()?),
                };
                Ok(())
            }
            DecodeInstr::WriteFieldFromReg { field, src } => {
                let reg = regs
                    .get_mut(src as usize)
                    .ok_or(Error::InvalidRegister { reg: src as usize })?;
                write_field_from_reg(
                    out.as_mut_ptr().cast(),
                    &plan.field_plans,
                    &mut field_inits,
                    field as usize,
                    src as usize,
                    reg,
                )
            }
            DecodeInstr::RequireEof => {
                if !reader.is_eof() {
                    return Err(Error::TrailingBytes {
                        offset: reader.offset(),
                    });
                }
                Ok(())
            }
        };
        if let Err(err) = step {
            unsafe {
                drop_initialized_fields(out.as_mut_ptr().cast(), &plan.field_plans, &field_inits);
            }
            return Err(err);
        }
    }
    if let Some(missing) = field_inits.iter().position(|init| *init == 0) {
        unsafe {
            drop_initialized_fields(out.as_mut_ptr().cast(), &plan.field_plans, &field_inits);
        }
        return Err(Error::RegisterUnset { reg: missing });
    }
    Ok(unsafe { out.assume_init() })
}

fn decode_with_interpreter<T>(plan: &PostcardStructPlan, input: &[u8]) -> Result<T, Error>
where
    T: Facet<'static>,
{
    let mut reader = Reader::new(input);
    decode_struct_with_reader::<T>(plan, &mut reader)
}

fn decode_vec_with_interpreter<T>(
    plan: &PostcardVecStructPlan,
    input: &[u8],
) -> Result<Vec<T>, Error>
where
    T: Facet<'static>,
{
    let mut reader = Reader::new(input);
    let len = reader.read_u32()? as usize;
    let mut out = Vec::with_capacity(len);
    for _ in 0..len {
        out.push(decode_struct_with_reader::<T>(
            &plan.element_plan,
            &mut reader,
        )?);
    }
    if !reader.is_eof() {
        return Err(Error::TrailingBytes {
            offset: reader.offset(),
        });
    }
    Ok(out)
}

#[cfg(feature = "dynasm-rt")]
#[repr(C)]
struct JitCallContext<T> {
    input_ptr: *const u8,
    input_len: usize,
    pos: usize,
    field_inits: Vec<u8>,
    field_inits_ptr: *mut u8,
    failed: bool,
    error_tag: JitErrorTag,
    error_offset: usize,
    error_value: u32,
    out: MaybeUninit<T>,
}

#[cfg(feature = "dynasm-rt")]
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum JitErrorTag {
    None = 0,
    UnexpectedEof = 1,
    VarintOverflow = 2,
    InvalidUtf8 = 3,
    InvalidBool = 4,
    TrailingBytes = 5,
    InvalidRegister = 6,
    RegisterUnset = 7,
    ShapeMismatch = 8,
}

#[cfg(feature = "dynasm-rt")]
impl JitErrorTag {
    fn from_byte(byte: u8) -> Self {
        match byte {
            1 => Self::UnexpectedEof,
            2 => Self::VarintOverflow,
            3 => Self::InvalidUtf8,
            4 => Self::InvalidBool,
            5 => Self::TrailingBytes,
            6 => Self::InvalidRegister,
            7 => Self::RegisterUnset,
            8 => Self::ShapeMismatch,
            _ => Self::None,
        }
    }
}

#[cfg(feature = "dynasm-rt")]
#[derive(Clone, Copy)]
struct JitContextOffsets {
    input_ptr: u32,
    input_len: u32,
    pos: u32,
    field_inits_ptr: u32,
    failed: u32,
    error_tag: u32,
    error_offset: u32,
    error_value: u32,
    out: u32,
}

#[cfg(feature = "dynasm-rt")]
fn jit_context_offsets<T>() -> JitContextOffsets
where
    T: Facet<'static>,
{
    JitContextOffsets {
        input_ptr: core::mem::offset_of!(JitCallContext<T>, input_ptr) as u32,
        input_len: core::mem::offset_of!(JitCallContext<T>, input_len) as u32,
        pos: core::mem::offset_of!(JitCallContext<T>, pos) as u32,
        field_inits_ptr: core::mem::offset_of!(JitCallContext<T>, field_inits_ptr) as u32,
        failed: core::mem::offset_of!(JitCallContext<T>, failed) as u32,
        error_tag: core::mem::offset_of!(JitCallContext<T>, error_tag) as u32,
        error_offset: core::mem::offset_of!(JitCallContext<T>, error_offset) as u32,
        error_value: core::mem::offset_of!(JitCallContext<T>, error_value) as u32,
        out: core::mem::offset_of!(JitCallContext<T>, out) as u32,
    }
}

#[cfg(feature = "dynasm-rt")]
unsafe fn jit_ctx<'a, T>(ctx: *mut c_void) -> &'a mut JitCallContext<T>
where
    T: Facet<'static>,
{
    unsafe { &mut *ctx.cast::<JitCallContext<T>>() }
}

#[cfg(feature = "dynasm-rt")]
fn jit_set_failure<T>(ctx: &mut JitCallContext<T>, tag: JitErrorTag, offset: usize, value: u32)
where
    T: Facet<'static>,
{
    if !ctx.failed {
        ctx.failed = true;
        ctx.error_tag = tag;
        ctx.error_offset = offset;
        ctx.error_value = value;
    }
}

#[cfg(feature = "dynasm-rt")]
fn jit_set_error_from_error<T>(ctx: &mut JitCallContext<T>, error: Error)
where
    T: Facet<'static>,
{
    match error {
        Error::UnexpectedEof { offset } => {
            jit_set_failure(ctx, JitErrorTag::UnexpectedEof, offset, 0);
        }
        Error::VarintOverflow { offset } => {
            jit_set_failure(ctx, JitErrorTag::VarintOverflow, offset, 0);
        }
        Error::InvalidUtf8 { offset } => {
            jit_set_failure(ctx, JitErrorTag::InvalidUtf8, offset, 0);
        }
        Error::InvalidBool { offset, value } => {
            jit_set_failure(ctx, JitErrorTag::InvalidBool, offset, value as u32);
        }
        Error::TrailingBytes { offset } => {
            jit_set_failure(ctx, JitErrorTag::TrailingBytes, offset, 0);
        }
        Error::InvalidRegister { reg } => {
            jit_set_failure(ctx, JitErrorTag::InvalidRegister, reg, 0);
        }
        Error::RegisterUnset { reg } => {
            jit_set_failure(ctx, JitErrorTag::RegisterUnset, reg, 0);
        }
        Error::ShapeMismatch => {
            jit_set_failure(ctx, JitErrorTag::ShapeMismatch, 0, 0);
        }
        Error::Compile(_) | Error::Trame(_) => {
            jit_set_failure(ctx, JitErrorTag::ShapeMismatch, 0, 0);
        }
    }
}

#[cfg(feature = "dynasm-rt")]
fn jit_error_from_context<T>(ctx: &JitCallContext<T>) -> Error
where
    T: Facet<'static>,
{
    match JitErrorTag::from_byte(ctx.error_tag as u8) {
        JitErrorTag::UnexpectedEof => Error::UnexpectedEof {
            offset: ctx.error_offset,
        },
        JitErrorTag::VarintOverflow => Error::VarintOverflow {
            offset: ctx.error_offset,
        },
        JitErrorTag::InvalidUtf8 => Error::InvalidUtf8 {
            offset: ctx.error_offset,
        },
        JitErrorTag::InvalidBool => Error::InvalidBool {
            offset: ctx.error_offset,
            value: ctx.error_value as u8,
        },
        JitErrorTag::TrailingBytes => Error::TrailingBytes {
            offset: ctx.error_offset,
        },
        JitErrorTag::InvalidRegister => Error::InvalidRegister {
            reg: ctx.error_offset,
        },
        JitErrorTag::RegisterUnset => Error::RegisterUnset {
            reg: ctx.error_offset,
        },
        JitErrorTag::ShapeMismatch | JitErrorTag::None => Error::ShapeMismatch,
    }
}

#[cfg(feature = "dynasm-rt")]
fn jit_read_byte<T>(ctx: &mut JitCallContext<T>) -> Result<u8, Error>
where
    T: Facet<'static>,
{
    let pos = ctx.pos;
    if pos >= ctx.input_len {
        return Err(Error::UnexpectedEof { offset: pos });
    }
    let byte = unsafe { *ctx.input_ptr.add(pos) };
    ctx.pos = pos + 1;
    Ok(byte)
}

#[cfg(feature = "dynasm-rt")]
fn jit_read_u32<T>(ctx: &mut JitCallContext<T>) -> Result<u32, Error>
where
    T: Facet<'static>,
{
    let start = ctx.pos;
    let mut out = 0u32;
    for shift in [0, 7, 14, 21, 28] {
        let byte = jit_read_byte(ctx)?;
        let data = (byte & 0x7f) as u32;
        out |= data << shift;
        if (byte & 0x80) == 0 {
            if shift == 28 && (data & 0xf0) != 0 {
                return Err(Error::VarintOverflow { offset: start });
            }
            return Ok(out);
        }
    }
    Err(Error::VarintOverflow { offset: start })
}

#[cfg(feature = "dynasm-rt")]
fn jit_read_string<T>(ctx: &mut JitCallContext<T>) -> Result<String, Error>
where
    T: Facet<'static>,
{
    let len = jit_read_u32(ctx)? as usize;
    let start = ctx.pos;
    let end = start
        .checked_add(len)
        .ok_or(Error::UnexpectedEof { offset: start })?;
    if end > ctx.input_len {
        return Err(Error::UnexpectedEof { offset: start });
    }
    let bytes = unsafe { core::slice::from_raw_parts(ctx.input_ptr.add(start), len) };
    ctx.pos = end;
    let value = core::str::from_utf8(bytes).map_err(|_| Error::InvalidUtf8 { offset: start })?;
    Ok(value.to_owned())
}

#[cfg(feature = "dynasm-rt")]
unsafe extern "C" fn jit_op_read_write_field_string<T>(
    ctx: *mut c_void,
    arg1: u64,
    arg2: u64,
) -> i32
where
    T: Facet<'static>,
{
    let ctx = unsafe { jit_ctx::<T>(ctx) };
    if ctx.failed {
        return 1;
    }
    let field = arg1 as usize;
    let offset = arg2 as usize;
    if field >= ctx.field_inits.len() {
        jit_set_failure(ctx, JitErrorTag::InvalidRegister, field, 0);
        return 1;
    }
    let value = match jit_read_string(ctx) {
        Ok(v) => v,
        Err(e) => {
            jit_set_error_from_error(ctx, e);
            return 1;
        }
    };
    let dst = unsafe {
        ctx.out
            .as_mut_ptr()
            .cast::<u8>()
            .add(offset)
            .cast::<String>()
    };
    if ctx.field_inits[field] != 0 {
        unsafe { core::ptr::drop_in_place(dst) };
    }
    unsafe { dst.write(value) };
    ctx.field_inits[field] = 1;
    0
}

#[cfg(feature = "dynasm-rt")]
#[derive(Clone, Copy)]
enum JitDynasmOp {
    ReadWriteU32 { field: u32, offset: usize },
    ReadWriteBool { field: u32, offset: usize },
    ReadWriteString { field: u32, offset: usize },
}

#[cfg(feature = "dynasm-rt")]
struct DynasmCompiledProgram {
    ops: Vec<JitDynasmOp>,
    require_eof: bool,
    string_helper: usize,
    context_offsets: JitContextOffsets,
}

#[cfg(feature = "dynasm-rt")]
impl DynasmCompiledProgram {
    fn from_plan<T>(plan: &PostcardStructPlan) -> Option<Self>
    where
        T: Facet<'static>,
    {
        let mut ops = Vec::with_capacity(plan.field_plans.len());
        let mut require_eof = false;
        let mut idx = 0usize;
        while let Some(instr) = plan.program.instructions.get(idx) {
            match (*instr, plan.program.instructions.get(idx + 1).copied()) {
                (
                    DecodeInstr::ReadScalar { kind, dst },
                    Some(DecodeInstr::WriteFieldFromReg { field, src }),
                ) if dst == src => {
                    let field_idx = field as usize;
                    let field_plan = plan.field_plans.get(field_idx)?;
                    if field_plan.kind != kind {
                        return None;
                    }
                    let op = match kind {
                        ScalarKind::U32 => JitDynasmOp::ReadWriteU32 {
                            field,
                            offset: field_plan.offset,
                        },
                        ScalarKind::Bool => JitDynasmOp::ReadWriteBool {
                            field,
                            offset: field_plan.offset,
                        },
                        ScalarKind::String => JitDynasmOp::ReadWriteString {
                            field,
                            offset: field_plan.offset,
                        },
                    };
                    ops.push(op);
                    idx += 2;
                }
                (DecodeInstr::RequireEof, _) => {
                    require_eof = true;
                    idx += 1;
                }
                _ => return None,
            }
        }
        Some(Self {
            ops,
            require_eof,
            string_helper: jit_op_read_write_field_string::<T> as *const () as usize,
            context_offsets: jit_context_offsets::<T>(),
        })
    }
}

#[cfg(feature = "dynasm-rt")]
struct DynasmTrampoline {
    _buffer: dynasmrt::ExecutableBuffer,
    entry: unsafe extern "C" fn(*mut c_void) -> i32,
}

#[cfg(feature = "dynasm-rt")]
impl DynasmTrampoline {
    fn compile(program: &DynasmCompiledProgram) -> Option<Self> {
        #[cfg(test)]
        if FORCE_DYNASM_COMPILE_FAIL.load(Ordering::Relaxed) {
            return None;
        }

        #[cfg(target_arch = "aarch64")]
        {
            Self::compile_aarch64(program)
        }
        #[cfg(target_arch = "x86_64")]
        {
            Self::compile_x64(program)
        }
        #[cfg(not(any(target_arch = "aarch64", target_arch = "x86_64")))]
        {
            let _ = program;
            None
        }
    }

    #[cfg(target_arch = "aarch64")]
    fn compile_aarch64(program: &DynasmCompiledProgram) -> Option<Self> {
        let mut ops = dynasmrt::aarch64::Assembler::new().ok()?;
        let o = program.context_offsets;
        let return_label = ops.new_dynamic_label();
        let err_eof = ops.new_dynamic_label();
        let err_overflow = ops.new_dynamic_label();
        let err_invalid_bool = ops.new_dynamic_label();
        let err_trailing = ops.new_dynamic_label();
        let entry = ops.offset();
        dynasm!(ops
            ; .arch aarch64
            ; sub sp, sp, #16
            ; str x0, [sp]
            ; str x30, [sp, #8]
            ; ldr x8, [sp]
            ; ldr x5, [x8, #o.input_ptr]
            ; ldr x6, [x8, #o.input_len]
            ; ldr x7, [x8, #o.pos]
        );

        for op in &program.ops {
            match *op {
                JitDynasmOp::ReadWriteU32 { field, offset } => {
                    emit_aarch64_decode_u32(&mut ops, err_eof, err_overflow);
                    emit_aarch64_store_field_u32(&mut ops, o.out as usize + offset);
                    emit_aarch64_mark_field_init(&mut ops, o, field as usize);
                }
                JitDynasmOp::ReadWriteBool { field, offset } => {
                    dynasm!(ops
                        ; .arch aarch64
                        ; mov x1, x7
                        ; cmp x7, x6
                        ; b.hs =>err_eof
                        ; ldrb w11, [x5, x7]
                        ; add x7, x7, #1
                        ; cmp w11, #0
                        ; b.eq >bool_ok
                        ; cmp w11, #1
                        ; b.eq >bool_ok
                        ; mov w2, w11
                        ; b =>err_invalid_bool
                        ; bool_ok:
                    );
                    emit_aarch64_store_field_bool(&mut ops, o.out as usize + offset);
                    emit_aarch64_mark_field_init(&mut ops, o, field as usize);
                }
                JitDynasmOp::ReadWriteString { field, offset } => {
                    dynasm!(ops
                        ; .arch aarch64
                        ; ldr x8, [sp]
                        ; str x7, [x8, #o.pos]
                        ; ldr x0, [sp]
                    );
                    emit_aarch64_load_u64(&mut ops, 1, field as u64);
                    emit_aarch64_load_u64(&mut ops, 2, offset as u64);
                    emit_aarch64_load_u64(&mut ops, 16, program.string_helper as u64);
                    dynasm!(ops
                        ; .arch aarch64
                        ; blr x16
                        ; cbnz w0, =>return_label
                        ; ldr x8, [sp]
                        ; ldr x5, [x8, #o.input_ptr]
                        ; ldr x6, [x8, #o.input_len]
                        ; ldr x7, [x8, #o.pos]
                    );
                }
            }
        }

        if program.require_eof {
            dynasm!(ops
                ; .arch aarch64
                ; cmp x7, x6
                ; b.eq >eof_ok
                ; mov x1, x7
                ; b =>err_trailing
                ; eof_ok:
            );
        }

        dynasm!(ops
            ; .arch aarch64
            ; ldr x8, [sp]
            ; str x7, [x8, #o.pos]
            ; mov w0, #0
            ; b =>return_label
        );

        dynasm!(ops
            ; .arch aarch64
            ; =>err_eof
            ; mov w2, wzr
        );
        emit_aarch64_write_failure(&mut ops, o, JitErrorTag::UnexpectedEof);
        dynasm!(ops
            ; .arch aarch64
            ; mov w0, #1
            ; b =>return_label
        );

        dynasm!(ops
            ; .arch aarch64
            ; =>err_overflow
            ; mov w2, wzr
        );
        emit_aarch64_write_failure(&mut ops, o, JitErrorTag::VarintOverflow);
        dynasm!(ops
            ; .arch aarch64
            ; mov w0, #1
            ; b =>return_label
        );

        dynasm!(ops
            ; .arch aarch64
            ; =>err_invalid_bool
        );
        emit_aarch64_write_failure(&mut ops, o, JitErrorTag::InvalidBool);
        dynasm!(ops
            ; .arch aarch64
            ; mov w0, #1
            ; b =>return_label
        );

        dynasm!(ops
            ; .arch aarch64
            ; =>err_trailing
            ; mov w2, wzr
        );
        emit_aarch64_write_failure(&mut ops, o, JitErrorTag::TrailingBytes);
        dynasm!(ops
            ; .arch aarch64
            ; mov w0, #1
            ; b =>return_label
        );

        dynasm!(ops
            ; .arch aarch64
            ; =>return_label
            ; ldr x30, [sp, #8]
            ; add sp, sp, #16
            ; ret
        );
        let buffer = ops.finalize().ok()?;
        Some(Self::from_buffer(buffer, entry))
    }

    #[cfg(target_arch = "x86_64")]
    fn compile_x64(program: &DynasmCompiledProgram) -> Option<Self> {
        let _ = program;
        None
    }

    fn from_buffer(buffer: dynasmrt::ExecutableBuffer, entry: AssemblyOffset) -> Self {
        let ptr = buffer.ptr(entry);
        let entry = unsafe {
            core::mem::transmute::<*const u8, unsafe extern "C" fn(*mut c_void) -> i32>(ptr)
        };
        Self {
            _buffer: buffer,
            entry,
        }
    }

    unsafe fn call(&self, ctx: *mut c_void) -> i32 {
        unsafe { (self.entry)(ctx) }
    }
}

#[cfg(all(feature = "dynasm-rt", target_arch = "aarch64"))]
fn emit_aarch64_write_failure(
    ops: &mut dynasmrt::aarch64::Assembler,
    o: JitContextOffsets,
    tag: JitErrorTag,
) {
    let tag = tag as u32;
    dynasm!(ops
        ; .arch aarch64
        ; ldr x8, [sp]
        ; mov w9, #1
        ; strb w9, [x8, #o.failed]
        ; mov w9, #tag
        ; strb w9, [x8, #o.error_tag]
        ; str x1, [x8, #o.error_offset]
        ; str w2, [x8, #o.error_value]
    );
}

#[cfg(all(feature = "dynasm-rt", target_arch = "aarch64"))]
fn emit_aarch64_mark_field_init(
    ops: &mut dynasmrt::aarch64::Assembler,
    o: JitContextOffsets,
    field: usize,
) {
    dynasm!(ops
        ; .arch aarch64
        ; ldr x8, [sp]
        ; ldr x9, [x8, #o.field_inits_ptr]
    );
    emit_aarch64_load_u64(ops, 10, field as u64);
    dynasm!(ops
        ; .arch aarch64
        ; add x9, x9, x10
        ; mov w10, #1
        ; strb w10, [x9]
    );
}

#[cfg(all(feature = "dynasm-rt", target_arch = "aarch64"))]
fn emit_aarch64_store_field_u32(ops: &mut dynasmrt::aarch64::Assembler, offset: usize) {
    dynasm!(ops
        ; .arch aarch64
        ; ldr x8, [sp]
    );
    emit_aarch64_load_u64(ops, 9, offset as u64);
    dynasm!(ops
        ; .arch aarch64
        ; add x8, x8, x9
        ; str w11, [x8]
    );
}

#[cfg(all(feature = "dynasm-rt", target_arch = "aarch64"))]
fn emit_aarch64_store_field_bool(ops: &mut dynasmrt::aarch64::Assembler, offset: usize) {
    dynasm!(ops
        ; .arch aarch64
        ; ldr x8, [sp]
    );
    emit_aarch64_load_u64(ops, 9, offset as u64);
    dynasm!(ops
        ; .arch aarch64
        ; add x8, x8, x9
        ; strb w11, [x8]
    );
}

#[cfg(all(feature = "dynasm-rt", target_arch = "aarch64"))]
fn emit_aarch64_decode_u32(
    ops: &mut dynasmrt::aarch64::Assembler,
    err_eof: dynasmrt::DynamicLabel,
    err_overflow: dynasmrt::DynamicLabel,
) {
    let done = ops.new_dynamic_label();
    let byte5_ok_nibble = ops.new_dynamic_label();
    dynasm!(ops
        ; .arch aarch64
        ; mov x15, x7
        ; mov w11, wzr

        ; mov x1, x7
        ; cmp x7, x6
        ; b.hs =>err_eof
        ; ldrb w10, [x5, x7]
        ; add x7, x7, #1
        ; and w12, w10, #0x7f
        ; orr w11, w11, w12
        ; tbz w10, #7, =>done

        ; mov x1, x7
        ; cmp x7, x6
        ; b.hs =>err_eof
        ; ldrb w10, [x5, x7]
        ; add x7, x7, #1
        ; and w12, w10, #0x7f
        ; lsl w12, w12, #7
        ; orr w11, w11, w12
        ; tbz w10, #7, =>done

        ; mov x1, x7
        ; cmp x7, x6
        ; b.hs =>err_eof
        ; ldrb w10, [x5, x7]
        ; add x7, x7, #1
        ; and w12, w10, #0x7f
        ; lsl w12, w12, #14
        ; orr w11, w11, w12
        ; tbz w10, #7, =>done

        ; mov x1, x7
        ; cmp x7, x6
        ; b.hs =>err_eof
        ; ldrb w10, [x5, x7]
        ; add x7, x7, #1
        ; and w12, w10, #0x7f
        ; lsl w12, w12, #21
        ; orr w11, w11, w12
        ; tbz w10, #7, =>done

        ; mov x1, x7
        ; cmp x7, x6
        ; b.hs =>err_eof
        ; ldrb w10, [x5, x7]
        ; add x7, x7, #1
        ; and w12, w10, #0x7f
        ; tst w12, #0xf0
        ; b.eq =>byte5_ok_nibble
        ; mov x1, x15
        ; b =>err_overflow
        ; =>byte5_ok_nibble
        ; lsl w12, w12, #28
        ; orr w11, w11, w12
        ; tbz w10, #7, =>done
        ; mov x1, x15
        ; b =>err_overflow

        ; =>done
    );
}

#[cfg(all(feature = "dynasm-rt", target_arch = "aarch64"))]
fn emit_aarch64_load_u64(ops: &mut dynasmrt::aarch64::Assembler, reg: u8, value: u64) {
    let i0 = (value & 0xffff) as u16;
    let i1 = ((value >> 16) & 0xffff) as u16;
    let i2 = ((value >> 32) & 0xffff) as u16;
    let i3 = ((value >> 48) & 0xffff) as u16;
    ops.push_u32(encode_aarch64_movz(reg, i0, 0));
    ops.push_u32(encode_aarch64_movk(reg, i1, 1));
    ops.push_u32(encode_aarch64_movk(reg, i2, 2));
    ops.push_u32(encode_aarch64_movk(reg, i3, 3));
}

#[cfg(all(feature = "dynasm-rt", target_arch = "aarch64"))]
fn encode_aarch64_movz(rd: u8, imm16: u16, hw: u8) -> u32 {
    0xD2800000 | ((hw as u32) << 21) | ((imm16 as u32) << 5) | (rd as u32)
}

#[cfg(all(feature = "dynasm-rt", target_arch = "aarch64"))]
fn encode_aarch64_movk(rd: u8, imm16: u16, hw: u8) -> u32 {
    0xF2800000 | ((hw as u32) << 21) | ((imm16 as u32) << 5) | (rd as u32)
}

#[cfg(feature = "dynasm-rt")]
fn decode_with_dynasm<T>(plan: &PostcardStructPlan, input: &[u8]) -> Result<T, Error>
where
    T: Facet<'static>,
{
    let Some(prepared) = plan.prepare_dynasm::<T>() else {
        return decode_with_interpreter(plan, input);
    };
    prepared.decode(input)
}

#[cfg(feature = "dynasm-rt")]
fn decode_vec_with_dynasm<T>(plan: &PostcardVecStructPlan, input: &[u8]) -> Result<Vec<T>, Error>
where
    T: Facet<'static>,
{
    let Some(prepared) = plan.prepare_dynasm::<T>() else {
        return decode_vec_with_interpreter(plan, input);
    };
    decode_vec_with_dynasm_prepared(&prepared.element, input)
}

#[cfg(feature = "dynasm-rt")]
fn decode_vec_with_dynasm_prepared<T>(
    prepared: &DynasmPrepared<T>,
    input: &[u8],
) -> Result<Vec<T>, Error>
where
    T: Facet<'static>,
{
    let mut reader = Reader::new(input);
    let len = reader.read_u32()? as usize;
    let mut out = RawVecBuilder::<T>::with_capacity(len);
    let mut pos = reader.offset();
    for _ in 0..len {
        let (value, next_pos) = prepared.decode_at(input, pos)?;
        unsafe { out.push_unchecked(value) };
        pos = next_pos;
    }
    if pos != input.len() {
        return Err(Error::TrailingBytes { offset: pos });
    }
    Ok(out.finish())
}

#[cfg(feature = "dynasm-rt")]
fn decode_with_trampoline_at<T>(
    plan: &PostcardStructPlan,
    trampoline: &DynasmTrampoline,
    input: &[u8],
    start_pos: usize,
) -> Result<(T, usize), Error>
where
    T: Facet<'static>,
{
    let mut ctx = JitCallContext::<T> {
        input_ptr: input.as_ptr(),
        input_len: input.len(),
        pos: start_pos,
        field_inits: vec![0u8; plan.field_plans.len()],
        field_inits_ptr: core::ptr::null_mut(),
        failed: false,
        error_tag: JitErrorTag::None,
        error_offset: 0,
        error_value: 0,
        out: MaybeUninit::uninit(),
    };
    ctx.field_inits_ptr = ctx.field_inits.as_mut_ptr();

    let status = unsafe { trampoline.call((&mut ctx as *mut JitCallContext<T>).cast()) };
    match status {
        0 => Ok((unsafe { ctx.out.assume_init() }, ctx.pos)),
        1 => {
            unsafe {
                drop_initialized_fields(
                    ctx.out.as_mut_ptr().cast(),
                    &plan.field_plans,
                    &ctx.field_inits,
                );
            }
            Err(jit_error_from_context(&ctx))
        }
        _ => {
            unsafe {
                drop_initialized_fields(
                    ctx.out.as_mut_ptr().cast(),
                    &plan.field_plans,
                    &ctx.field_inits,
                );
            }
            let mut reader = Reader::new(&input[start_pos..]);
            let value = decode_struct_with_reader::<T>(plan, &mut reader)?;
            Ok((value, start_pos + reader.offset()))
        }
    }
}

#[cfg(feature = "dynasm-rt")]
struct RawVecBuilder<T> {
    ptr: *mut T,
    len: usize,
    cap: usize,
}

#[cfg(feature = "dynasm-rt")]
impl<T> RawVecBuilder<T> {
    fn with_capacity(cap: usize) -> Self {
        let mut vec = Vec::<T>::with_capacity(cap);
        let ptr = vec.as_mut_ptr();
        let cap = vec.capacity();
        core::mem::forget(vec);
        Self { ptr, len: 0, cap }
    }

    unsafe fn push_unchecked(&mut self, value: T) {
        debug_assert!(self.len < self.cap);
        unsafe { self.ptr.add(self.len).write(value) };
        self.len += 1;
    }

    fn finish(mut self) -> Vec<T> {
        let ptr = self.ptr;
        let len = self.len;
        let cap = self.cap;
        self.ptr = core::ptr::null_mut();
        self.len = 0;
        self.cap = 0;
        unsafe { Vec::from_raw_parts(ptr, len, cap) }
    }
}

#[cfg(feature = "dynasm-rt")]
impl<T> Drop for RawVecBuilder<T> {
    fn drop(&mut self) {
        if self.ptr.is_null() {
            return;
        }
        unsafe {
            drop(Vec::from_raw_parts(self.ptr, self.len, self.cap));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;

    #[derive(Debug, PartialEq, facet::Facet)]
    struct Demo {
        id: u32,
        name: String,
    }

    #[derive(Debug, PartialEq, facet::Facet)]
    struct Other {
        a: u32,
        b: String,
    }

    #[derive(Debug, PartialEq, facet::Facet)]
    struct Demo3 {
        id: u32,
        name: String,
        ok: bool,
    }

    #[derive(Debug, PartialEq, facet::Facet)]
    struct WrongType {
        only: i64,
    }

    fn encode_u32(mut value: u32) -> Vec<u8> {
        let mut out = Vec::new();
        loop {
            let low = (value & 0x7f) as u8;
            value >>= 7;
            if value == 0 {
                out.push(low);
                return out;
            }
            out.push(low | 0x80);
        }
    }

    fn encode_demo_wire(id: u32, name: &str) -> Vec<u8> {
        let mut out = encode_u32(id);
        out.extend(encode_u32(name.len() as u32));
        out.extend(name.as_bytes());
        out
    }

    fn encode_demo3_wire(id: u32, name: &str, ok: bool) -> Vec<u8> {
        let mut out = encode_demo_wire(id, name);
        out.push(if ok { 1 } else { 0 });
        out
    }

    fn snapshot_program(program: &PostcardDecodeProgram) -> String {
        program
            .to_sexp()
            .lines()
            .map(|line| {
                if line.trim_start().starts_with("(shape-id TypeId(") {
                    "  (shape-id TypeId(<opaque>))".to_string()
                } else {
                    line.to_string()
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    #[cfg(feature = "dynasm-rt")]
    struct DynasmCompileFailGuard;

    #[cfg(feature = "dynasm-rt")]
    impl DynasmCompileFailGuard {
        fn activate() -> Self {
            FORCE_DYNASM_COMPILE_FAIL.store(true, Ordering::SeqCst);
            Self
        }
    }

    #[cfg(feature = "dynasm-rt")]
    impl Drop for DynasmCompileFailGuard {
        fn drop(&mut self) {
            FORCE_DYNASM_COMPILE_FAIL.store(false, Ordering::SeqCst);
        }
    }

    #[test]
    // t[verify format.exec.interpreter-is-reference]
    fn decode_demo() {
        let wire = encode_demo_wire(7, "alice");
        let value: Demo = from_slice(&wire).expect("decode should succeed");
        assert_eq!(
            value,
            Demo {
                id: 7,
                name: "alice".into()
            }
        );
    }

    #[test]
    // t[verify format.exec.backend-interface-stable]
    fn decode_with_explicit_interpreter_backend() {
        let plan = compile::<Demo>().expect("compile should succeed");
        let wire = encode_demo_wire(7, "alice");
        let value: Demo = plan
            .decode_with::<Demo, InterpreterBackend>(&wire)
            .expect("decode should succeed");
        assert_eq!(
            value,
            Demo {
                id: 7,
                name: "alice".into()
            }
        );
    }

    #[test]
    // t[verify format.exec.backend-interface-stable]
    // t[verify format.exec.jit-optional]
    fn decode_with_runtime_backend_selector() {
        let plan = compile::<Demo>().expect("compile should succeed");
        let wire = encode_demo_wire(7, "alice");
        let value: Demo = plan
            .decode_on::<Demo>(&wire, BackendKind::Interpreter)
            .expect("decode should succeed");
        assert_eq!(
            value,
            Demo {
                id: 7,
                name: "alice".into()
            }
        );
    }

    #[cfg(feature = "dynasm-rt")]
    #[test]
    // t[verify format.exec.backend-interface-stable]
    fn decode_with_dynasm_backend_feature_path() {
        let plan = compile::<Demo>().expect("compile should succeed");
        let wire = encode_demo_wire(9, "bob");
        let value: Demo = plan
            .decode_with::<Demo, DynasmRtBackend>(&wire)
            .expect("decode should succeed");
        assert_eq!(
            value,
            Demo {
                id: 9,
                name: "bob".into()
            }
        );
    }

    #[cfg(feature = "dynasm-rt")]
    #[test]
    // t[verify format.exec.jit-semantic-equivalence]
    fn dynasm_and_interpreter_backends_are_equivalent() {
        let plan = compile::<Demo3>().expect("compile should succeed");
        let wire = encode_demo3_wire(11, "eve", true);
        let from_interpreter: Demo3 = plan
            .decode_on::<Demo3>(&wire, BackendKind::Interpreter)
            .expect("interpreter should succeed");
        let from_dynasm: Demo3 = plan
            .decode_on::<Demo3>(&wire, BackendKind::DynasmRt)
            .expect("dynasm backend should succeed");
        assert_eq!(from_dynasm, from_interpreter);
    }

    #[cfg(feature = "dynasm-rt")]
    #[test]
    // t[verify format.exec.jit-compile-failure-fallback]
    fn dynasm_compile_failure_falls_back_to_interpreter() {
        let _guard = DynasmCompileFailGuard::activate();
        let plan = compile::<Demo>().expect("compile should succeed");
        let wire = encode_demo_wire(13, "mallory");
        let value: Demo = plan
            .decode_on::<Demo>(&wire, BackendKind::DynasmRt)
            .expect("fallback decode should succeed");
        assert_eq!(
            value,
            Demo {
                id: 13,
                name: "mallory".into()
            }
        );
    }

    #[test]
    fn compile_rejects_unsupported_field_type() {
        let err = compile::<WrongType>().expect_err("shape should be rejected");
        assert_eq!(
            err,
            CompileError::UnsupportedFieldType {
                field_index: 0,
                field_name: "only",
                type_name: "i64",
            }
        );
    }

    #[test]
    // t[verify format.parse.single-program-lex-and-build]
    // t[verify format.ir.program-self-contained]
    fn compile_emits_program() {
        let plan = compile::<Demo>().expect("compile should succeed");
        assert_snapshot!(
            "compile_demo_program_sexp",
            snapshot_program(plan.program())
        );
    }

    #[test]
    fn compile_is_shape_driven_for_multiple_fields() {
        let plan = compile::<Demo3>().expect("compile should succeed");
        assert_snapshot!(
            "compile_demo3_program_sexp",
            snapshot_program(plan.program())
        );
    }

    #[test]
    // t[verify format.ir.program-abi-version]
    // t[verify format.ir.program-root-shape]
    fn compile_emits_program_guards() {
        let plan = compile::<Demo>().expect("compile should succeed");
        assert_eq!(plan.program().abi_version, POSTCARD_DECODE_ABI_V1);
        assert_eq!(plan.program().shape_id, Demo::SHAPE.id);
    }

    #[test]
    // t[verify format.ir.program-self-contained]
    fn decode_rejects_shape_mismatch() {
        let plan = compile::<Demo>().expect("compile should succeed");
        let wire = encode_demo_wire(1, "x");
        let err = plan
            .decode::<Other>(&wire)
            .expect_err("mismatched shape should fail");
        assert_eq!(err, Error::ShapeMismatch);
    }

    #[test]
    fn decode_rejects_trailing_bytes() {
        let mut wire = encode_demo_wire(1, "x");
        wire.push(0x42);
        let err = from_slice::<Demo>(&wire).expect_err("trailing byte should fail");
        assert_eq!(
            err,
            Error::TrailingBytes {
                offset: wire.len() - 1
            }
        );
    }

    #[test]
    fn decode_rejects_invalid_utf8() {
        let mut wire = encode_u32(1);
        wire.extend(encode_u32(2));
        wire.extend([0xc3, 0x28]); // invalid UTF-8

        let err = from_slice::<Demo>(&wire).expect_err("invalid utf8 should fail");
        assert_eq!(err, Error::InvalidUtf8 { offset: 2 });
    }

    #[test]
    fn decode_shape_driven_three_fields() {
        let wire = encode_demo3_wire(7, "alice", true);
        let value: Demo3 = from_slice(&wire).expect("decode should succeed");
        assert_eq!(
            value,
            Demo3 {
                id: 7,
                name: "alice".into(),
                ok: true,
            }
        );
    }

    #[test]
    fn compile_vec_rejects_non_list_root() {
        let err = compile_vec::<Demo>().expect_err("non-list root should fail");
        assert_eq!(err, CompileError::RootNotList { type_name: "Demo" });
    }

    #[test]
    fn compile_vec_rejects_non_struct_elements() {
        let err = compile_vec::<Vec<u32>>().expect_err("scalar elements should be rejected");
        assert_eq!(err, CompileError::ListElementNotStruct { type_name: "u32" });
    }

    #[test]
    fn decode_vec_shape_driven() {
        let expected = vec![
            Demo3 {
                id: 1,
                name: "alice".into(),
                ok: true,
            },
            Demo3 {
                id: 2,
                name: "bob".into(),
                ok: false,
            },
            Demo3 {
                id: 3,
                name: "eve".into(),
                ok: true,
            },
        ];
        let wire = facet_postcard::to_vec(&expected).expect("encode should succeed");
        let plan =
            PostcardVecStructPlan::compile_for::<Vec<Demo3>>().expect("compile should succeed");
        let actual = plan
            .decode_vec_on::<Demo3>(&wire, BackendKind::Interpreter)
            .expect("decode should succeed");
        assert_eq!(actual, expected);
    }

    #[cfg(feature = "dynasm-rt")]
    #[test]
    fn decode_vec_dynasm_and_interpreter_are_equivalent() {
        let expected = vec![
            Demo3 {
                id: 10,
                name: "mallory".into(),
                ok: true,
            },
            Demo3 {
                id: 11,
                name: "trent".into(),
                ok: false,
            },
        ];
        let wire = facet_postcard::to_vec(&expected).expect("encode should succeed");
        let plan =
            PostcardVecStructPlan::compile_for::<Vec<Demo3>>().expect("compile should succeed");
        let from_interpreter = plan
            .decode_vec_on::<Demo3>(&wire, BackendKind::Interpreter)
            .expect("interpreter should succeed");
        let from_dynasm = plan
            .decode_vec_on::<Demo3>(&wire, BackendKind::DynasmRt)
            .expect("dynasm should succeed");
        assert_eq!(from_dynasm, from_interpreter);
    }
}
