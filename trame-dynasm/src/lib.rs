use core::ffi::c_void;
use core::marker::PhantomData;
use core::mem::MaybeUninit;
use core::ptr::NonNull;
#[cfg(test)]
use core::sync::atomic::{AtomicBool, Ordering};
use dynasmrt::{AssemblyOffset, DynasmApi, DynasmLabelApi, dynasm};
use facet_core::Facet;
use std::alloc::{Layout, alloc, dealloc};
use trame_ir::{
    DecodeInstr, Error, ReadScalarOp, ScalarKind, StructFieldPlan, StructPlan, VecStructPlan,
};

#[cfg(test)]
static FORCE_DYNASM_COMPILE_FAIL: AtomicBool = AtomicBool::new(false);

pub struct DynasmPrepared<T>
where
    T: Facet<'static>,
{
    plan: StructPlan,
    trampoline: DynasmTrampoline,
    _marker: PhantomData<fn() -> T>,
}

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

pub struct DynasmPreparedVec<T>
where
    T: Facet<'static>,
{
    plan: VecStructPlan,
    trampoline: DynasmTrampoline,
    _marker: PhantomData<fn() -> T>,
}

impl<T> DynasmPreparedVec<T>
where
    T: Facet<'static>,
{
    pub fn decode(&self, input: &[u8]) -> Result<Vec<T>, Error> {
        decode_vec_with_dynasm_prepared(self, input)
    }
}

pub fn prepare<T>(plan: &StructPlan) -> Option<DynasmPrepared<T>>
where
    T: Facet<'static>,
{
    if plan.shape_id != T::SHAPE.id || plan.program.shape_id != T::SHAPE.id {
        return None;
    }
    let program = DynasmCompiledProgram::from_plan::<T>(plan)?;
    let trampoline = DynasmTrampoline::compile(&program)?;
    Some(DynasmPrepared {
        plan: plan.clone(),
        trampoline,
        _marker: PhantomData,
    })
}

pub fn prepare_vec<T>(plan: &VecStructPlan) -> Option<DynasmPreparedVec<T>>
where
    T: Facet<'static>,
{
    if plan.vec_shape_id != <Vec<T>>::SHAPE.id || plan.element_plan.program.shape_id != T::SHAPE.id
    {
        return None;
    }
    let element_program = DynasmCompiledProgram::from_plan::<T>(&plan.element_plan)?;
    let trampoline = DynasmTrampoline::compile_vec::<T>(&element_program)?;
    Some(DynasmPreparedVec {
        plan: plan.clone(),
        trampoline,
        _marker: PhantomData,
    })
}

pub fn decode<T>(plan: &StructPlan, input: &[u8]) -> Result<T, Error>
where
    T: Facet<'static>,
{
    decode_with_dynasm(plan, input)
}

pub fn decode_vec<T>(plan: &VecStructPlan, input: &[u8]) -> Result<Vec<T>, Error>
where
    T: Facet<'static>,
{
    decode_vec_with_dynasm(plan, input)
}
#[repr(C)]
struct JitCallContext<T> {
    input_ptr: *const u8,
    input_len: usize,
    pos: usize,
    field_plans_ptr: *const StructFieldPlan,
    field_plans_len: usize,
    field_inits: Vec<u8>,
    field_inits_ptr: *mut u8,
    failed: bool,
    error_tag: JitErrorTag,
    error_offset: usize,
    error_value: u32,
    vec_ptr: *mut T,
    vec_cap: usize,
    vec_len: usize,
    out: MaybeUninit<T>,
    out_vec: MaybeUninit<Vec<T>>,
}

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
    vec_ptr: u32,
    vec_len: u32,
    out: u32,
}

#[cfg(target_arch = "aarch64")]
#[derive(Clone, Copy)]
struct StringWordOffsets {
    ptr: u32,
    len: u32,
    cap: u32,
}

#[cfg(target_arch = "aarch64")]
#[derive(Clone, Copy)]
struct VecStringEmitConfig {
    o: JitContextOffsets,
    field_offset: usize,
    helper_alloc_bytes: usize,
    dangling_u8_ptr: u64,
    string_word_offsets: StringWordOffsets,
    err_elem_eof: dynasmrt::DynamicLabel,
    err_elem_invalid_utf8: dynasmrt::DynamicLabel,
    elem_fail_label: dynasmrt::DynamicLabel,
}

#[cfg(target_arch = "aarch64")]
fn jit_string_word_offsets() -> Option<StringWordOffsets> {
    if core::mem::size_of::<String>() != 3 * core::mem::size_of::<usize>() {
        return None;
    }
    let mut sample = String::with_capacity(17);
    sample.push('x');
    let ptr = sample.as_ptr() as usize;
    let len = sample.len();
    let cap = sample.capacity();
    let words = unsafe { *(&sample as *const String).cast::<[usize; 3]>() };

    let mut ptr_idx = None;
    let mut len_idx = None;
    let mut cap_idx = None;
    for (idx, word) in words.into_iter().enumerate() {
        if word == ptr {
            if ptr_idx.is_some() {
                return None;
            }
            ptr_idx = Some(idx);
        }
        if word == len {
            if len_idx.is_some() {
                return None;
            }
            len_idx = Some(idx);
        }
        if word == cap {
            if cap_idx.is_some() {
                return None;
            }
            cap_idx = Some(idx);
        }
    }
    let ptr_idx = ptr_idx?;
    let len_idx = len_idx?;
    let cap_idx = cap_idx?;
    if ptr_idx == len_idx || ptr_idx == cap_idx || len_idx == cap_idx {
        return None;
    }
    let word_size = core::mem::size_of::<usize>() as u32;
    Some(StringWordOffsets {
        ptr: ptr_idx as u32 * word_size,
        len: len_idx as u32 * word_size,
        cap: cap_idx as u32 * word_size,
    })
}

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
        vec_ptr: core::mem::offset_of!(JitCallContext<T>, vec_ptr) as u32,
        vec_len: core::mem::offset_of!(JitCallContext<T>, vec_len) as u32,
        out: core::mem::offset_of!(JitCallContext<T>, out) as u32,
    }
}

unsafe fn jit_ctx<'a, T>(ctx: *mut c_void) -> &'a mut JitCallContext<T>
where
    T: Facet<'static>,
{
    unsafe { &mut *ctx.cast::<JitCallContext<T>>() }
}

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
        Error::UnsupportedReadOp { .. } => {
            jit_set_failure(ctx, JitErrorTag::ShapeMismatch, 0, 0);
        }
    }
}

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

fn jit_read_string<T>(ctx: &mut JitCallContext<T>) -> Result<String, Error>
where
    T: Facet<'static>,
{
    let len = jit_read_u32(ctx)? as usize;
    jit_read_string_with_len(ctx, len)
}

fn jit_read_string_with_len<T>(ctx: &mut JitCallContext<T>, len: usize) -> Result<String, Error>
where
    T: Facet<'static>,
{
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

unsafe extern "C" fn jit_op_vec_alloc<T>(ctx: *mut c_void, arg1: u64, _arg2: u64) -> i32
where
    T: Facet<'static>,
{
    let ctx = unsafe { jit_ctx::<T>(ctx) };
    if ctx.failed {
        return 1;
    }
    let len = arg1 as usize;
    if core::mem::size_of::<T>() == 0 {
        ctx.vec_ptr = NonNull::<T>::dangling().as_ptr();
        ctx.vec_cap = len;
        ctx.vec_len = 0;
        return 0;
    }
    let Ok(layout) = Layout::array::<T>(len) else {
        jit_set_failure(ctx, JitErrorTag::ShapeMismatch, 0, 0);
        return 1;
    };
    if layout.size() == 0 {
        ctx.vec_ptr = NonNull::<T>::dangling().as_ptr();
        ctx.vec_cap = len;
        ctx.vec_len = 0;
        return 0;
    }
    let ptr = unsafe { alloc(layout) }.cast::<T>();
    if ptr.is_null() {
        jit_set_failure(ctx, JitErrorTag::ShapeMismatch, 0, 0);
        return 1;
    }
    ctx.vec_ptr = ptr;
    ctx.vec_cap = len;
    ctx.vec_len = 0;
    0
}

#[cfg(target_arch = "aarch64")]
unsafe extern "C" fn jit_alloc_bytes(arg1: u64, arg2: u64) -> *mut u8 {
    let size = arg1 as usize;
    let align = arg2 as usize;
    if size == 0 {
        return NonNull::<u8>::dangling().as_ptr();
    }
    let Ok(layout) = Layout::from_size_align(size, align) else {
        return core::ptr::null_mut();
    };
    unsafe { alloc(layout) }
}

unsafe extern "C" fn jit_op_drop_string_ptr(_: *mut c_void, arg1: u64, _arg2: u64) -> i32 {
    let dst = arg1 as usize as *mut String;
    unsafe {
        core::ptr::drop_in_place(dst);
    }
    0
}

unsafe extern "C" fn jit_op_vec_dealloc_raw<T>(ctx: *mut c_void, _arg1: u64, _arg2: u64) -> i32
where
    T: Facet<'static>,
{
    let ctx = unsafe { jit_ctx::<T>(ctx) };
    if !ctx.vec_ptr.is_null() {
        if core::mem::size_of::<T>() != 0 && ctx.vec_cap != 0 {
            if let Ok(layout) = Layout::array::<T>(ctx.vec_cap) {
                unsafe {
                    dealloc(ctx.vec_ptr.cast::<u8>(), layout);
                }
            }
        }
        ctx.vec_ptr = core::ptr::null_mut();
        ctx.vec_cap = 0;
        ctx.vec_len = 0;
    }
    0
}

unsafe extern "C" fn jit_op_vec_finalize<T>(ctx: *mut c_void, _arg1: u64, _arg2: u64) -> i32
where
    T: Facet<'static>,
{
    let ctx = unsafe { jit_ctx::<T>(ctx) };
    if ctx.failed {
        return 1;
    }
    if ctx.vec_ptr.is_null() {
        jit_set_failure(ctx, JitErrorTag::ShapeMismatch, 0, 0);
        return 1;
    }
    let vec = unsafe { Vec::from_raw_parts(ctx.vec_ptr, ctx.vec_len, ctx.vec_cap) };
    ctx.out_vec.write(vec);
    ctx.vec_ptr = core::ptr::null_mut();
    ctx.vec_cap = 0;
    ctx.vec_len = 0;
    0
}

#[derive(Clone, Copy)]
enum JitDynasmOp {
    ReadWriteU32 { field: u32, offset: usize },
    ReadWriteBool { field: u32, offset: usize },
    ReadWriteString { field: u32, offset: usize },
}

struct DynasmCompiledProgram {
    ops: Vec<JitDynasmOp>,
    require_eof: bool,
    string_helper: usize,
    context_offsets: JitContextOffsets,
}

impl DynasmCompiledProgram {
    fn from_plan<T>(plan: &StructPlan) -> Option<Self>
    where
        T: Facet<'static>,
    {
        let mut ops = Vec::with_capacity(plan.field_plans.len());
        let mut require_eof = false;
        let mut idx = 0usize;
        while let Some(instr) = plan.program.instructions.get(idx) {
            if let Some((op, next_idx)) = Self::match_lowered_len_prefixed_utf8(plan, idx) {
                ops.push(op);
                idx = next_idx;
                continue;
            }
            if let Some((op, next_idx)) = Self::match_lowered_varint_u32(plan, idx) {
                ops.push(op);
                idx = next_idx;
                continue;
            }
            if let Some((op, next_idx)) = Self::match_lowered_bool_byte01(plan, idx) {
                ops.push(op);
                idx = next_idx;
                continue;
            }
            match (*instr, plan.program.instructions.get(idx + 1).copied()) {
                (
                    DecodeInstr::ReadScalar { op, dst },
                    Some(DecodeInstr::WriteFieldFromReg { field, src }),
                ) if dst == src => {
                    let field_idx = field as usize;
                    let field_plan = plan.field_plans.get(field_idx)?;
                    if field_plan.kind != op.result_kind() {
                        return None;
                    }
                    let op = match op {
                        ReadScalarOp::VarintU32 => JitDynasmOp::ReadWriteU32 {
                            field,
                            offset: field_plan.offset,
                        },
                        ReadScalarOp::BoolByte01 => JitDynasmOp::ReadWriteBool {
                            field,
                            offset: field_plan.offset,
                        },
                        ReadScalarOp::LenPrefixedUtf8 => JitDynasmOp::ReadWriteString {
                            field,
                            offset: field_plan.offset,
                        },
                        _ => return None,
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

    fn parse_lowered_varint_seq(plan: &StructPlan, start: usize) -> Option<(u8, usize)> {
        let DecodeInstr::SetRegU32 { dst, value } = *plan.program.instructions.get(start)? else {
            return None;
        };
        if value != 0 {
            return None;
        }
        let mut idx = start + 1;
        let mut done_target = None::<usize>;

        for shift in [0u8, 7, 14, 21, 28] {
            let DecodeInstr::ReadInputByte { dst: byte_reg } =
                *plan.program.instructions.get(idx)?
            else {
                return None;
            };
            idx += 1;

            let DecodeInstr::AndImmU32 {
                dst: data_reg,
                src,
                imm,
            } = *plan.program.instructions.get(idx)?
            else {
                return None;
            };
            if src != byte_reg || imm != 0x7f {
                return None;
            }
            idx += 1;

            if shift != 0 {
                let DecodeInstr::ShlImmU32 {
                    dst: shl_dst,
                    src: shl_src,
                    shift: shl_shift,
                } = *plan.program.instructions.get(idx)?
                else {
                    return None;
                };
                if shl_dst != data_reg || shl_src != data_reg || shl_shift != shift {
                    return None;
                }
                idx += 1;
            }

            let DecodeInstr::OrU32 {
                dst: or_dst,
                lhs,
                rhs,
            } = *plan.program.instructions.get(idx)?
            else {
                return None;
            };
            if or_dst != dst || lhs != dst || rhs != data_reg {
                return None;
            }
            idx += 1;

            let DecodeInstr::JumpIfByteHighBitClear { src, target } =
                *plan.program.instructions.get(idx)?
            else {
                return None;
            };
            if src != byte_reg {
                return None;
            }
            match done_target {
                Some(existing) if existing != target => return None,
                None => done_target = Some(target),
                _ => {}
            }
            idx += 1;
        }

        let done_target = done_target?;
        if done_target != idx {
            return None;
        }

        Some((dst, idx))
    }

    fn match_lowered_varint_u32(plan: &StructPlan, start: usize) -> Option<(JitDynasmOp, usize)> {
        let (dst, idx) = Self::parse_lowered_varint_seq(plan, start)?;
        let DecodeInstr::WriteFieldFromReg { field, src } = *plan.program.instructions.get(idx)?
        else {
            return None;
        };
        if src != dst {
            return None;
        }
        let field_plan = plan.field_plans.get(field as usize)?;
        if field_plan.kind != ScalarKind::U32 {
            return None;
        }

        Some((
            JitDynasmOp::ReadWriteU32 {
                field,
                offset: field_plan.offset,
            },
            idx + 1,
        ))
    }

    fn match_lowered_len_prefixed_utf8(
        plan: &StructPlan,
        start: usize,
    ) -> Option<(JitDynasmOp, usize)> {
        let (len_reg, mut idx) = Self::parse_lowered_varint_seq(plan, start)?;
        let DecodeInstr::CaptureInputRangeByLenReg {
            dst: range_reg,
            len_reg: ir_len_reg,
        } = *plan.program.instructions.get(idx)?
        else {
            return None;
        };
        if ir_len_reg != len_reg {
            return None;
        }
        idx += 1;
        let DecodeInstr::ValidateUtf8Range {
            dst: validated_reg,
            src: validate_src,
        } = *plan.program.instructions.get(idx)?
        else {
            return None;
        };
        if validate_src != range_reg {
            return None;
        }
        idx += 1;
        let DecodeInstr::Utf8RangeToString { dst, src } = *plan.program.instructions.get(idx)?
        else {
            return None;
        };
        if src != validated_reg {
            return None;
        }
        idx += 1;
        let DecodeInstr::WriteFieldFromReg { field, src } = *plan.program.instructions.get(idx)?
        else {
            return None;
        };
        if src != dst {
            return None;
        }
        let field_plan = plan.field_plans.get(field as usize)?;
        if field_plan.kind != ScalarKind::String {
            return None;
        }
        Some((
            JitDynasmOp::ReadWriteString {
                field,
                offset: field_plan.offset,
            },
            idx + 1,
        ))
    }

    fn match_lowered_bool_byte01(plan: &StructPlan, start: usize) -> Option<(JitDynasmOp, usize)> {
        let DecodeInstr::ReadInputByte { dst: byte_reg } = *plan.program.instructions.get(start)?
        else {
            return None;
        };
        let DecodeInstr::AndImmU32 {
            dst: mask_reg,
            src: mask_src,
            imm,
        } = *plan.program.instructions.get(start + 1)?
        else {
            return None;
        };
        if mask_src != byte_reg || imm != 0xfe {
            return None;
        }
        let DecodeInstr::JumpIfRegZero {
            src: jump_src,
            target,
        } = *plan.program.instructions.get(start + 2)?
        else {
            return None;
        };
        if jump_src != mask_reg {
            return None;
        }
        let DecodeInstr::FailInvalidBool { value_reg } =
            *plan.program.instructions.get(start + 3)?
        else {
            return None;
        };
        if value_reg != byte_reg {
            return None;
        }
        let DecodeInstr::MoveRegU32 {
            dst: value_dst,
            src: value_src,
        } = *plan.program.instructions.get(start + 4)?
        else {
            return None;
        };
        if value_src != byte_reg {
            return None;
        }
        if target != start + 4 {
            return None;
        }
        let DecodeInstr::WriteFieldFromReg { field, src } =
            *plan.program.instructions.get(start + 5)?
        else {
            return None;
        };
        if src != value_dst {
            return None;
        }
        let field_plan = plan.field_plans.get(field as usize)?;
        if field_plan.kind != ScalarKind::Bool {
            return None;
        }
        Some((
            JitDynasmOp::ReadWriteBool {
                field,
                offset: field_plan.offset,
            },
            start + 6,
        ))
    }
}

struct DynasmTrampoline {
    _buffer: dynasmrt::ExecutableBuffer,
    entry: unsafe extern "C" fn(*mut c_void) -> i32,
}

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

    fn compile_vec<T>(element_program: &DynasmCompiledProgram) -> Option<Self>
    where
        T: Facet<'static>,
    {
        #[cfg(test)]
        if FORCE_DYNASM_COMPILE_FAIL.load(Ordering::Relaxed) {
            return None;
        }

        #[cfg(target_arch = "aarch64")]
        {
            Self::compile_vec_aarch64::<T>(element_program)
        }
        #[cfg(target_arch = "x86_64")]
        {
            let _ = element_program;
            None
        }
        #[cfg(not(any(target_arch = "aarch64", target_arch = "x86_64")))]
        {
            let _ = element_program;
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

    #[cfg(target_arch = "aarch64")]
    fn compile_vec_aarch64<T>(element_program: &DynasmCompiledProgram) -> Option<Self>
    where
        T: Facet<'static>,
    {
        let mut ops = dynasmrt::aarch64::Assembler::new().ok()?;
        let o = jit_context_offsets::<T>();
        let return_label = ops.new_dynamic_label();
        let loop_label = ops.new_dynamic_label();
        let done_label = ops.new_dynamic_label();
        let elem_fail_label = ops.new_dynamic_label();
        let err_elem_eof = ops.new_dynamic_label();
        let err_elem_overflow = ops.new_dynamic_label();
        let err_elem_invalid_bool = ops.new_dynamic_label();
        let err_elem_invalid_utf8 = ops.new_dynamic_label();
        let err_eof = ops.new_dynamic_label();
        let err_overflow = ops.new_dynamic_label();
        let entry = ops.offset();

        let drop_string_helper = jit_op_drop_string_ptr as *const () as usize;
        let helper_alloc = jit_op_vec_alloc::<T> as *const () as usize;
        let helper_dealloc = jit_op_vec_dealloc_raw::<T> as *const () as usize;
        let helper_finalize = jit_op_vec_finalize::<T> as *const () as usize;
        let helper_alloc_bytes = jit_alloc_bytes as *const () as usize;
        let string_word_offsets = jit_string_word_offsets()?;
        let string_ptr_off = string_word_offsets.ptr;
        let string_len_off = string_word_offsets.len;
        let string_cap_off = string_word_offsets.cap;
        let dangling_u8_ptr = NonNull::<u8>::dangling().as_ptr() as usize as u64;
        let stride = core::mem::size_of::<T>() as u64;

        let mut string_fields = Vec::new();
        for (idx, op) in element_program.ops.iter().enumerate() {
            if let JitDynasmOp::ReadWriteString { offset, .. } = *op {
                string_fields.push((idx as u64 + 1, offset as u64));
            }
        }

        dynasm!(ops
            ; .arch aarch64
            ; sub sp, sp, #64
            ; str x0, [sp]
            ; str x30, [sp, #8]
            ; str x19, [sp, #16]
            ; str x20, [sp, #24]
            ; str x21, [sp, #32]
            ; str x22, [sp, #40]
            ; str x23, [sp, #48]
            ; str x24, [sp, #56]
            ; ldr x8, [sp]
            ; ldr x5, [x8, #o.input_ptr]
            ; ldr x6, [x8, #o.input_len]
            ; ldr x7, [x8, #o.pos]
        );

        emit_aarch64_decode_u32(&mut ops, err_eof, err_overflow);
        dynasm!(ops
            ; .arch aarch64
            ; ldr x8, [sp]
            ; str x7, [x8, #o.pos]
            ; mov x19, x11 // target len
            ; ldr x0, [sp]
            ; mov x1, x19
            ; mov x2, #0
        );
        emit_aarch64_load_u64(&mut ops, 16, helper_alloc as u64);
        dynasm!(ops
            ; .arch aarch64
            ; blr x16
            ; cbnz w0, =>return_label
            ; ldr x8, [sp]
            ; ldr x5, [x8, #o.input_ptr]
            ; ldr x6, [x8, #o.input_len]
            ; ldr x7, [x8, #o.pos]
            ; mov x20, #0 // i
            ; mov x21, #0 // current element stage
            ; mov x22, #0 // current element ptr
            ; =>loop_label
            ; cmp x20, x19
            ; b.eq =>done_label
        );

        dynasm!(ops
            ; .arch aarch64
            ; ldr x8, [sp]
            ; ldr x22, [x8, #o.vec_ptr]
        );
        if stride != 0 {
            emit_aarch64_load_u64(&mut ops, 9, stride);
            dynasm!(ops
                ; .arch aarch64
                ; mul x10, x20, x9
                ; add x22, x22, x10
            );
        }
        dynasm!(ops
            ; .arch aarch64
            ; mov x21, #0
        );

        for op in &element_program.ops {
            match *op {
                JitDynasmOp::ReadWriteU32 { offset, .. } => {
                    emit_aarch64_decode_u32(&mut ops, err_elem_eof, err_elem_overflow);
                    dynasm!(ops
                        ; .arch aarch64
                        ; mov x9, x22
                    );
                    emit_aarch64_load_u64(&mut ops, 10, offset as u64);
                    dynasm!(ops
                        ; .arch aarch64
                        ; add x9, x9, x10
                        ; str w11, [x9]
                        ; add x21, x21, #1
                    );
                }
                JitDynasmOp::ReadWriteBool { offset, .. } => {
                    dynasm!(ops
                        ; .arch aarch64
                        ; mov x1, x7
                        ; cmp x7, x6
                        ; b.hs =>err_elem_eof
                        ; ldrb w11, [x5, x7]
                        ; add x7, x7, #1
                        ; cmp w11, #0
                        ; b.eq >bool_ok_vec
                        ; cmp w11, #1
                        ; b.eq >bool_ok_vec
                        ; mov w2, w11
                        ; b =>err_elem_invalid_bool
                        ; bool_ok_vec:
                        ; mov x9, x22
                    );
                    emit_aarch64_load_u64(&mut ops, 10, offset as u64);
                    dynasm!(ops
                        ; .arch aarch64
                        ; add x9, x9, x10
                        ; strb w11, [x9]
                        ; add x21, x21, #1
                    );
                }
                JitDynasmOp::ReadWriteString { offset, .. } => {
                    emit_aarch64_decode_u32(&mut ops, err_elem_eof, err_elem_overflow);
                    emit_aarch64_vec_write_string_from_len(
                        &mut ops,
                        VecStringEmitConfig {
                            o,
                            field_offset: offset,
                            helper_alloc_bytes,
                            dangling_u8_ptr,
                            string_word_offsets: StringWordOffsets {
                                ptr: string_ptr_off,
                                len: string_len_off,
                                cap: string_cap_off,
                            },
                            err_elem_eof,
                            err_elem_invalid_utf8,
                            elem_fail_label,
                        },
                    );
                }
            }
        }

        dynasm!(ops
            ; .arch aarch64
            ; add x10, x20, #1
            ; ldr x8, [sp]
            ; str x7, [x8, #o.pos]
            ; str x10, [x8, #o.vec_len]
            ; add x20, x20, #1
            ; b =>loop_label

            ; =>done_label
            ; ldr x8, [sp]
            ; ldr x7, [x8, #o.pos]
            ; ldr x6, [x8, #o.input_len]
            ; cmp x7, x6
            ; b.eq >vec_finalize
            ; mov x1, x7
            ; mov w2, wzr
        );
        emit_aarch64_write_failure(&mut ops, o, JitErrorTag::TrailingBytes);
        dynasm!(ops
            ; .arch aarch64
            ; mov x21, #0
            ; b =>elem_fail_label

            ; vec_finalize:
            ; ldr x0, [sp]
            ; mov x1, #0
            ; mov x2, #0
        );
        emit_aarch64_load_u64(&mut ops, 16, helper_finalize as u64);
        dynasm!(ops
            ; .arch aarch64
            ; blr x16
            ; b =>return_label

            ; =>elem_fail_label
        );

        for (stage_needed, offset) in &string_fields {
            let skip = ops.new_dynamic_label();
            emit_aarch64_load_u64(&mut ops, 9, *stage_needed);
            dynasm!(ops
                ; .arch aarch64
                ; cmp x21, x9
                ; b.lo =>skip
                ; ldr x0, [sp]
                ; mov x1, x22
            );
            emit_aarch64_load_u64(&mut ops, 10, *offset);
            dynasm!(ops
                ; .arch aarch64
                ; add x1, x1, x10
                ; mov x2, #0
            );
            emit_aarch64_load_u64(&mut ops, 16, drop_string_helper as u64);
            dynasm!(ops
                ; .arch aarch64
                ; blr x16
                ; =>skip
            );
        }

        let drop_loop = ops.new_dynamic_label();
        let drop_done = ops.new_dynamic_label();
        dynasm!(ops
            ; .arch aarch64
            ; mov x23, #0
            ; =>drop_loop
            ; cmp x23, x20
            ; b.eq =>drop_done
            ; ldr x8, [sp]
            ; ldr x24, [x8, #o.vec_ptr]
        );
        if stride != 0 {
            emit_aarch64_load_u64(&mut ops, 9, stride);
            dynasm!(ops
                ; .arch aarch64
                ; mul x10, x23, x9
                ; add x24, x24, x10
            );
        }
        for (_, offset) in &string_fields {
            dynasm!(ops
                ; .arch aarch64
                ; ldr x0, [sp]
                ; mov x1, x24
            );
            emit_aarch64_load_u64(&mut ops, 10, *offset);
            dynasm!(ops
                ; .arch aarch64
                ; add x1, x1, x10
                ; mov x2, #0
            );
            emit_aarch64_load_u64(&mut ops, 16, drop_string_helper as u64);
            dynasm!(ops
                ; .arch aarch64
                ; blr x16
            );
        }
        dynasm!(ops
            ; .arch aarch64
            ; add x23, x23, #1
            ; b =>drop_loop
            ; =>drop_done
            ; ldr x0, [sp]
            ; mov x1, #0
            ; mov x2, #0
        );
        emit_aarch64_load_u64(&mut ops, 16, helper_dealloc as u64);
        dynasm!(ops
            ; .arch aarch64
            ; blr x16
            ; mov w0, #1
            ; b =>return_label
        );

        dynasm!(ops
            ; .arch aarch64
            ; =>err_elem_eof
            ; mov w2, wzr
        );
        emit_aarch64_write_failure(&mut ops, o, JitErrorTag::UnexpectedEof);
        dynasm!(ops
            ; .arch aarch64
            ; b =>elem_fail_label
        );

        dynasm!(ops
            ; .arch aarch64
            ; =>err_elem_overflow
            ; mov w2, wzr
        );
        emit_aarch64_write_failure(&mut ops, o, JitErrorTag::VarintOverflow);
        dynasm!(ops
            ; .arch aarch64
            ; b =>elem_fail_label
        );

        dynasm!(ops
            ; .arch aarch64
            ; =>err_elem_invalid_bool
        );
        emit_aarch64_write_failure(&mut ops, o, JitErrorTag::InvalidBool);
        dynasm!(ops
            ; .arch aarch64
            ; b =>elem_fail_label
        );

        dynasm!(ops
            ; .arch aarch64
            ; =>err_elem_invalid_utf8
        );
        emit_aarch64_write_failure(&mut ops, o, JitErrorTag::InvalidUtf8);
        dynasm!(ops
            ; .arch aarch64
            ; b =>elem_fail_label
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
            ; =>return_label
            ; ldr x24, [sp, #56]
            ; ldr x23, [sp, #48]
            ; ldr x22, [sp, #40]
            ; ldr x21, [sp, #32]
            ; ldr x20, [sp, #24]
            ; ldr x19, [sp, #16]
            ; ldr x30, [sp, #8]
            ; add sp, sp, #64
            ; ret
        );

        let buffer = ops.finalize().ok()?;
        Some(Self::from_buffer(buffer, entry))
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

#[cfg(target_arch = "aarch64")]
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

#[cfg(target_arch = "aarch64")]
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

#[cfg(target_arch = "aarch64")]
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

#[cfg(target_arch = "aarch64")]
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

#[cfg(target_arch = "aarch64")]
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

#[cfg(target_arch = "aarch64")]
fn emit_aarch64_vec_write_string_from_len(
    ops: &mut dynasmrt::aarch64::Assembler,
    cfg: VecStringEmitConfig,
) {
    let o = cfg.o;
    let field_offset = cfg.field_offset;
    let helper_alloc_bytes = cfg.helper_alloc_bytes;
    let dangling_u8_ptr = cfg.dangling_u8_ptr;
    let string_ptr_off = cfg.string_word_offsets.ptr;
    let string_len_off = cfg.string_word_offsets.len;
    let string_cap_off = cfg.string_word_offsets.cap;
    let err_elem_eof = cfg.err_elem_eof;
    let err_elem_invalid_utf8 = cfg.err_elem_invalid_utf8;
    let elem_fail_label = cfg.elem_fail_label;

    let string_zero = ops.new_dynamic_label();
    let string_store = ops.new_dynamic_label();
    let copy_qword_loop = ops.new_dynamic_label();
    let copy_tail_loop = ops.new_dynamic_label();
    let copy_done = ops.new_dynamic_label();
    let utf8_loop = ops.new_dynamic_label();
    let utf8_valid = ops.new_dynamic_label();
    let utf8_scalar_start = ops.new_dynamic_label();
    let utf8_ascii8_loop = ops.new_dynamic_label();
    let utf8_ascii_tail_loop = ops.new_dynamic_label();
    let utf8_ascii = ops.new_dynamic_label();
    let utf8_two = ops.new_dynamic_label();
    let utf8_three = ops.new_dynamic_label();
    let utf8_four = ops.new_dynamic_label();
    let utf8_three_not_e0 = ops.new_dynamic_label();
    let utf8_three_after_ed = ops.new_dynamic_label();
    let utf8_four_not_f0 = ops.new_dynamic_label();
    let utf8_four_after_f4 = ops.new_dynamic_label();
    let utf8_invalid = ops.new_dynamic_label();

    emit_aarch64_load_u64(ops, 17, 0x8080_8080_8080_8080);
    dynasm!(ops
        ; .arch aarch64
        ; mov x13, x7
        ; adds x12, x7, x11
        ; b.cs =>err_elem_eof
        ; cmp x12, x6
        ; b.hi =>err_elem_eof
        ; mov x14, x12
        ; mov x23, x11
        ; add x24, x5, x7
        ; cbz x23, =>utf8_valid
        ; mov x15, #0
        ; cmp x23, #8
        ; b.lo =>utf8_scalar_start
        ; =>utf8_ascii8_loop
        ; add x10, x15, #8
        ; cmp x10, x23
        ; b.hi =>utf8_ascii_tail_loop
        ; ldr x9, [x24, x15]
        ; and x9, x9, x17
        ; cbnz x9, =>utf8_scalar_start
        ; mov x15, x10
        ; b =>utf8_ascii8_loop
        ; =>utf8_ascii_tail_loop
        ; cmp x15, x23
        ; b.eq =>utf8_valid
        ; ldrb w9, [x24, x15]
        ; tbnz w9, #7, =>utf8_scalar_start
        ; add x15, x15, #1
        ; b =>utf8_ascii_tail_loop
        ; =>utf8_scalar_start
        ; mov x15, #0
        ; =>utf8_loop
        ; cmp x15, x23
        ; b.eq =>utf8_valid
        ; ldrb w9, [x24, x15]
        ; cmp w9, #0x7f
        ; b.ls =>utf8_ascii
        ; and w10, w9, #0xE0
        ; cmp w10, #0xC0
        ; b.eq =>utf8_two
        ; and w10, w9, #0xF0
        ; cmp w10, #0xE0
        ; b.eq =>utf8_three
        ; and w10, w9, #0xF8
        ; cmp w10, #0xF0
        ; b.eq =>utf8_four
        ; b =>utf8_invalid

        ; =>utf8_ascii
        ; add x15, x15, #1
        ; b =>utf8_loop

        ; =>utf8_two
        ; add x10, x15, #1
        ; cmp x10, x23
        ; b.hs =>utf8_invalid
        ; ldrb w11, [x24, x10]
        ; and w12, w11, #0xC0
        ; cmp w12, #0x80
        ; b.ne =>utf8_invalid
        ; cmp w9, #0xC2
        ; b.lo =>utf8_invalid
        ; add x15, x15, #2
        ; b =>utf8_loop

        ; =>utf8_three
        ; add x10, x15, #2
        ; cmp x10, x23
        ; b.hs =>utf8_invalid
        ; add x10, x15, #1
        ; ldrb w11, [x24, x10]
        ; add x12, x15, #2
        ; ldrb w17, [x24, x12]
        ; and w10, w11, #0xC0
        ; cmp w10, #0x80
        ; b.ne =>utf8_invalid
        ; and w10, w17, #0xC0
        ; cmp w10, #0x80
        ; b.ne =>utf8_invalid
        ; cmp w9, #0xE0
        ; b.ne =>utf8_three_not_e0
        ; cmp w11, #0xA0
        ; b.lo =>utf8_invalid
        ; =>utf8_three_not_e0
        ; cmp w9, #0xED
        ; b.ne =>utf8_three_after_ed
        ; cmp w11, #0xA0
        ; b.hs =>utf8_invalid
        ; =>utf8_three_after_ed
        ; add x15, x15, #3
        ; b =>utf8_loop

        ; =>utf8_four
        ; add x10, x15, #3
        ; cmp x10, x23
        ; b.hs =>utf8_invalid
        ; cmp w9, #0xF4
        ; b.hi =>utf8_invalid
        ; add x10, x15, #1
        ; ldrb w11, [x24, x10]
        ; add x12, x15, #2
        ; ldrb w17, [x24, x12]
        ; add x10, x15, #3
        ; ldrb w12, [x24, x10]
        ; and w10, w11, #0xC0
        ; cmp w10, #0x80
        ; b.ne =>utf8_invalid
        ; and w10, w17, #0xC0
        ; cmp w10, #0x80
        ; b.ne =>utf8_invalid
        ; and w10, w12, #0xC0
        ; cmp w10, #0x80
        ; b.ne =>utf8_invalid
        ; cmp w9, #0xF0
        ; b.ne =>utf8_four_not_f0
        ; cmp w11, #0x90
        ; b.lo =>utf8_invalid
        ; =>utf8_four_not_f0
        ; cmp w9, #0xF4
        ; b.ne =>utf8_four_after_f4
        ; cmp w11, #0x90
        ; b.hs =>utf8_invalid
        ; =>utf8_four_after_f4
        ; add x15, x15, #4
        ; b =>utf8_loop

        ; =>utf8_invalid
        ; mov x1, x13
        ; mov w2, wzr
        ; b =>err_elem_invalid_utf8

        ; =>utf8_valid
        ; mov x7, x14
        ; ldr x8, [sp]
        ; str x14, [x8, #o.pos]
        ; cbz x23, =>string_zero
        ; mov x0, x23
        ; mov x1, #1
    );
    emit_aarch64_load_u64(ops, 16, helper_alloc_bytes as u64);
    dynasm!(ops
        ; .arch aarch64
        ; blr x16
        ; mov x14, x0
        ; cbnz x14, >string_alloc_ok
        ; mov x1, #0
        ; mov w2, wzr
    );
    emit_aarch64_write_failure(ops, o, JitErrorTag::ShapeMismatch);
    dynasm!(ops
        ; .arch aarch64
        ; b =>elem_fail_label
        ; string_alloc_ok:
        ; mov x9, #0
        ; cmp x23, #8
        ; b.lo =>copy_tail_loop
        ; =>copy_qword_loop
        ; add x10, x9, #8
        ; cmp x10, x23
        ; b.hi =>copy_tail_loop
        ; ldr x11, [x24, x9]
        ; str x11, [x14, x9]
        ; mov x9, x10
        ; b =>copy_qword_loop
        ; =>copy_tail_loop
        ; cmp x9, x23
        ; b.eq =>copy_done
        ; ldrb w10, [x24, x9]
        ; strb w10, [x14, x9]
        ; add x9, x9, #1
        ; b =>copy_tail_loop
        ; =>copy_done
        ; b =>string_store
        ; =>string_zero
    );
    emit_aarch64_load_u64(ops, 14, dangling_u8_ptr);
    dynasm!(ops
        ; .arch aarch64
        ; =>string_store
        ; mov x9, x22
    );
    emit_aarch64_load_u64(ops, 10, field_offset as u64);
    dynasm!(ops
        ; .arch aarch64
        ; add x9, x9, x10
        ; str x14, [x9, #string_ptr_off]
        ; str x23, [x9, #string_len_off]
        ; str x23, [x9, #string_cap_off]
        ; ldr x8, [sp]
        ; ldr x5, [x8, #o.input_ptr]
        ; ldr x6, [x8, #o.input_len]
        ; ldr x7, [x8, #o.pos]
        ; add x21, x21, #1
    );
}

#[cfg(target_arch = "aarch64")]
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

#[cfg(target_arch = "aarch64")]
fn encode_aarch64_movz(rd: u8, imm16: u16, hw: u8) -> u32 {
    0xD2800000 | ((hw as u32) << 21) | ((imm16 as u32) << 5) | (rd as u32)
}

#[cfg(target_arch = "aarch64")]
fn encode_aarch64_movk(rd: u8, imm16: u16, hw: u8) -> u32 {
    0xF2800000 | ((hw as u32) << 21) | ((imm16 as u32) << 5) | (rd as u32)
}

fn decode_with_dynasm<T>(plan: &StructPlan, input: &[u8]) -> Result<T, Error>
where
    T: Facet<'static>,
{
    let Some(prepared) = prepare::<T>(plan) else {
        return trame_interpreter::decode(plan, input);
    };
    prepared.decode(input)
}

fn decode_vec_with_dynasm<T>(plan: &VecStructPlan, input: &[u8]) -> Result<Vec<T>, Error>
where
    T: Facet<'static>,
{
    let Some(prepared) = prepare_vec::<T>(plan) else {
        return trame_interpreter::decode_vec(plan, input);
    };
    decode_vec_with_dynasm_prepared(&prepared, input)
}

fn decode_vec_with_dynasm_prepared<T>(
    prepared: &DynasmPreparedVec<T>,
    input: &[u8],
) -> Result<Vec<T>, Error>
where
    T: Facet<'static>,
{
    let plan = &prepared.plan.element_plan;
    let mut ctx = JitCallContext::<T> {
        input_ptr: input.as_ptr(),
        input_len: input.len(),
        pos: 0,
        field_plans_ptr: plan.field_plans.as_ptr(),
        field_plans_len: plan.field_plans.len(),
        field_inits: vec![0u8; plan.field_plans.len()],
        field_inits_ptr: core::ptr::null_mut(),
        failed: false,
        error_tag: JitErrorTag::None,
        error_offset: 0,
        error_value: 0,
        vec_ptr: core::ptr::null_mut(),
        vec_cap: 0,
        vec_len: 0,
        out: MaybeUninit::uninit(),
        out_vec: MaybeUninit::uninit(),
    };
    ctx.field_inits_ptr = ctx.field_inits.as_mut_ptr();

    let status = unsafe {
        prepared
            .trampoline
            .call((&mut ctx as *mut JitCallContext<T>).cast())
    };
    match status {
        0 => Ok(unsafe { ctx.out_vec.assume_init() }),
        1 => Err(jit_error_from_context(&ctx)),
        _ => trame_interpreter::decode_vec(&prepared.plan, input),
    }
}

fn decode_with_trampoline_at<T>(
    plan: &StructPlan,
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
        field_plans_ptr: plan.field_plans.as_ptr(),
        field_plans_len: plan.field_plans.len(),
        field_inits: vec![0u8; plan.field_plans.len()],
        field_inits_ptr: core::ptr::null_mut(),
        failed: false,
        error_tag: JitErrorTag::None,
        error_offset: 0,
        error_value: 0,
        vec_ptr: core::ptr::null_mut(),
        vec_cap: 0,
        vec_len: 0,
        out: MaybeUninit::uninit(),
        out_vec: MaybeUninit::uninit(),
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
        _ => Err(Error::ShapeMismatch),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, PartialEq, facet::Facet)]
    struct Demo {
        id: u32,
        name: String,
    }

    #[derive(Debug, PartialEq, facet::Facet)]
    struct Demo3 {
        id: u32,
        name: String,
        ok: bool,
    }

    struct DynasmCompileFailGuard;

    impl DynasmCompileFailGuard {
        fn activate() -> Self {
            FORCE_DYNASM_COMPILE_FAIL.store(true, Ordering::SeqCst);
            Self
        }
    }

    impl Drop for DynasmCompileFailGuard {
        fn drop(&mut self) {
            FORCE_DYNASM_COMPILE_FAIL.store(false, Ordering::SeqCst);
        }
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

    #[test]
    fn dynasm_and_interpreter_backends_are_equivalent() {
        let plan = trame_postcard::compile_for::<Demo3>().expect("compile should succeed");
        let wire = facet_postcard::to_vec(&Demo3 {
            id: 11,
            name: "eve".into(),
            ok: true,
        })
        .expect("encode");

        let from_interpreter: Demo3 = trame_interpreter::decode(&plan, &wire).expect("ok");
        let from_dynasm: Demo3 = decode(&plan, &wire).expect("ok");
        assert_eq!(from_dynasm, from_interpreter);
    }

    #[test]
    fn dynasm_compile_failure_falls_back_to_interpreter() {
        let _guard = DynasmCompileFailGuard::activate();
        let plan = trame_postcard::compile_for::<Demo>().expect("compile should succeed");
        let wire = encode_demo_wire(13, "mallory");
        let value: Demo = decode(&plan, &wire).expect("fallback decode should succeed");
        assert_eq!(
            value,
            Demo {
                id: 13,
                name: "mallory".into()
            }
        );
    }

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
        let plan = trame_postcard::compile_vec_for::<Vec<Demo3>>().expect("compile should succeed");
        let from_interpreter = trame_interpreter::decode_vec::<Demo3>(&plan, &wire)
            .expect("interpreter should succeed");
        let from_dynasm = decode_vec::<Demo3>(&plan, &wire).expect("dynasm should succeed");
        assert_eq!(from_dynasm, from_interpreter);
    }

    #[test]
    fn decode_vec_dynasm_handles_non_ascii_strings() {
        let expected = vec![
            Demo3 {
                id: 10,
                name: "héllo".into(),
                ok: true,
            },
            Demo3 {
                id: 11,
                name: "🙂🚀".into(),
                ok: false,
            },
        ];
        let wire = facet_postcard::to_vec(&expected).expect("encode should succeed");
        let plan = trame_postcard::compile_vec_for::<Vec<Demo3>>().expect("compile should succeed");
        let from_dynasm = decode_vec::<Demo3>(&plan, &wire).expect("dynasm should succeed");
        assert_eq!(from_dynasm, expected);
    }

    #[test]
    fn decode_vec_dynasm_rejects_invalid_utf8() {
        let mut wire = encode_u32(1);
        wire.extend(encode_u32(7));
        wire.extend(encode_u32(1));
        wire.push(0xFF);
        wire.push(1);
        let plan = trame_postcard::compile_vec_for::<Vec<Demo3>>().expect("compile should succeed");
        let err = decode_vec::<Demo3>(&plan, &wire).expect_err("dynasm should fail");
        assert_eq!(err, Error::InvalidUtf8 { offset: 3 });
    }
}
