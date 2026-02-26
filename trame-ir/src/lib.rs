use facet_core::{ConstTypeId, Facet};

pub const DECODE_ABI_V1: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    ShapeMismatch,
    UnexpectedEof { offset: usize },
    VarintOverflow { offset: usize },
    InvalidUtf8 { offset: usize },
    InvalidBool { offset: usize, value: u8 },
    TrailingBytes { offset: usize },
    InvalidRegister { reg: usize },
    RegisterUnset { reg: usize },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarKind {
    U32,
    String,
    Bool,
}

impl ScalarKind {
    pub fn as_symbol(self) -> &'static str {
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
pub struct DecodeProgram {
    pub abi_version: u32,
    pub shape_id: ConstTypeId,
    pub register_count: usize,
    pub instructions: Vec<DecodeInstr>,
}

impl DecodeProgram {
    pub fn to_sexp(&self) -> String {
        use core::fmt::Write as _;

        let mut out = String::new();
        let _ = writeln!(&mut out, "(decode-program");
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StructFieldPlan {
    pub kind: ScalarKind,
    pub offset: usize,
}

#[derive(Clone, Debug)]
pub struct StructPlan {
    pub shape_id: ConstTypeId,
    pub program: DecodeProgram,
    pub field_plans: Vec<StructFieldPlan>,
}

impl StructPlan {
    pub fn ensure_shape<T>(&self) -> Result<(), Error>
    where
        T: Facet<'static>,
    {
        if self.shape_id != T::SHAPE.id || self.program.shape_id != T::SHAPE.id {
            return Err(Error::ShapeMismatch);
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct VecStructPlan {
    pub vec_shape_id: ConstTypeId,
    pub element_plan: StructPlan,
}

impl VecStructPlan {
    pub fn ensure_vec_shape<T>(&self) -> Result<(), Error>
    where
        T: Facet<'static>,
    {
        if self.vec_shape_id != <Vec<T>>::SHAPE.id
            || self.element_plan.program.shape_id != T::SHAPE.id
        {
            return Err(Error::ShapeMismatch);
        }
        Ok(())
    }
}
