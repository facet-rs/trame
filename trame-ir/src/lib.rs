use facet_core::ConstTypeId;

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
    UnsupportedReadOp { op: ReadScalarOp },
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
pub enum ReadScalarOp {
    VarintU32,
    LenPrefixedUtf8,
    BoolByte01,
    DecimalU32,
    QuotedUtf8String,
    BoolLiteral,
}

impl ReadScalarOp {
    pub fn as_symbol(self) -> &'static str {
        match self {
            ReadScalarOp::VarintU32 => "varint-u32",
            ReadScalarOp::LenPrefixedUtf8 => "len-prefixed-utf8",
            ReadScalarOp::BoolByte01 => "bool-byte-01",
            ReadScalarOp::DecimalU32 => "decimal-u32",
            ReadScalarOp::QuotedUtf8String => "quoted-utf8-string",
            ReadScalarOp::BoolLiteral => "bool-literal",
        }
    }

    pub fn result_kind(self) -> ScalarKind {
        match self {
            ReadScalarOp::VarintU32 | ReadScalarOp::DecimalU32 => ScalarKind::U32,
            ReadScalarOp::LenPrefixedUtf8 | ReadScalarOp::QuotedUtf8String => ScalarKind::String,
            ReadScalarOp::BoolByte01 | ReadScalarOp::BoolLiteral => ScalarKind::Bool,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DecodeInstr {
    SetRegU32 { dst: u8, value: u32 },
    MoveRegU32 { dst: u8, src: u8 },
    ReadInputByte { dst: u8 },
    CaptureInputRangeByLenReg { dst: u8, len_reg: u8 },
    Utf8RangeToString { dst: u8, src: u8 },
    AndImmU32 { dst: u8, src: u8, imm: u32 },
    ShlImmU32 { dst: u8, src: u8, shift: u8 },
    OrU32 { dst: u8, lhs: u8, rhs: u8 },
    JumpIfRegZero { src: u8, target: usize },
    JumpIfByteHighBitClear { src: u8, target: usize },
    FailInvalidBool { value_reg: u8 },
    ReadScalar { op: ReadScalarOp, dst: u8 },
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
                DecodeInstr::SetRegU32 { dst, value } => {
                    let _ = writeln!(
                        &mut out,
                        "    (set-reg-u32 (dst r{}) (value {}))",
                        dst, value
                    );
                }
                DecodeInstr::MoveRegU32 { dst, src } => {
                    let _ = writeln!(&mut out, "    (move-reg-u32 (dst r{}) (src r{}))", dst, src);
                }
                DecodeInstr::ReadInputByte { dst } => {
                    let _ = writeln!(&mut out, "    (read-input-byte (dst r{}))", dst);
                }
                DecodeInstr::CaptureInputRangeByLenReg { dst, len_reg } => {
                    let _ = writeln!(
                        &mut out,
                        "    (capture-input-range-by-len-reg (dst r{}) (len-reg r{}))",
                        dst, len_reg
                    );
                }
                DecodeInstr::Utf8RangeToString { dst, src } => {
                    let _ = writeln!(
                        &mut out,
                        "    (utf8-range-to-string (dst r{}) (src r{}))",
                        dst, src
                    );
                }
                DecodeInstr::AndImmU32 { dst, src, imm } => {
                    let _ = writeln!(
                        &mut out,
                        "    (and-imm-u32 (dst r{}) (src r{}) (imm {}))",
                        dst, src, imm
                    );
                }
                DecodeInstr::ShlImmU32 { dst, src, shift } => {
                    let _ = writeln!(
                        &mut out,
                        "    (shl-imm-u32 (dst r{}) (src r{}) (shift {}))",
                        dst, src, shift
                    );
                }
                DecodeInstr::OrU32 { dst, lhs, rhs } => {
                    let _ = writeln!(
                        &mut out,
                        "    (or-u32 (dst r{}) (lhs r{}) (rhs r{}))",
                        dst, lhs, rhs
                    );
                }
                DecodeInstr::JumpIfRegZero { src, target } => {
                    let _ = writeln!(
                        &mut out,
                        "    (jump-if-reg-zero (src r{}) (target {}))",
                        src, target
                    );
                }
                DecodeInstr::JumpIfByteHighBitClear { src, target } => {
                    let _ = writeln!(
                        &mut out,
                        "    (jump-if-byte-high-bit-clear (src r{}) (target {}))",
                        src, target
                    );
                }
                DecodeInstr::FailInvalidBool { value_reg } => {
                    let _ = writeln!(
                        &mut out,
                        "    (fail-invalid-bool (value-reg r{}))",
                        value_reg
                    );
                }
                DecodeInstr::ReadScalar { op, dst } => {
                    let _ = writeln!(
                        &mut out,
                        "    (read-scalar (op {}) (dst r{}))",
                        op.as_symbol(),
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

#[derive(Clone, Debug)]
pub struct VecStructPlan {
    pub vec_shape_id: ConstTypeId,
    pub element_plan: StructPlan,
}
