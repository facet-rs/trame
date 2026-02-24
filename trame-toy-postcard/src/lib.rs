use facet_core::{Facet, Shape, Type, UserType};
use trame::{LRuntime, Path, Trame, TrameError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
    RootNotStruct {
        type_name: &'static str,
    },
    UnsupportedFieldCount {
        type_name: &'static str,
        found: usize,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DecodeInstr {
    ReadScalar { kind: ScalarKind, dst: u8 },
    WriteFieldFromReg { field: u32, src: u8 },
    RequireEof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PostcardDecodeProgram {
    pub instructions: Vec<DecodeInstr>,
}

/// Compile-time validated decode plan for the first postcard vertical slice:
/// one struct with exactly two fields (`u32`, `String`) in declaration order.
#[derive(Clone, Debug)]
pub struct PostcardStructU32StringPlan {
    shape: &'static Shape,
    program: PostcardDecodeProgram,
}

impl PostcardStructU32StringPlan {
    pub fn compile(shape: &'static Shape) -> Result<Self, CompileError> {
        let Type::User(UserType::Struct(st)) = shape.ty else {
            return Err(CompileError::RootNotStruct {
                type_name: shape.type_identifier,
            });
        };

        if st.fields.len() != 2 {
            return Err(CompileError::UnsupportedFieldCount {
                type_name: shape.type_identifier,
                found: st.fields.len(),
            });
        }

        let mut instructions = Vec::with_capacity(st.fields.len() * 2 + 1);
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
        }
        instructions.push(DecodeInstr::RequireEof);
        let program = PostcardDecodeProgram { instructions };

        Ok(Self { shape, program })
    }

    pub fn compile_for<T>() -> Result<Self, CompileError>
    where
        T: Facet<'static>,
    {
        Self::compile(T::SHAPE)
    }

    pub fn decode<T>(&self, input: &[u8]) -> Result<T, Error>
    where
        T: Facet<'static>,
    {
        if self.shape.id != T::SHAPE.id {
            return Err(Error::ShapeMismatch);
        }

        let mut reader = Reader::new(input);
        let mut regs = [RegValue::Unset, RegValue::Unset];
        let mut trame = Trame::<LRuntime>::alloc::<T>()?;
        for instr in &self.program.instructions {
            match *instr {
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
                    };
                }
                DecodeInstr::WriteFieldFromReg { field, src } => {
                    let reg = regs
                        .get(src as usize)
                        .ok_or(Error::InvalidRegister { reg: src as usize })?;
                    match reg {
                        RegValue::Unset => {
                            return Err(Error::RegisterUnset { reg: src as usize });
                        }
                        RegValue::U32(value) => {
                            let value_text = value.to_string();
                            trame.parse_from_str(Path::field(field), &value_text)?;
                        }
                        RegValue::String(value) => {
                            trame.parse_from_str(Path::field(field), value)?;
                        }
                    }
                }
                DecodeInstr::RequireEof => {
                    if !reader.is_eof() {
                        return Err(Error::TrailingBytes {
                            offset: reader.offset(),
                        });
                    }
                }
            }
        }

        let hv = trame.build()?;
        hv.materialize::<T>().map_err(Error::Trame)
    }

    pub fn program(&self) -> &PostcardDecodeProgram {
        &self.program
    }
}

pub fn compile<T>() -> Result<PostcardStructU32StringPlan, CompileError>
where
    T: Facet<'static>,
{
    PostcardStructU32StringPlan::compile_for::<T>()
}

pub fn from_slice<T>(input: &[u8]) -> Result<T, Error>
where
    T: Facet<'static>,
{
    let plan = compile::<T>()?;
    plan.decode(input)
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum RegValue {
    Unset,
    U32(u32),
    String(String),
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
}

fn scalar_kind_for_shape(shape: &'static Shape) -> Option<ScalarKind> {
    if shape.type_identifier == "u32" {
        return Some(ScalarKind::U32);
    }
    if shape.type_identifier == "String" {
        return Some(ScalarKind::String);
    }
    None
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
    struct Other {
        a: u32,
        b: String,
    }

    #[derive(Debug, PartialEq, facet::Facet)]
    struct WrongShape {
        only: u32,
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
    fn compile_rejects_wrong_shape() {
        let err = compile::<WrongShape>().expect_err("shape should be rejected");
        assert_eq!(
            err,
            CompileError::UnsupportedFieldCount {
                type_name: "WrongShape",
                found: 1
            }
        );
    }

    #[test]
    fn compile_emits_program() {
        let plan = compile::<Demo>().expect("compile should succeed");
        let program = plan.program();
        assert_eq!(
            program.instructions,
            vec![
                DecodeInstr::ReadScalar {
                    kind: ScalarKind::U32,
                    dst: 0,
                },
                DecodeInstr::WriteFieldFromReg { field: 0, src: 0 },
                DecodeInstr::ReadScalar {
                    kind: ScalarKind::String,
                    dst: 1,
                },
                DecodeInstr::WriteFieldFromReg { field: 1, src: 1 },
                DecodeInstr::RequireEof,
            ]
        );
    }

    #[test]
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
}
