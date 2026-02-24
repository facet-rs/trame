use facet_core::{Facet, Shape, Type, UserType};
use trame::{LRuntime, Path, Trame, TrameError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
    RootNotStruct {
        type_name: &'static str,
    },
    ExpectedTwoFields {
        type_name: &'static str,
        found: usize,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    ShapeMismatch,
    UnexpectedEof { offset: usize },
    VarintOverflow { offset: usize },
    InvalidUtf8 { offset: usize },
    TrailingBytes { offset: usize },
    Trame(TrameError),
}

impl From<TrameError> for Error {
    fn from(value: TrameError) -> Self {
        Self::Trame(value)
    }
}

/// Compile-time validated decode plan for the first postcard vertical slice:
/// one struct with exactly two fields (`u32`, `String`) in declaration order.
#[derive(Clone, Copy, Debug)]
pub struct PostcardStructU32StringPlan {
    shape: &'static Shape,
}

impl PostcardStructU32StringPlan {
    pub fn compile(shape: &'static Shape) -> Result<Self, CompileError> {
        let Type::User(UserType::Struct(st)) = shape.ty else {
            return Err(CompileError::RootNotStruct {
                type_name: shape.type_identifier,
            });
        };

        if st.fields.len() != 2 {
            return Err(CompileError::ExpectedTwoFields {
                type_name: shape.type_identifier,
                found: st.fields.len(),
            });
        }

        Ok(Self { shape })
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

        let first = reader.read_u32()?;
        let (str_offset, str_bytes) = reader.read_string_bytes()?;
        let second = core::str::from_utf8(str_bytes)
            .map_err(|_| Error::InvalidUtf8 { offset: str_offset })?
            .to_owned();

        if !reader.is_eof() {
            return Err(Error::TrailingBytes {
                offset: reader.offset(),
            });
        }

        let mut trame = Trame::<LRuntime>::alloc::<T>()?;
        let first_text = first.to_string();
        trame.parse_from_str(Path::field(0), &first_text)?;
        trame.parse_from_str(Path::field(1), &second)?;
        let hv = trame.build()?;
        hv.materialize::<T>().map_err(Error::Trame)
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
    let plan = compile::<T>().map_err(|_| Error::ShapeMismatch)?;
    plan.decode(input)
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
            CompileError::ExpectedTwoFields {
                type_name: "WrongShape",
                found: 1
            }
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
