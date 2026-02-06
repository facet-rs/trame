use std::alloc::Layout;

use facet_core::{Facet, PtrMut, PtrUninit, Shape, Type, UserType};
use trame::{LRuntime, Op, Path, Source, Trame, TrameError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    ExpectedValue {
        offset: usize,
    },
    UnterminatedString {
        offset: usize,
    },
    InvalidEscape {
        offset: usize,
    },
    InvalidNumber {
        offset: usize,
    },
    InvalidLiteral {
        offset: usize,
    },
    TrailingCharacters {
        offset: usize,
    },
    TypeMismatch {
        expected: &'static str,
        offset: usize,
    },
    MissingField {
        field: &'static str,
        offset: usize,
    },
    UnknownField {
        field: String,
        offset: usize,
    },
    UnsupportedShape {
        type_name: &'static str,
        offset: usize,
    },
    ScalarParse {
        type_name: &'static str,
        offset: usize,
    },
    Trame(TrameError),
}

impl From<TrameError> for Error {
    fn from(value: TrameError) -> Self {
        Self::Trame(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum JsonValue {
    Object(Vec<(String, JsonValue)>),
    String(String),
    Number(String),
    Bool(bool),
    Null,
}

struct Parser<'a> {
    input: &'a str,
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            bytes: input.as_bytes(),
            pos: 0,
        }
    }

    fn parse(mut self) -> Result<JsonValue, Error> {
        self.skip_ws();
        let value = self.parse_value()?;
        self.skip_ws();
        if self.pos != self.bytes.len() {
            return Err(Error::TrailingCharacters { offset: self.pos });
        }
        Ok(value)
    }

    fn parse_value(&mut self) -> Result<JsonValue, Error> {
        self.skip_ws();
        let Some(&b) = self.bytes.get(self.pos) else {
            return Err(Error::ExpectedValue { offset: self.pos });
        };

        match b {
            b'{' => self.parse_object(),
            b'"' => self.parse_string().map(JsonValue::String),
            b'-' | b'0'..=b'9' => self.parse_number().map(JsonValue::Number),
            b't' => {
                self.expect_literal("true")?;
                Ok(JsonValue::Bool(true))
            }
            b'f' => {
                self.expect_literal("false")?;
                Ok(JsonValue::Bool(false))
            }
            b'n' => {
                self.expect_literal("null")?;
                Ok(JsonValue::Null)
            }
            _ => Err(Error::ExpectedValue { offset: self.pos }),
        }
    }

    fn parse_object(&mut self) -> Result<JsonValue, Error> {
        self.pos += 1; // '{'
        self.skip_ws();
        let mut entries = Vec::new();

        if self.peek_byte() == Some(b'}') {
            self.pos += 1;
            return Ok(JsonValue::Object(entries));
        }

        loop {
            self.skip_ws();
            let key = self.parse_string()?;
            self.skip_ws();
            if self.peek_byte() != Some(b':') {
                return Err(Error::InvalidLiteral { offset: self.pos });
            }
            self.pos += 1;
            self.skip_ws();
            let value = self.parse_value()?;
            entries.push((key, value));
            self.skip_ws();
            match self.peek_byte() {
                Some(b',') => {
                    self.pos += 1;
                }
                Some(b'}') => {
                    self.pos += 1;
                    break;
                }
                _ => return Err(Error::InvalidLiteral { offset: self.pos }),
            }
        }

        Ok(JsonValue::Object(entries))
    }

    fn parse_string(&mut self) -> Result<String, Error> {
        if self.peek_byte() != Some(b'"') {
            return Err(Error::TypeMismatch {
                expected: "string",
                offset: self.pos,
            });
        }
        self.pos += 1;
        let mut out = String::new();

        loop {
            let Some(b) = self.peek_byte() else {
                return Err(Error::UnterminatedString { offset: self.pos });
            };

            match b {
                b'"' => {
                    self.pos += 1;
                    break;
                }
                b'\\' => {
                    self.pos += 1;
                    let Some(esc) = self.peek_byte() else {
                        return Err(Error::UnterminatedString { offset: self.pos });
                    };
                    self.pos += 1;
                    match esc {
                        b'"' => out.push('"'),
                        b'\\' => out.push('\\'),
                        b'/' => out.push('/'),
                        b'b' => out.push('\u{0008}'),
                        b'f' => out.push('\u{000C}'),
                        b'n' => out.push('\n'),
                        b'r' => out.push('\r'),
                        b't' => out.push('\t'),
                        b'u' => {
                            let code = self.parse_hex4()?;
                            let ch = char::from_u32(code as u32)
                                .ok_or(Error::InvalidEscape { offset: self.pos })?;
                            out.push(ch);
                        }
                        _ => {
                            return Err(Error::InvalidEscape {
                                offset: self.pos - 1,
                            });
                        }
                    }
                }
                _ => {
                    let Some(ch) = self.next_char() else {
                        return Err(Error::UnterminatedString { offset: self.pos });
                    };
                    out.push(ch);
                }
            }
        }

        Ok(out)
    }

    fn parse_hex4(&mut self) -> Result<u16, Error> {
        let start = self.pos;
        if self.pos + 4 > self.bytes.len() {
            return Err(Error::InvalidEscape { offset: start });
        }

        let mut value: u16 = 0;
        for _ in 0..4 {
            let b = self.bytes[self.pos];
            self.pos += 1;
            value = value
                .checked_mul(16)
                .ok_or(Error::InvalidEscape { offset: start })?;
            value += match b {
                b'0'..=b'9' => (b - b'0') as u16,
                b'a'..=b'f' => (b - b'a' + 10) as u16,
                b'A'..=b'F' => (b - b'A' + 10) as u16,
                _ => {
                    return Err(Error::InvalidEscape {
                        offset: self.pos - 1,
                    });
                }
            };
        }

        Ok(value)
    }

    fn parse_number(&mut self) -> Result<String, Error> {
        let start = self.pos;
        if self.peek_byte() == Some(b'-') {
            self.pos += 1;
        }

        match self.peek_byte() {
            Some(b'0') => {
                self.pos += 1;
            }
            Some(b'1'..=b'9') => {
                self.pos += 1;
                while matches!(self.peek_byte(), Some(b'0'..=b'9')) {
                    self.pos += 1;
                }
            }
            _ => return Err(Error::InvalidNumber { offset: start }),
        }

        if self.peek_byte() == Some(b'.') {
            self.pos += 1;
            let mut any = false;
            while matches!(self.peek_byte(), Some(b'0'..=b'9')) {
                self.pos += 1;
                any = true;
            }
            if !any {
                return Err(Error::InvalidNumber { offset: self.pos });
            }
        }

        if matches!(self.peek_byte(), Some(b'e' | b'E')) {
            self.pos += 1;
            if matches!(self.peek_byte(), Some(b'+' | b'-')) {
                self.pos += 1;
            }
            let mut any = false;
            while matches!(self.peek_byte(), Some(b'0'..=b'9')) {
                self.pos += 1;
                any = true;
            }
            if !any {
                return Err(Error::InvalidNumber { offset: self.pos });
            }
        }

        Ok(self.input[start..self.pos].to_owned())
    }

    fn expect_literal(&mut self, lit: &str) -> Result<(), Error> {
        let start = self.pos;
        if self.input[self.pos..].starts_with(lit) {
            self.pos += lit.len();
            Ok(())
        } else {
            Err(Error::InvalidLiteral { offset: start })
        }
    }

    fn skip_ws(&mut self) {
        while matches!(self.peek_byte(), Some(b' ' | b'\n' | b'\r' | b'\t')) {
            self.pos += 1;
        }
    }

    fn peek_byte(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn next_char(&mut self) -> Option<char> {
        let rest = self.input.get(self.pos..)?;
        let mut chars = rest.chars();
        let ch = chars.next()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }
}

struct RawValue {
    ptr: *mut u8,
    layout: Option<Layout>,
}

impl RawValue {
    fn new(shape: &'static Shape) -> Result<Self, Error> {
        let layout = shape.layout.sized_layout().ok();
        let ptr = match layout {
            Some(layout) if layout.size() > 0 => {
                // SAFETY: non-zero layout from shape metadata.
                let ptr = unsafe { std::alloc::alloc(layout) };
                if ptr.is_null() {
                    std::alloc::handle_alloc_error(layout);
                }
                ptr
            }
            _ => core::ptr::NonNull::<u8>::dangling().as_ptr(),
        };
        Ok(Self { ptr, layout })
    }

    fn dealloc(self) {
        if let Some(layout) = self.layout
            && layout.size() > 0
        {
            // SAFETY: allocation came from `alloc` with same layout.
            unsafe { std::alloc::dealloc(self.ptr, layout) };
        }
    }

    fn drop_and_dealloc(self, shape: &'static Shape) {
        if let Some(layout) = self.layout
            && layout.size() > 0
        {
            // SAFETY: parse succeeded and value is initialized at ptr.
            let _ = unsafe { shape.call_drop_in_place(PtrMut::new(self.ptr)) };
            // SAFETY: allocation came from `alloc` with same layout.
            unsafe { std::alloc::dealloc(self.ptr, layout) };
        }
    }
}

pub fn from_str<T>(input: &str) -> Result<T, Error>
where
    T: Facet<'static>,
{
    let value = Parser::new(input).parse()?;

    let mut trame = Trame::<LRuntime>::alloc::<T>()?;
    apply_value(&mut trame, Path::empty(), T::SHAPE, &value, 0)?;

    let hv = trame.build()?;
    hv.materialize::<T>().map_err(Error::Trame)
}

fn apply_value(
    trame: &mut Trame<'_, LRuntime>,
    path: Path,
    shape: &'static Shape,
    value: &JsonValue,
    offset: usize,
) -> Result<(), Error> {
    match (&shape.ty, value) {
        (Type::User(UserType::Struct(st)), JsonValue::Object(entries)) => {
            let staged = !path.is_empty();
            if staged {
                trame.apply(Op::Set {
                    dst: path,
                    src: Source::stage(None),
                })?;
            }

            let mut consumed = vec![false; entries.len()];

            for (idx, field) in st.fields.iter().enumerate() {
                let mut match_idx = None;
                for (entry_idx, (key, _)) in entries.iter().enumerate() {
                    if key == field.effective_name()
                        || field.alias.is_some_and(|alias| key == alias)
                    {
                        match_idx = Some(entry_idx);
                        break;
                    }
                }

                match match_idx {
                    Some(entry_idx) => {
                        consumed[entry_idx] = true;
                        let (_, child_value) = &entries[entry_idx];
                        apply_value(
                            trame,
                            Path::field(idx as u32),
                            field.shape.get(),
                            child_value,
                            offset,
                        )?;
                    }
                    None if field.should_skip_deserializing() || field.has_default() => {
                        trame.apply(Op::Set {
                            dst: Path::field(idx as u32),
                            src: Source::default_value(),
                        })?;
                    }
                    None => {
                        return Err(Error::MissingField {
                            field: field.effective_name(),
                            offset,
                        });
                    }
                }
            }

            for (idx, used) in consumed.into_iter().enumerate() {
                if !used {
                    return Err(Error::UnknownField {
                        field: entries[idx].0.clone(),
                        offset,
                    });
                }
            }

            if staged {
                trame.apply(Op::end())?;
            }
            Ok(())
        }
        _ => apply_scalar(trame, path, shape, value, offset),
    }
}

fn apply_scalar(
    trame: &mut Trame<'_, LRuntime>,
    path: Path,
    shape: &'static Shape,
    value: &JsonValue,
    offset: usize,
) -> Result<(), Error> {
    let scalar = match value {
        JsonValue::String(s) => s.as_str(),
        JsonValue::Number(s) => s.as_str(),
        JsonValue::Bool(true) => "true",
        JsonValue::Bool(false) => "false",
        JsonValue::Null => {
            trame.apply(Op::Set {
                dst: path,
                src: Source::default_value(),
            })?;
            return Ok(());
        }
        JsonValue::Object(_) => {
            return Err(Error::TypeMismatch {
                expected: shape.type_identifier,
                offset,
            });
        }
    };

    let raw = RawValue::new(shape)?;
    let parsed = unsafe { shape.call_parse(scalar, PtrUninit::new(raw.ptr)) };

    match parsed {
        Some(Ok(())) => {
            let result = trame.apply(Op::Set {
                dst: path,
                src: unsafe { Source::from_ptr_shape(raw.ptr, shape) },
            });
            match result {
                Ok(()) => {
                    raw.dealloc();
                    Ok(())
                }
                Err(err) => {
                    raw.drop_and_dealloc(shape);
                    Err(Error::Trame(err))
                }
            }
        }
        Some(Err(_)) => {
            raw.dealloc();
            Err(Error::ScalarParse {
                type_name: shape.type_identifier,
                offset,
            })
        }
        None => {
            raw.dealloc();
            Err(Error::UnsupportedShape {
                type_name: shape.type_identifier,
                offset,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, PartialEq, facet::Facet)]
    struct Inner {
        ok: bool,
    }

    #[derive(Debug, PartialEq, facet::Facet)]
    struct Outer {
        count: u32,
        name: String,
        inner: Inner,
    }

    #[test]
    fn deserializes_nested_struct() {
        let value: Outer = from_str(r#"{"count":42,"name":"hello","inner":{"ok":true}}"#)
            .expect("valid JSON should deserialize");

        assert_eq!(
            value,
            Outer {
                count: 42,
                name: "hello".to_owned(),
                inner: Inner { ok: true },
            }
        );
    }

    #[test]
    fn missing_required_field_errors() {
        let err = from_str::<Outer>(r#"{"count":42,"name":"hello"}"#)
            .expect_err("missing required field should error");

        assert_eq!(
            err,
            Error::MissingField {
                field: "inner",
                offset: 0,
            }
        );
    }

    #[test]
    fn unknown_field_errors() {
        let err = from_str::<Outer>(r#"{"count":42,"name":"hello","inner":{"ok":true},"extra":1}"#)
            .expect_err("unknown field should error");

        assert_eq!(
            err,
            Error::UnknownField {
                field: "extra".to_owned(),
                offset: 0,
            }
        );
    }
}
