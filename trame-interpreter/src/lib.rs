use facet_core::Facet;
use trame_ir::{
    DecodeInstr, Error, ReadScalarOp, ScalarKind, StructFieldPlan, StructPlan, VecStructPlan,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum RegValue {
    Unset,
    U32(u32),
    InputRange { offset: usize, len: usize },
    ValidatedInputRange { offset: usize, len: usize },
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

    fn read_u32(&mut self) -> Result<u32, Error> {
        let start = self.pos;
        let mut out = 0u32;

        for shift in [0, 7, 14, 21, 28] {
            let byte = self.read_byte()?;
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

    fn skip_json_ws(&mut self) {
        while let Some(&byte) = self.input.get(self.pos) {
            if matches!(byte, b' ' | b'\t' | b'\n' | b'\r') {
                self.pos += 1;
                continue;
            }
            break;
        }
    }

    fn read_decimal_u32(&mut self) -> Result<u32, Error> {
        self.skip_json_ws();
        let start = self.pos;
        let mut value = 0u32;
        let mut saw_digit = false;
        while let Some(&byte) = self.input.get(self.pos) {
            if !byte.is_ascii_digit() {
                break;
            }
            saw_digit = true;
            value = value
                .checked_mul(10)
                .and_then(|v| v.checked_add((byte - b'0') as u32))
                .ok_or(Error::VarintOverflow { offset: start })?;
            self.pos += 1;
        }
        if !saw_digit {
            return Err(Error::ShapeMismatch);
        }
        Ok(value)
    }

    fn read_quoted_utf8_string(&mut self) -> Result<String, Error> {
        self.skip_json_ws();
        if self.read_byte()? != b'"' {
            return Err(Error::ShapeMismatch);
        }
        let start = self.pos;
        while let Some(&byte) = self.input.get(self.pos) {
            self.pos += 1;
            match byte {
                b'"' => {
                    let bytes = &self.input[start..self.pos - 1];
                    let value = core::str::from_utf8(bytes)
                        .map_err(|_| Error::InvalidUtf8 { offset: start })?;
                    return Ok(value.to_owned());
                }
                b'\\' => return Err(Error::ShapeMismatch),
                _ => {}
            }
        }
        Err(Error::UnexpectedEof { offset: start })
    }

    fn read_bool_literal(&mut self) -> Result<bool, Error> {
        self.skip_json_ws();
        let remaining = &self.input[self.pos..];
        if remaining.starts_with(b"true") {
            self.pos += 4;
            return Ok(true);
        }
        if remaining.starts_with(b"false") {
            self.pos += 5;
            return Ok(false);
        }
        Err(Error::ShapeMismatch)
    }

    fn expect_input_byte(&mut self, expected: u8) -> Result<(), Error> {
        let found = self.read_byte()?;
        if found == expected {
            return Ok(());
        }
        Err(Error::ShapeMismatch)
    }
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
        (ScalarKind::Bool, RegValue::U32(value)) => match *value {
            0 => unsafe { (dst as *mut bool).write(false) },
            1 => unsafe { (dst as *mut bool).write(true) },
            other => {
                return Err(Error::InvalidBool {
                    offset: 0,
                    value: other as u8,
                });
            }
        },
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

fn read_reg_u32(regs: &[RegValue], idx: usize) -> Result<u32, Error> {
    let reg = regs.get(idx).ok_or(Error::InvalidRegister { reg: idx })?;
    match reg {
        RegValue::U32(value) => Ok(*value),
        RegValue::Unset => Err(Error::RegisterUnset { reg: idx }),
        _ => Err(Error::ShapeMismatch),
    }
}

fn read_input_range(regs: &[RegValue], idx: usize) -> Result<(usize, usize, bool), Error> {
    let reg = regs.get(idx).ok_or(Error::InvalidRegister { reg: idx })?;
    match reg {
        RegValue::InputRange { offset, len } => Ok((*offset, *len, false)),
        RegValue::ValidatedInputRange { offset, len } => Ok((*offset, *len, true)),
        RegValue::Unset => Err(Error::RegisterUnset { reg: idx }),
        _ => Err(Error::ShapeMismatch),
    }
}

fn is_valid_utf8(bytes: &[u8]) -> bool {
    let mut i = 0usize;
    while i < bytes.len() {
        let b0 = bytes[i];
        if b0 < 0x80 {
            i += 1;
            continue;
        }
        if (0xC2..=0xDF).contains(&b0) {
            if i + 1 >= bytes.len() || (bytes[i + 1] & 0xC0) != 0x80 {
                return false;
            }
            i += 2;
            continue;
        }
        if b0 == 0xE0 {
            if i + 2 >= bytes.len() {
                return false;
            }
            let b1 = bytes[i + 1];
            let b2 = bytes[i + 2];
            if !(0xA0..=0xBF).contains(&b1) || (b2 & 0xC0) != 0x80 {
                return false;
            }
            i += 3;
            continue;
        }
        if (0xE1..=0xEC).contains(&b0) || (0xEE..=0xEF).contains(&b0) {
            if i + 2 >= bytes.len() {
                return false;
            }
            let b1 = bytes[i + 1];
            let b2 = bytes[i + 2];
            if (b1 & 0xC0) != 0x80 || (b2 & 0xC0) != 0x80 {
                return false;
            }
            i += 3;
            continue;
        }
        if b0 == 0xED {
            if i + 2 >= bytes.len() {
                return false;
            }
            let b1 = bytes[i + 1];
            let b2 = bytes[i + 2];
            if !(0x80..=0x9F).contains(&b1) || (b2 & 0xC0) != 0x80 {
                return false;
            }
            i += 3;
            continue;
        }
        if b0 == 0xF0 {
            if i + 3 >= bytes.len() {
                return false;
            }
            let b1 = bytes[i + 1];
            let b2 = bytes[i + 2];
            let b3 = bytes[i + 3];
            if !(0x90..=0xBF).contains(&b1) || (b2 & 0xC0) != 0x80 || (b3 & 0xC0) != 0x80 {
                return false;
            }
            i += 4;
            continue;
        }
        if (0xF1..=0xF3).contains(&b0) {
            if i + 3 >= bytes.len() {
                return false;
            }
            let b1 = bytes[i + 1];
            let b2 = bytes[i + 2];
            let b3 = bytes[i + 3];
            if (b1 & 0xC0) != 0x80 || (b2 & 0xC0) != 0x80 || (b3 & 0xC0) != 0x80 {
                return false;
            }
            i += 4;
            continue;
        }
        if b0 == 0xF4 {
            if i + 3 >= bytes.len() {
                return false;
            }
            let b1 = bytes[i + 1];
            let b2 = bytes[i + 2];
            let b3 = bytes[i + 3];
            if !(0x80..=0x8F).contains(&b1) || (b2 & 0xC0) != 0x80 || (b3 & 0xC0) != 0x80 {
                return false;
            }
            i += 4;
            continue;
        }
        return false;
    }
    true
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

fn decode_struct_with_reader<T>(plan: &StructPlan, reader: &mut Reader<'_>) -> Result<T, Error>
where
    T: Facet<'static>,
{
    let mut regs = vec![RegValue::Unset; plan.program.register_count];
    let mut field_inits = vec![0u8; plan.field_plans.len()];
    let mut out = core::mem::MaybeUninit::<T>::uninit();
    let mut pc = 0usize;
    while let Some(instr) = plan.program.instructions.get(pc).copied() {
        let step = match instr {
            DecodeInstr::SkipJsonWhitespace => {
                reader.skip_json_ws();
                Ok(())
            }
            DecodeInstr::ExpectInputByte { value } => reader.expect_input_byte(value),
            DecodeInstr::SetRegU32 { dst, value } => {
                let slot = regs
                    .get_mut(dst as usize)
                    .ok_or(Error::InvalidRegister { reg: dst as usize })?;
                *slot = RegValue::U32(value);
                Ok(())
            }
            DecodeInstr::ReadInputByte { dst } => {
                let slot = regs
                    .get_mut(dst as usize)
                    .ok_or(Error::InvalidRegister { reg: dst as usize })?;
                *slot = RegValue::U32(reader.read_byte()? as u32);
                Ok(())
            }
            DecodeInstr::CaptureInputRangeByLenReg { dst, len_reg } => {
                let len = read_reg_u32(&regs, len_reg as usize)? as usize;
                let offset = reader.offset();
                let _ = reader.read_bytes(len)?;
                let slot = regs
                    .get_mut(dst as usize)
                    .ok_or(Error::InvalidRegister { reg: dst as usize })?;
                *slot = RegValue::InputRange { offset, len };
                Ok(())
            }
            DecodeInstr::ValidateUtf8Range { dst, src } => {
                let (offset, len, _) = read_input_range(&regs, src as usize)?;
                let end = offset
                    .checked_add(len)
                    .ok_or(Error::UnexpectedEof { offset })?;
                let bytes = reader
                    .input
                    .get(offset..end)
                    .ok_or(Error::UnexpectedEof { offset })?;
                if !is_valid_utf8(bytes) {
                    return Err(Error::InvalidUtf8 { offset });
                }
                let slot = regs
                    .get_mut(dst as usize)
                    .ok_or(Error::InvalidRegister { reg: dst as usize })?;
                *slot = RegValue::ValidatedInputRange { offset, len };
                Ok(())
            }
            DecodeInstr::Utf8RangeToString { dst, src } => {
                let (offset, len, is_validated) = read_input_range(&regs, src as usize)?;
                let end = offset
                    .checked_add(len)
                    .ok_or(Error::UnexpectedEof { offset })?;
                let bytes = reader
                    .input
                    .get(offset..end)
                    .ok_or(Error::UnexpectedEof { offset })?;
                let value = if is_validated {
                    unsafe { core::str::from_utf8_unchecked(bytes) }.to_owned()
                } else {
                    core::str::from_utf8(bytes)
                        .map_err(|_| Error::InvalidUtf8 { offset })?
                        .to_owned()
                };
                let slot = regs
                    .get_mut(dst as usize)
                    .ok_or(Error::InvalidRegister { reg: dst as usize })?;
                *slot = RegValue::String(value);
                Ok(())
            }
            DecodeInstr::MoveRegU32 { dst, src } => {
                let out = read_reg_u32(&regs, src as usize)?;
                let slot = regs
                    .get_mut(dst as usize)
                    .ok_or(Error::InvalidRegister { reg: dst as usize })?;
                *slot = RegValue::U32(out);
                Ok(())
            }
            DecodeInstr::AndImmU32 { dst, src, imm } => {
                let out = read_reg_u32(&regs, src as usize)? & imm;
                let slot = regs
                    .get_mut(dst as usize)
                    .ok_or(Error::InvalidRegister { reg: dst as usize })?;
                *slot = RegValue::U32(out);
                Ok(())
            }
            DecodeInstr::ShlImmU32 { dst, src, shift } => {
                let out = read_reg_u32(&regs, src as usize)? << shift;
                let slot = regs
                    .get_mut(dst as usize)
                    .ok_or(Error::InvalidRegister { reg: dst as usize })?;
                *slot = RegValue::U32(out);
                Ok(())
            }
            DecodeInstr::OrU32 { dst, lhs, rhs } => {
                let out = read_reg_u32(&regs, lhs as usize)? | read_reg_u32(&regs, rhs as usize)?;
                let slot = regs
                    .get_mut(dst as usize)
                    .ok_or(Error::InvalidRegister { reg: dst as usize })?;
                *slot = RegValue::U32(out);
                Ok(())
            }
            DecodeInstr::JumpIfRegZero { src, target } => {
                if target > plan.program.instructions.len() {
                    return Err(Error::ShapeMismatch);
                }
                if read_reg_u32(&regs, src as usize)? == 0 {
                    pc = target;
                    continue;
                }
                Ok(())
            }
            DecodeInstr::JumpIfByteHighBitClear { src, target } => {
                if target > plan.program.instructions.len() {
                    return Err(Error::ShapeMismatch);
                }
                let value = read_reg_u32(&regs, src as usize)?;
                if (value & 0x80) == 0 {
                    pc = target;
                    continue;
                }
                Ok(())
            }
            DecodeInstr::FailInvalidBool { value_reg } => {
                let value = read_reg_u32(&regs, value_reg as usize)? as u8;
                let offset = reader.offset().saturating_sub(1);
                return Err(Error::InvalidBool { offset, value });
            }
            DecodeInstr::ReadScalar { op, dst } => {
                let slot = regs
                    .get_mut(dst as usize)
                    .ok_or(Error::InvalidRegister { reg: dst as usize })?;
                *slot = match op {
                    ReadScalarOp::VarintU32 => RegValue::U32(reader.read_u32()?),
                    ReadScalarOp::LenPrefixedUtf8 => {
                        let (str_offset, str_bytes) = reader.read_string_bytes()?;
                        let value = core::str::from_utf8(str_bytes)
                            .map_err(|_| Error::InvalidUtf8 { offset: str_offset })?
                            .to_owned();
                        RegValue::String(value)
                    }
                    ReadScalarOp::BoolByte01 => RegValue::Bool(reader.read_bool()?),
                    ReadScalarOp::DecimalU32 => RegValue::U32(reader.read_decimal_u32()?),
                    ReadScalarOp::QuotedUtf8String => {
                        RegValue::String(reader.read_quoted_utf8_string()?)
                    }
                    ReadScalarOp::BoolLiteral => RegValue::Bool(reader.read_bool_literal()?),
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
        pc += 1;
    }

    if let Some(missing) = field_inits.iter().position(|init| *init == 0) {
        unsafe {
            drop_initialized_fields(out.as_mut_ptr().cast(), &plan.field_plans, &field_inits);
        }
        return Err(Error::RegisterUnset { reg: missing });
    }

    Ok(unsafe { out.assume_init() })
}

pub fn decode<T>(plan: &StructPlan, input: &[u8]) -> Result<T, Error>
where
    T: Facet<'static>,
{
    if plan.shape_id != T::SHAPE.id || plan.program.shape_id != T::SHAPE.id {
        return Err(Error::ShapeMismatch);
    }
    let mut reader = Reader::new(input);
    decode_struct_with_reader::<T>(plan, &mut reader)
}

pub fn decode_vec<T>(plan: &VecStructPlan, input: &[u8]) -> Result<Vec<T>, Error>
where
    T: Facet<'static>,
{
    if plan.vec_shape_id != <Vec<T>>::SHAPE.id || plan.element_plan.program.shape_id != T::SHAPE.id
    {
        return Err(Error::ShapeMismatch);
    }
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

    #[test]
    fn decode_demo() {
        let plan = trame_postcard::compile_for::<Demo>().expect("compile should succeed");
        let wire = encode_demo_wire(7, "alice");
        let value: Demo = decode(&plan, &wire).expect("decode should succeed");
        assert_eq!(
            value,
            Demo {
                id: 7,
                name: "alice".into()
            }
        );
    }

    #[test]
    fn decode_shape_driven_three_fields() {
        let plan = trame_postcard::compile_for::<Demo3>().expect("compile should succeed");
        let wire = encode_demo3_wire(7, "alice", true);
        let value: Demo3 = decode(&plan, &wire).expect("decode should succeed");
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
        let plan = trame_postcard::compile_vec_for::<Vec<Demo3>>().expect("compile should succeed");
        let actual = decode_vec::<Demo3>(&plan, &wire).expect("decode should succeed");
        assert_eq!(actual, expected);
    }
}
