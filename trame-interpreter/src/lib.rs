use facet_core::Facet;
use trame_ir::{DecodeInstr, Error, ScalarKind, StructFieldPlan, StructPlan, VecStructPlan};

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

fn decode_struct_with_reader<T>(plan: &StructPlan, reader: &mut Reader<'_>) -> Result<T, Error>
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

pub fn decode<T>(plan: &StructPlan, input: &[u8]) -> Result<T, Error>
where
    T: Facet<'static>,
{
    plan.ensure_shape::<T>()?;
    let mut reader = Reader::new(input);
    decode_struct_with_reader::<T>(plan, &mut reader)
}

pub fn decode_vec<T>(plan: &VecStructPlan, input: &[u8]) -> Result<Vec<T>, Error>
where
    T: Facet<'static>,
{
    plan.ensure_vec_shape::<T>()?;
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
