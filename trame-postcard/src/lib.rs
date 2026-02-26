use facet_core::{Def, Facet, Shape, Type, UserType};
use trame_ir::{
    DECODE_ABI_V1, DecodeInstr, DecodeProgram, ReadScalarOp, ScalarKind, StructFieldPlan,
    StructPlan, VecStructPlan,
};

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

pub fn compile(shape: &'static Shape) -> Result<StructPlan, CompileError> {
    compile_struct_plan(shape, true)
}

pub fn compile_for<T>() -> Result<StructPlan, CompileError>
where
    T: Facet<'static>,
{
    compile(T::SHAPE)
}

pub fn compile_vec(shape: &'static Shape) -> Result<VecStructPlan, CompileError> {
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
    Ok(VecStructPlan {
        vec_shape_id: shape.id,
        element_plan,
    })
}

pub fn compile_vec_for<T>() -> Result<VecStructPlan, CompileError>
where
    T: Facet<'static>,
{
    compile_vec(T::SHAPE)
}

fn scalar_meta_for_shape(shape: &'static Shape) -> Option<(ScalarKind, ReadScalarOp)> {
    if shape.type_identifier == "u32" {
        return Some((ScalarKind::U32, ReadScalarOp::VarintU32));
    }
    if shape.type_identifier == "String" {
        return Some((ScalarKind::String, ReadScalarOp::LenPrefixedUtf8));
    }
    if shape.type_identifier == "bool" {
        return Some((ScalarKind::Bool, ReadScalarOp::BoolByte01));
    }
    None
}

fn compile_struct_plan(
    shape: &'static Shape,
    require_eof: bool,
) -> Result<StructPlan, CompileError> {
    let Type::User(UserType::Struct(st)) = shape.ty else {
        return Err(CompileError::RootNotStruct {
            type_name: shape.type_identifier,
        });
    };

    let mut instructions = Vec::with_capacity(st.fields.len() * 12 + 1);
    let mut field_plans = Vec::with_capacity(st.fields.len());
    let tmp_byte_reg = st.fields.len() as u8;
    let tmp_data_reg = tmp_byte_reg.saturating_add(1);
    for (idx, field) in st.fields.iter().enumerate() {
        let field_shape = field.shape.get();
        let Some((kind, op)) = scalar_meta_for_shape(field_shape) else {
            return Err(CompileError::UnsupportedFieldType {
                field_index: idx,
                field_name: field.name,
                type_name: field_shape.type_identifier,
            });
        };
        match op {
            ReadScalarOp::VarintU32 => {
                emit_varint_u32_read(&mut instructions, idx as u8, tmp_byte_reg, tmp_data_reg);
            }
            ReadScalarOp::LenPrefixedUtf8 => {
                emit_varint_u32_read(&mut instructions, idx as u8, tmp_byte_reg, tmp_data_reg);
                instructions.push(DecodeInstr::ReadUtf8BytesFromLenReg {
                    dst: idx as u8,
                    len_reg: idx as u8,
                });
            }
            ReadScalarOp::BoolByte01 => {
                emit_bool_byte01_read(&mut instructions, idx as u8, tmp_byte_reg, tmp_data_reg);
            }
            _ => instructions.push(DecodeInstr::ReadScalar { op, dst: idx as u8 }),
        }
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
    let program = DecodeProgram {
        abi_version: DECODE_ABI_V1,
        shape_id: shape.id,
        register_count: st.fields.len() + 2,
        instructions,
    };
    Ok(StructPlan {
        shape_id: shape.id,
        program,
        field_plans,
    })
}

fn emit_varint_u32_read(instructions: &mut Vec<DecodeInstr>, dst: u8, tmp_byte: u8, tmp_data: u8) {
    instructions.push(DecodeInstr::SetRegU32 { dst, value: 0 });
    let mut done_jumps = Vec::with_capacity(5);
    for shift in [0u8, 7, 14, 21, 28] {
        instructions.push(DecodeInstr::ReadInputByte { dst: tmp_byte });
        instructions.push(DecodeInstr::AndImmU32 {
            dst: tmp_data,
            src: tmp_byte,
            imm: 0x7f,
        });
        if shift != 0 {
            instructions.push(DecodeInstr::ShlImmU32 {
                dst: tmp_data,
                src: tmp_data,
                shift,
            });
        }
        instructions.push(DecodeInstr::OrU32 {
            dst,
            lhs: dst,
            rhs: tmp_data,
        });
        done_jumps.push(instructions.len());
        instructions.push(DecodeInstr::JumpIfByteHighBitClear {
            src: tmp_byte,
            target: usize::MAX,
        });
    }
    let done_target = instructions.len();
    for jump_idx in done_jumps {
        match &mut instructions[jump_idx] {
            DecodeInstr::JumpIfByteHighBitClear { target, .. } => *target = done_target,
            _ => unreachable!("jump slot should always point to JumpIfByteHighBitClear"),
        }
    }
}

fn emit_bool_byte01_read(instructions: &mut Vec<DecodeInstr>, dst: u8, tmp_byte: u8, tmp_data: u8) {
    instructions.push(DecodeInstr::ReadInputByte { dst: tmp_byte });
    instructions.push(DecodeInstr::AndImmU32 {
        dst: tmp_data,
        src: tmp_byte,
        imm: 0xfe,
    });
    let jump_idx = instructions.len();
    instructions.push(DecodeInstr::JumpIfRegZero {
        src: tmp_data,
        target: usize::MAX,
    });
    instructions.push(DecodeInstr::FailInvalidBool {
        value_reg: tmp_byte,
    });
    let ok_target = instructions.len();
    instructions.push(DecodeInstr::MoveRegU32 { dst, src: tmp_byte });
    match &mut instructions[jump_idx] {
        DecodeInstr::JumpIfRegZero { target, .. } => *target = ok_target,
        _ => unreachable!("jump slot should always point to JumpIfRegZero"),
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

    #[derive(Debug, PartialEq, facet::Facet)]
    struct WrongType {
        only: i64,
    }

    #[test]
    fn compile_rejects_unsupported_field_type() {
        let err = compile_for::<WrongType>().expect_err("shape should be rejected");
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
    fn compile_emits_program() {
        let plan = compile_for::<Demo>().expect("compile should succeed");
        assert_eq!(plan.program.register_count, 4);
        assert_eq!(plan.program.instructions.len(), 54);
    }

    #[test]
    fn compile_is_shape_driven_for_multiple_fields() {
        let plan = compile_for::<Demo3>().expect("compile should succeed");
        assert_eq!(plan.program.register_count, 5);
        assert_eq!(plan.program.instructions.len(), 60);
    }

    #[test]
    fn compile_emits_program_guards() {
        let plan = compile_for::<Demo>().expect("compile should succeed");
        assert_eq!(plan.program.abi_version, DECODE_ABI_V1);
        assert_eq!(plan.program.shape_id, Demo::SHAPE.id);
    }

    #[test]
    fn compile_vec_rejects_non_list_root() {
        let err = compile_vec_for::<Demo>().expect_err("non-list root should fail");
        assert_eq!(err, CompileError::RootNotList { type_name: "Demo" });
    }

    #[test]
    fn compile_vec_rejects_non_struct_elements() {
        let err = compile_vec_for::<Vec<u32>>().expect_err("scalar elements should be rejected");
        assert_eq!(err, CompileError::ListElementNotStruct { type_name: "u32" });
    }
}
