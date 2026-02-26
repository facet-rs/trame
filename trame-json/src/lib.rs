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
        return Some((ScalarKind::U32, ReadScalarOp::DecimalU32));
    }
    if shape.type_identifier == "String" {
        return Some((ScalarKind::String, ReadScalarOp::QuotedUtf8String));
    }
    if shape.type_identifier == "bool" {
        return Some((ScalarKind::Bool, ReadScalarOp::BoolLiteral));
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

    let mut instructions = Vec::with_capacity(st.fields.len() * 2 + 1);
    let mut field_plans = Vec::with_capacity(st.fields.len());
    for (idx, field) in st.fields.iter().enumerate() {
        let field_shape = field.shape.get();
        let Some((kind, op)) = scalar_meta_for_shape(field_shape) else {
            return Err(CompileError::UnsupportedFieldType {
                field_index: idx,
                field_name: field.name,
                type_name: field_shape.type_identifier,
            });
        };
        instructions.push(DecodeInstr::ReadScalar { op, dst: idx as u8 });
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
        register_count: st.fields.len(),
        instructions,
    };
    Ok(StructPlan {
        shape_id: shape.id,
        program,
        field_plans,
    })
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
    fn compile_emits_program_guards() {
        let plan = compile_for::<Demo>().expect("compile should succeed");
        assert_eq!(plan.program.abi_version, DECODE_ABI_V1);
        assert_eq!(plan.program.shape_id, Demo::SHAPE.id);
    }

    #[test]
    fn compile_matches_postcard_generator_for_scalar_subset() {
        let from_json = compile_for::<Demo3>().expect("json compile");
        let from_postcard = trame_postcard::compile_for::<Demo3>().expect("postcard compile");
        assert_ne!(from_json.program, from_postcard.program);
        assert_eq!(from_json.field_plans, from_postcard.field_plans);
    }

    #[test]
    fn compile_vec_matches_postcard_generator_for_scalar_subset() {
        let from_json = compile_vec_for::<Vec<Demo3>>().expect("json compile");
        let from_postcard =
            trame_postcard::compile_vec_for::<Vec<Demo3>>().expect("postcard compile");
        assert_ne!(
            from_json.element_plan.program,
            from_postcard.element_plan.program
        );
        assert_eq!(
            from_json.element_plan.field_plans,
            from_postcard.element_plan.field_plans
        );
    }

    #[test]
    fn interpreter_decodes_scalar_stream_for_json_ops() {
        let plan = compile_for::<Demo3>().expect("json compile");
        let input = br#"42 "alice" true"#;
        let decoded: Demo3 = trame_interpreter::decode(&plan, input).expect("decode");
        assert_eq!(
            decoded,
            Demo3 {
                id: 42,
                name: "alice".into(),
                ok: true,
            }
        );
    }

    #[test]
    fn interpreter_decodes_utf8_quoted_string() {
        let plan = compile_for::<Demo>().expect("json compile");
        let input = "7 \"héllo\"".as_bytes();
        let decoded: Demo = trame_interpreter::decode(&plan, input).expect("decode");
        assert_eq!(
            decoded,
            Demo {
                id: 7,
                name: "héllo".into()
            }
        );
    }
}
