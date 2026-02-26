use facet_core::Facet;
use trame_ir::{Error, StructPlan, VecStructPlan};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackendKind {
    Interpreter,
    #[cfg(feature = "dynasm-rt")]
    DynasmRt,
}

pub fn decode<T>(plan: &StructPlan, input: &[u8], backend: BackendKind) -> Result<T, Error>
where
    T: Facet<'static>,
{
    match backend {
        BackendKind::Interpreter => trame_interpreter::decode(plan, input),
        #[cfg(feature = "dynasm-rt")]
        BackendKind::DynasmRt => trame_dynasm_decode(plan, input),
    }
}

pub fn decode_vec<T>(
    plan: &VecStructPlan,
    input: &[u8],
    backend: BackendKind,
) -> Result<Vec<T>, Error>
where
    T: Facet<'static>,
{
    match backend {
        BackendKind::Interpreter => trame_interpreter::decode_vec(plan, input),
        #[cfg(feature = "dynasm-rt")]
        BackendKind::DynasmRt => trame_dynasm_decode_vec(plan, input),
    }
}

#[cfg(feature = "dynasm-rt")]
fn trame_dynasm_decode<T>(plan: &StructPlan, input: &[u8]) -> Result<T, Error>
where
    T: Facet<'static>,
{
    trame_dynasm::decode(plan, input)
}

#[cfg(feature = "dynasm-rt")]
fn trame_dynasm_decode_vec<T>(plan: &VecStructPlan, input: &[u8]) -> Result<Vec<T>, Error>
where
    T: Facet<'static>,
{
    trame_dynasm::decode_vec(plan, input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, PartialEq, facet::Facet)]
    struct Demo3 {
        id: u32,
        name: String,
        ok: bool,
    }

    #[test]
    fn interpreter_backend_decodes() {
        let expected = Demo3 {
            id: 7,
            name: "alice".into(),
            ok: true,
        };
        let wire = facet_postcard::to_vec(&expected).expect("encode");
        let plan = trame_postcard::compile_for::<Demo3>().expect("compile");
        let actual: Demo3 = decode(&plan, &wire, BackendKind::Interpreter).expect("decode");
        assert_eq!(actual, expected);
    }

    #[test]
    fn interpreter_backend_decodes_json_plan() {
        let expected = Demo3 {
            id: 7,
            name: "alice".into(),
            ok: true,
        };
        let input = br#"{ "id": 7, "name": "alice", "ok": true }"#;
        let plan = trame_json::compile_for::<Demo3>().expect("compile");
        let actual: Demo3 = decode(&plan, input, BackendKind::Interpreter).expect("decode");
        assert_eq!(actual, expected);
    }

    #[cfg(feature = "dynasm-rt")]
    #[test]
    fn dynasm_backend_matches_interpreter() {
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
        let wire = facet_postcard::to_vec(&expected).expect("encode");
        let plan = trame_postcard::compile_vec_for::<Vec<Demo3>>().expect("compile");
        let from_interp =
            decode_vec::<Demo3>(&plan, &wire, BackendKind::Interpreter).expect("interp");
        let from_dynasm = decode_vec::<Demo3>(&plan, &wire, BackendKind::DynasmRt).expect("dynasm");
        assert_eq!(from_interp, from_dynasm);
    }
}
