use divan::{Bencher, black_box};
use facet::Facet;
use serde::{Deserialize, Serialize};
use std::sync::LazyLock;
use trame_dynasm::{DynasmPreparedVec, decode_vec as decode_dynasm_vec, prepare_vec};

#[derive(Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize)]
struct SampleStruct {
    id: u32,
    name: String,
    ok: bool,
}

const BATCH_SIZE: usize = 512;

fn make_samples() -> Vec<SampleStruct> {
    (0..BATCH_SIZE)
        .map(|i| SampleStruct {
            id: (i as u32) * 17 + 3,
            name: format!("name-{i:04}-{}", "xyz".repeat((i % 5) + 1)),
            ok: i % 3 != 0,
        })
        .collect()
}

static SAMPLES: LazyLock<Vec<SampleStruct>> = LazyLock::new(make_samples);
static JSON_WIRE: LazyLock<Vec<u8>> =
    LazyLock::new(|| serde_json::to_vec(&*SAMPLES).expect("json serialization should work"));
static JSON_VEC_PLAN: LazyLock<trame_ir::VecStructPlan> = LazyLock::new(|| {
    trame_json::compile_vec_for::<Vec<SampleStruct>>().expect("json vec plan compile")
});
static DYNASM_VEC: LazyLock<Option<DynasmPreparedVec<SampleStruct>>> =
    LazyLock::new(|| prepare_vec::<SampleStruct>(&JSON_VEC_PLAN));

#[divan::bench]
fn trame_interpreter_json_vec_program(bencher: Bencher) {
    let wire = &*JSON_WIRE;
    let plan = &*JSON_VEC_PLAN;
    bencher.bench(|| {
        black_box(
            trame_interpreter::decode_vec::<SampleStruct>(plan, black_box(wire))
                .expect("decode should succeed"),
        )
    });
}

#[divan::bench]
fn trame_dynasm_rt_json_vec_program(bencher: Bencher) {
    let wire = &*JSON_WIRE;
    let plan = &*JSON_VEC_PLAN;
    let prepared = &*DYNASM_VEC;
    bencher.bench(|| {
        black_box(
            if let Some(prepared) = prepared {
                prepared.decode(black_box(wire))
            } else {
                trame_interpreter::decode_vec::<SampleStruct>(plan, black_box(wire))
            }
            .expect("decode should succeed"),
        )
    });
}

#[divan::bench]
fn trame_dynasm_rt_json_vec_program_no_prepare_cache(bencher: Bencher) {
    let wire = &*JSON_WIRE;
    let plan = &*JSON_VEC_PLAN;
    bencher.bench(|| {
        black_box(
            decode_dynasm_vec::<SampleStruct>(plan, black_box(wire))
                .expect("decode should succeed"),
        )
    });
}

#[divan::bench]
fn facet_json_vec_from_slice(bencher: Bencher) {
    let wire = &*JSON_WIRE;
    bencher.bench(|| {
        black_box(
            facet_json::from_slice::<Vec<SampleStruct>>(black_box(wire))
                .expect("decode should succeed"),
        )
    });
}

#[divan::bench]
fn serde_json_vec_from_slice(bencher: Bencher) {
    let wire = &*JSON_WIRE;
    bencher.bench(|| {
        black_box(
            serde_json::from_slice::<Vec<SampleStruct>>(black_box(wire))
                .expect("decode should succeed"),
        )
    });
}

fn main() {
    divan::main();
}
