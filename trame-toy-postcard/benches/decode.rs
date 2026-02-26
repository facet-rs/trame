use divan::{Bencher, black_box};
use facet::Facet;
use serde::{Deserialize, Serialize};
use std::sync::LazyLock;
#[cfg(feature = "dynasm-rt")]
use trame_toy_postcard::DynasmPreparedVec;
use trame_toy_postcard::{BackendKind, PostcardVecStructPlan};

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
static ENCODED_VEC: LazyLock<Vec<u8>> =
    LazyLock::new(|| postcard::to_allocvec(&*SAMPLES).expect("batch serialization should work"));
static VEC_PLAN: LazyLock<PostcardVecStructPlan> = LazyLock::new(|| {
    PostcardVecStructPlan::compile_for::<Vec<SampleStruct>>().expect("vec plan compile")
});
#[cfg(feature = "dynasm-rt")]
static DYNASM_VEC: LazyLock<DynasmPreparedVec<SampleStruct>> = LazyLock::new(|| {
    (&*VEC_PLAN)
        .prepare_dynasm::<SampleStruct>()
        .expect("dynasm vec prepare should succeed")
});

#[divan::bench]
fn trame_interpreter_vec_program(bencher: Bencher) {
    let wire = &*ENCODED_VEC;
    let plan = &*VEC_PLAN;
    bencher.bench(|| {
        black_box(
            plan.decode_vec_on::<SampleStruct>(black_box(wire), BackendKind::Interpreter)
                .expect("decode should succeed"),
        )
    });
}

#[cfg(feature = "dynasm-rt")]
#[divan::bench]
fn trame_dynasm_rt_vec_program(bencher: Bencher) {
    let wire = &*ENCODED_VEC;
    let dynasm = &*DYNASM_VEC;
    bencher.bench(|| {
        black_box(
            dynasm
                .decode(black_box(wire))
                .expect("decode should succeed"),
        )
    });
}

#[divan::bench]
fn facet_postcard_vec_from_slice(bencher: Bencher) {
    let wire = &*ENCODED_VEC;
    bencher.bench(|| {
        black_box(
            facet_postcard::from_slice::<Vec<SampleStruct>>(black_box(wire))
                .expect("decode should succeed"),
        )
    });
}

#[divan::bench]
fn postcard_serde_from_bytes(bencher: Bencher) {
    let wire = &*ENCODED_VEC;
    bencher.bench(|| {
        black_box(
            postcard::from_bytes::<Vec<SampleStruct>>(black_box(wire))
                .expect("decode should succeed"),
        )
    });
}

fn main() {
    divan::main();
}
