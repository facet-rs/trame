use divan::{Bencher, black_box};
use facet::Facet;
use serde::{Deserialize, Serialize};
use std::sync::LazyLock;
use trame_dynasm::{DynasmPrepared, decode as decode_dynasm, prepare};
use trame_ir::Error;

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

fn is_json_ws(byte: u8) -> bool {
    matches!(byte, b' ' | b'\t' | b'\n' | b'\r')
}

fn skip_json_ws(input: &[u8], mut i: usize) -> usize {
    while let Some(&byte) = input.get(i) {
        if is_json_ws(byte) {
            i += 1;
            continue;
        }
        break;
    }
    i
}

fn trim_ws_range(input: &[u8], mut start: usize, mut end: usize) -> (usize, usize) {
    while start < end && is_json_ws(input[start]) {
        start += 1;
    }
    while start < end && is_json_ws(input[end - 1]) {
        end -= 1;
    }
    (start, end)
}

fn decode_json_array_with<T, F>(input: &[u8], mut decode_item: F) -> Result<Vec<T>, Error>
where
    F: FnMut(&[u8]) -> Result<T, Error>,
{
    let mut i = skip_json_ws(input, 0);
    if input.get(i).copied() != Some(b'[') {
        return Err(Error::ShapeMismatch);
    }
    i += 1;
    let mut out = Vec::new();

    loop {
        i = skip_json_ws(input, i);
        match input.get(i).copied() {
            Some(b']') => {
                i += 1;
                i = skip_json_ws(input, i);
                if i != input.len() {
                    return Err(Error::TrailingBytes { offset: i });
                }
                return Ok(out);
            }
            Some(_) => {}
            None => return Err(Error::UnexpectedEof { offset: i }),
        }

        let item_start = i;
        let mut in_string = false;
        let mut escaped = false;
        let mut object_depth = 0usize;
        let mut array_depth = 0usize;

        loop {
            let Some(&byte) = input.get(i) else {
                return Err(Error::UnexpectedEof { offset: i });
            };
            if in_string {
                i += 1;
                if escaped {
                    escaped = false;
                    continue;
                }
                if byte == b'\\' {
                    escaped = true;
                    continue;
                }
                if byte == b'"' {
                    in_string = false;
                }
                continue;
            }

            match byte {
                b'"' => {
                    in_string = true;
                    i += 1;
                }
                b'{' => {
                    object_depth += 1;
                    i += 1;
                }
                b'}' => {
                    if object_depth == 0 {
                        return Err(Error::ShapeMismatch);
                    }
                    object_depth -= 1;
                    i += 1;
                }
                b'[' => {
                    array_depth += 1;
                    i += 1;
                }
                b']' => {
                    if object_depth == 0 && array_depth == 0 {
                        let (start, end) = trim_ws_range(input, item_start, i);
                        out.push(decode_item(&input[start..end])?);
                        i += 1;
                        i = skip_json_ws(input, i);
                        if i != input.len() {
                            return Err(Error::TrailingBytes { offset: i });
                        }
                        return Ok(out);
                    }
                    if array_depth == 0 {
                        return Err(Error::ShapeMismatch);
                    }
                    array_depth -= 1;
                    i += 1;
                }
                b',' => {
                    if object_depth == 0 && array_depth == 0 {
                        let (start, end) = trim_ws_range(input, item_start, i);
                        out.push(decode_item(&input[start..end])?);
                        i += 1;
                        break;
                    }
                    i += 1;
                }
                _ => i += 1,
            }
        }
    }
}

static SAMPLES: LazyLock<Vec<SampleStruct>> = LazyLock::new(make_samples);
static JSON_WIRE: LazyLock<Vec<u8>> =
    LazyLock::new(|| serde_json::to_vec(&*SAMPLES).expect("json serialization should work"));
static JSON_STRUCT_PLAN: LazyLock<trame_ir::StructPlan> =
    LazyLock::new(|| trame_json::compile_for::<SampleStruct>().expect("json struct plan compile"));
static DYNASM_STRUCT: LazyLock<Option<DynasmPrepared<SampleStruct>>> =
    LazyLock::new(|| prepare::<SampleStruct>(&JSON_STRUCT_PLAN));

#[divan::bench]
fn trame_interpreter_json_vec_program(bencher: Bencher) {
    let wire = &*JSON_WIRE;
    let plan = &*JSON_STRUCT_PLAN;
    bencher.bench(|| {
        black_box(
            decode_json_array_with(black_box(wire), |item| {
                trame_interpreter::decode::<SampleStruct>(plan, item)
            })
            .expect("decode should succeed"),
        )
    });
}

#[divan::bench]
fn trame_dynasm_rt_json_vec_program(bencher: Bencher) {
    let wire = &*JSON_WIRE;
    let plan = &*JSON_STRUCT_PLAN;
    let prepared = &*DYNASM_STRUCT;
    bencher.bench(|| {
        black_box(
            decode_json_array_with(black_box(wire), |item| {
                if let Some(prepared) = prepared {
                    prepared.decode(item)
                } else {
                    trame_interpreter::decode::<SampleStruct>(plan, item)
                }
            })
            .expect("decode should succeed"),
        )
    });
}

#[divan::bench]
fn trame_dynasm_rt_json_vec_program_no_prepare_cache(bencher: Bencher) {
    let wire = &*JSON_WIRE;
    let plan = &*JSON_STRUCT_PLAN;
    bencher.bench(|| {
        black_box(
            decode_json_array_with(black_box(wire), |item| {
                decode_dynasm::<SampleStruct>(plan, item)
            })
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
