# list:
#     just --list

all:
    # disabled: bit slow still
    # just kani
    just test

# Creusot migration target (currently expected to require follow-up)
prove *args:
    cargo creusot clean --force
    cargo creusot prove {{ args }} -- -p trame-runtime -p trame

# Run all tests (including prop tests)
test:
    cargo nextest run

# Run property tests only
prop:
    cargo nextest run -p trame-proptest

# Test all kani proofs using soteria-rust
kani:
    just kani-one trame-runtime
    just kani-one trame

# Test kani proofs via soteria-rust. Use `--filter substring` to run only some tests
kani-one crate *args:
    soteria-rust cargo {{ crate }} --kani --summary --stats stdout --flamegraph-dir .soteria {{ args }}

# Run fuzzing with afl
fuzz:
    cd trame-fuzz && CARGO_TARGET_DIR=target-afl cargo afl build
    cd trame-fuzz && CARGO_TARGET_DIR=target-afl cargo afl fuzz -i in -o out ./target-afl/debug/trame-fuzz

# Run VShape fuzzing target (LRuntime + VShapeView)
fuzz-vshape:
    cd trame-fuzz && just fuzz-vshape

# Run solver fuzzing target (trame-solver key routing)
fuzz-solver:
    cd trame-fuzz && just fuzz-solver

ci-push ident:
    depot build --push --build-platform=linux/amd64 -t ghcr.io/facet-rs/{{ ident }}-ci:latest -f .docker/Dockerfile.{{ ident }} .docker/

ci-push-all:
    just ci-push afl
    just ci-push soteria
