# list:
#     just --list

all:
    # disabled: bit slow still
    # just kani
    just test
    just prove

# Run all tests (including prop tests)
test:
    cargo nextest run

# Test all kani proofs using soteria-rust
kani:
    just kani-one trame-runtime
    just kani-one trame

# Test kani proofs via soteria-rust. Use `--filter substring` to run only some tests
kani-one crate *args:
    soteria-rust cargo {{ crate }} --kani --summary --stats stdout --flamegraph-dir .soteria {{ args }}

# Prove with creusot
prove *args:
    cargo creusot clean --force
    cargo creusot prove {{ args }}

# Run fuzzing with afl
fuzz:
    cd trame-fuzz && cargo afl build
    cd trame-fuzz && cargo afl fuzz -i in -o out ./target/debug/trame-afl
