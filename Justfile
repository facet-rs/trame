list:
    just --list

all:
    just kani
    just test

prove:
    just prove-one trame-runtime
    just prove-one trame

prop:
    cd trame-proptest && cargo nextest run

fuzz:
    cd trame-fuzz && cargo afl build
    cd trame-fuzz && cargo afl fuzz -i in -o out ./target/debug/trame-afl

# Run proofs via soteria-rust. Use `--filter substring` to run only some tests
prove-one crate *args:
    soteria-rust cargo {{ crate }} --kani --summary --stats stdout --flamegraph-dir .soteria {{ args }}
