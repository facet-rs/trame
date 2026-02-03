list:
    just --list

all:
    just kani
    just test

prove:
    just prove-one trame-runtime
    just prove-one trame

# Run proofs via soteria-rust. Use `--filter substring` to run only some tests
prove-one crate *args:
    soteria-rust cargo {{ crate }} --kani --summary --stats stdout --flamegraph-dir .soteria {{ args }}
