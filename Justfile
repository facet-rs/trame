list:
    just --list

all:
    just kani
    just test

kani:
    just kani-one trame-runtime
    just kani-one trame

prove:
    cargo creusot prove

prop:
    cd trame-proptest && cargo nextest run

fuzz:
    cd trame-fuzz && cargo afl build
    cd trame-fuzz && cargo afl fuzz -i in -o out ./target/debug/trame-afl

# Test kani proofs via soteria-rust. Use `--filter substring` to run only some tests
kani-one crate *args:
    soteria-rust cargo {{ crate }} --kani --summary --stats stdout --flamegraph-dir .soteria {{ args }}
