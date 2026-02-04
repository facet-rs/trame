# trame

A formally verified partial value constructor for [facet](https://github.com/facet-rs/facet).

Trame manages incremental construction of values, tracking which fields have
been initialized and ensuring proper cleanup on failure. It's designed to be
verified from the ground up.

## Verification Strategy

Trame uses a layered verification approach combining four complementary techniques:

| Approach | Coverage | Speed | Annotation Cost | What it catches |
|----------|----------|-------|-----------------|-----------------|
| [Kani](https://github.com/model-checking/kani) + VRuntime | Exhaustive within bounds | Slow (exponential) | None | Logic bugs, invariant violations |
| [Proptest](https://github.com/proptest-rs/proptest) + VRuntime | Statistical | Fast | None | Logic bugs via random exploration |
| [AFL](https://github.com/rust-fuzz/afl.rs) + LRuntime | Statistical | Fast | None | Memory bugs, crashes, UB |
| [Creusot](https://github.com/creusot-rs/creusot) + CRuntime | Unbounded/Universal | Fast (per-function) | High | Everything, with proof |

### Runtimes

The codebase is generic over an `IRuntime` trait with three implementations:

- **VRuntime** (Verified Runtime) - Bounded state for Kani proofs and proptest. Uses
  fixed-size arrays, fat pointers with allocation tracking, and explicit byte-range
  initialization tracking. No real memory allocation.
- **LRuntime** (Live Runtime) - Real memory operations for production use and fuzzing.
- **CRuntime** (Creusot Runtime) - Logic-level runtime for deductive verification
  via Creusot.

### Kani + VRuntime (Bounded Model Checking)

[Kani](https://github.com/model-checking/kani) exhaustively explores all execution
paths within bounds (e.g., up to 8 fields, 2-3 operations). This catches logic bugs
fast with zero annotation overhead.

Proofs are run via [soteria-rust](https://github.com/soteria-tools/soteria), a
Kani-compatible symbolic execution engine: `just kani` in the `trame/` directory.

### Proptest + VRuntime (Property-Based Testing)

[Proptest](https://github.com/proptest-rs/proptest) generates random shapes and
operation sequences to exercise VRuntime statistically. This catches logic bugs that
fall outside Kani's bounded state space, with no annotation overhead.

Run with `cargo nextest run` (the `trame-proptest` crate).

### AFL + LRuntime (Fuzzing)

[AFL](https://github.com/rust-fuzz/afl.rs) fuzzing exercises LRuntime with real
memory allocations, catching memory safety issues and crashes that only manifest
with actual allocations.

Run with `just fuzz`.

### Creusot + CRuntime (Deductive Verification)

[Creusot](https://github.com/creusot-rs/creusot) translates Rust code to
[Why3](https://www.why3.org/) for SMT-based proof discharge. This proves properties
hold for *all* inputs, not just those within Kani's bounds. Requires contract
annotations (pre/post conditions, invariants, logic functions) but provides universal
guarantees.

Run proofs with `just prove`.

## Spec

Trame has a work-in-progress spec you can visualize via the `tracey web` subcommand,
see [tracey](https://github.com/bearcove/tracey)


## Sponsors

CI runs on [Depot](https://depot.dev/) runners. Thanks to Depot for the sponsorship!

## License

Licensed under either of Apache License, Version 2.0 or MIT license at your option.
