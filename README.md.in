# trame

A formally verified partial value constructor for [facet](https://github.com/facet-rs/facet).

Trame manages incremental construction of values, tracking which fields have
been initialized and ensuring proper cleanup on failure. It's designed to be
verified from the ground up.

## Verification Strategy

Trame uses a layered verification approach combining three complementary techniques:

| Approach | Coverage | Speed | Annotation Cost | What it catches |
|----------|----------|-------|-----------------|-----------------|
| Kani + VRuntime | Exhaustive within bounds | Slow (exponential) | None | Logic bugs, invariant violations |
| Fuzzing + LRuntime | Statistical | Fast | None | Memory bugs, crashes, UB |
| VeriFast + LRuntime | Unbounded/Universal | Fast (per-function) | High | Everything, with proof |

### Kani + VRuntime (Bounded Model Checking)

The codebase is generic over an `IRuntime` trait with two implementations:

- **VRuntime** (Verified Runtime) - Bounded state for Kani proofs. Uses fixed-size
  arrays, fat pointers with allocation tracking, and explicit byte-range initialization
  tracking. No real memory allocation.
- **LRuntime** (Live Runtime) - Real memory operations for production use.

Kani exhaustively explores all execution paths within bounds (e.g., up to 8 fields,
2-3 operations). This catches logic bugs fast with zero annotation overhead.

Run proofs with `just kani` in the `trame/` directory.

### Fuzzing + LRuntime

Property-based testing and fuzzing exercise the real implementation with random
inputs, catching memory safety issues and crashes that only manifest with actual
allocations.

### VeriFast + LRuntime (Inductive Verification)

[VeriFast](https://github.com/verifast/verifast) provides separation logic-based
verification that proves properties hold for *all* inputs, not just those within
Kani's bounds. This requires annotating the code with predicates, pre/post conditions,
and lemmas, but provides universal guarantees.

The predicates formalize invariants already encoded in VRuntime's design:
- Node completeness (all fields initialized or have complete children)
- Depth consistency (cursor distance from root)
- Cleanup safety (children dropped before parents)

## Spec

Trame has a work-in-progress spec you can visualize via the `tracey web` subcommand,
see [tracey](https://github.com/bearcove/tracey)

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.
