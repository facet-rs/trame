+++
title = "trame"
description = "Safe, verified partial construction of Rust values"
+++

trame is a library for safe partial construction of Rust values using facet reflection.

## Quick Links

- [Specification](/spec/) - Full specification document
- [GitHub](https://github.com/bearcove/trame)

## Overview

By combining facet reflection with low-level memory operations, trame makes incremental construction of arbitrary Rust values possible without causing UB or soundness issues.

The API surface is intentionally small and designed for verification.

## Verification Strategy

<p class="disclaimer">✝︎ Formal verification is a work in progress. We use multiple complementary techniques to build confidence in correctness.</p>

Trame uses a layered verification approach. The codebase is generic over an `IRuntime` trait, letting the same construction logic run against different backends:

- **LRuntime** (Live Runtime) — Real memory operations. This is what ships in production.
- **VRuntime** (Verified Runtime) — Bounded state with fixed-size arrays, fat pointers, and explicit byte-range initialization tracking. No real memory allocation.
- **CRuntime** (Creusot Runtime) — Logic-level runtime for deductive verification.

Each verification technique targets a specific runtime:

| Approach | Runtime | Coverage | What it catches |
|----------|---------|----------|-----------------|
| [Proptest](https://github.com/proptest-rs/proptest) | Verified | Statistical | Logic bugs via random exploration |
| [afl.rs](https://github.com/rust-fuzz/afl.rs) | Live | Statistical | Memory bugs, crashes, UB |
| [Soteria Rust](https://github.com/soteria-tools/soteria) | Verified | Exhaustive within bounds | Logic bugs, invariant violations |
| [Creusot](https://github.com/creusot-rs/creusot) | Creusot (for now) | Universal | Everything, with proof |

### Fuzzing and Property Testing

[Proptest](https://github.com/proptest-rs/proptest) generates random shapes and
operation sequences to exercise VRuntime statistically.

[afl.rs](https://github.com/rust-fuzz/afl.rs) fuzzing exercises LRuntime with a
static set of test shapes, and real memory allocations. Together they catch
logic bugs, memory safety issues, and crashes through random exploration—with
zero annotation overhead.

### Kani (Bounded Symbolic Execution)

[Soteria Rust](https://github.com/soteria-tools/soteria) performs symbolic
execution to prove properties exhaustively within bounds. It's compatible with
[Kani](https://github.com/model-checking/kani)-style proofs, which we use to
verify the code. Unlike fuzzing, it doesn't sample—it proves no bugs exist
within the bounded state space.

Unfortunately, bounds must be kept fairly small (e.g., up to 8 fields, 2-3
operations) to avoid runtime explosion.

### Creusot (Deductive Verification)

[Creusot](https://github.com/creusot-rs/creusot) translates Rust code to
[Why3](https://www.why3.org/) for SMT-based proof discharge. This is inductive
verification: it proves properties hold for *all* inputs, not just those within
Kani's bounds. Requires contract annotations (preconditions, postconditions,
invariants, logic functions) but provides universal guarantees.
