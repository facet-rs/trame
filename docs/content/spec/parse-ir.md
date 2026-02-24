+++
title = "Parse and Build IR"
insert_anchor_links = "heading"
weight = 40
+++

This document specifies the write-side IR architecture used to decode input
formats and build values through trame operations.

## Scope

This section defines:

- parse-side format IR (`ParseProgram`)
- value build IR (`BuildProgram`)
- fused decode/build execution behavior

## Model

Deserialization is split into phases:

1. compile decode semantics into `ParseProgram` (format-owned)
2. compile build semantics into `BuildProgram` (value/runtime-owned)
3. optionally fuse into one executable program
4. execute through engine; all writes happen under trame safety rules

## Parse Program

`ParseProgram` describes format token handling, structural expectations, and
decode-side branching.

> t[format.parse.program-profile-bound] A `ParseProgram` MUST be bound to one
> format profile and MUST NOT be reused across incompatible profiles.

> t[format.parse.program-abi-version] A `ParseProgram` MUST carry an ABI
> version and MUST be rejected by incompatible executors.

## Build Program

`BuildProgram` describes value construction semantics in terms compatible with
trame state transitions.

> t[format.parse.build-ops-equivalence] `BuildProgram` semantics MUST be
> equivalent to executing the corresponding `Op` sequence on `Trame`.

> t[format.parse.build-state-machine-preserved] Build execution MUST preserve
> existing trame state-machine rules (strict/deferred behavior, overwrite,
> container materialization, cleanup semantics).

## Fusion

Fusion is an optimization, not a semantic change.

> t[format.parse.fusion-semantic-equivalence] Fused decode/build execution MUST
> be semantically equivalent to non-fused parse-then-build execution.

> t[format.parse.fusion-optional] Fusion MAY be unavailable; execution MUST
> remain correct via unfused interpreter path.

## Safety

> t[format.parse.unsafe-contained] Unsafe writes and pointer arithmetic MUST be
> contained to the execution engine/runtime boundary.

> t[format.parse.no-direct-pointer-write-in-format] Format crates MUST NOT
> perform direct raw-pointer writes to destination values.

> t[format.parse.apply-through-runtime] Materialization effects MUST flow
> through runtime/build semantics, not ad hoc writes in format backends.

## Error Semantics

> t[format.parse.error-token-context] Decode errors MUST include source token
> context when available.

> t[format.parse.error-build-context] Build failures MUST include value-path
> context when available.

> t[format.parse.error-stop-on-failure] Execution MUST stop on first
> unrecoverable decode/build failure and MUST NOT continue mutating state.

## Performance

> t[format.parse.batch-friendly-execution] Engine execution MUST support
> batching/chunked processing to reduce per-token/per-op overhead.

> t[format.parse.hotpath-jit-eligibility] Hot fused decode/build programs MAY be
> JIT-compiled under the same safety and equivalence constraints as read-side
> execution.
