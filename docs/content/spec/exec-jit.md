+++
title = "Execution Engine and JIT"
insert_anchor_links = "heading"
weight = 50
+++

This document specifies execution-tier behavior for serialization and
deserialization program engines.

## Scope

This section defines:

- interpreter requirements
- JIT requirements
- guard/deopt behavior
- safety constraints for generated code

## Tiers

Execution uses tiers:

1. interpreter (reference semantics)
2. baseline JIT (hot-path acceleration)
3. optional optimized JIT (additional specialization)

> t[format.exec.interpreter-is-reference] Interpreter semantics MUST be the
> reference behavior for all programs.

> t[format.exec.jit-semantic-equivalence] JIT execution MUST be semantically
> equivalent to interpreter execution for the same input/program.

> t[format.exec.jit-optional] JIT compilation MAY be unavailable; in that case,
> execution MUST remain fully functional via interpreter.

## Hotness and Promotion

> t[format.exec.hotness-counter] Runtime MUST track execution hotness per
> executable program instance.

> t[format.exec.promotion-threshold] Promotion to JIT MUST only happen after a
> configurable hotness threshold.

> t[format.exec.jit-compile-failure-fallback] If JIT compilation fails,
> execution MUST fall back to interpreter without changing observable semantics.

## Entry Guards

> t[format.exec.guard-shape-id] JIT entry MUST guard on expected root shape id.

> t[format.exec.guard-program-abi] JIT entry MUST guard on expected program ABI
> version.

> t[format.exec.guard-runtime-capabilities] JIT entry MUST guard on required
> runtime capabilities (for example predicate hooks or scalar projection
> support) when those are assumed by generated code.

## Deoptimization

> t[format.exec.deopt-on-guard-fail] Guard failure MUST trigger deoptimization
> to interpreter.

> t[format.exec.deopt-resume-point] Deoptimization MUST resume interpreter at an
> equivalent program point (block + instruction index or stronger equivalent).

> t[format.exec.deopt-state-transfer] Deoptimization MUST transfer sufficient VM
> state (pointer/shape stacks and loop state) to preserve behavior.

## Safety

> t[format.exec.unsafe-contained] Unsafe operations in this architecture MUST be
> contained to the execution engine boundary (interpreter/JIT runtime).

> t[format.exec.no-pointer-leak] Generated code MUST NOT expose raw pointers to
> format sinks or higher-level libraries.

> t[format.exec.bounded-memory-access] Generated reads/writes MUST be bounded by
> prevalidated shape/layout metadata.

> t[format.exec.no-ub-on-unsupported-case] Unsupported runtime cases MUST deopt
> or error; they MUST NOT execute undefined behavior.

## ABI and Function Shape

Implementation may choose a concrete ABI, but it MUST satisfy:

> t[format.exec.abi-root-ptr] Execution entry MUST accept a root value pointer.

> t[format.exec.abi-shape-context] Execution entry MUST accept read-only shape
> context sufficient for safe traversal.

> t[format.exec.abi-sink-context] Execution entry MUST accept mutable sink
> context/writer state.

> t[format.exec.abi-scratch-context] Execution entry MUST accept scratch context
> for temporary owned fallbacks when needed.

## Error Semantics

> t[format.exec.error-propagation] Execution engine MUST propagate sink/runtime
> failures as explicit execution errors.

> t[format.exec.error-context] Execution errors MUST include execution context
> sufficient to diagnose failure (program position and logical path when
> available).

## Caching

> t[format.exec.code-cache-key] Compiled code cache keys MUST include program
> identity and ABI/runtime constraints assumed by generated code.

> t[format.exec.code-cache-in-process-v0] v0 code cache MUST be in-process only.

> t[format.exec.code-cache-invalidate-on-runtime-mismatch] Cached code MUST be
> invalidated when runtime capability assumptions no longer hold.
