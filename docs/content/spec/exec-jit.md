+++
title = "Execution Engine and JIT"
insert_anchor_links = "heading"
weight = 50
+++

# Execution Engine and JIT

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

> t[format.exec.state-model-shared] Interpreter and JIT MUST implement the same
> logical VM state model (pc, call stack, path stack, token register, and
> source/sink state handles).

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

> t[format.exec.deopt-state-transfer] Deoptimization MUST transfer full VM state
> required for semantic equivalence at resume.

> t[format.exec.deopt-state-parity] Deopt snapshot fields MUST be sufficient to
> reconstruct the VM state model defined by VM IR execution semantics.

### Deopt Snapshot (Canonical)

Deopt transfer state uses a versioned snapshot.

```lisp
(deopt-v1
  (pc (proc f0) (block b3) (ip 2))
  (call-stack ((proc f0) (block b1) (ip 5)))
  (path-stack ((field 1)))
  (token-reg (valid true) (kind key) (payload "name"))
  (cand-mask #x03)
  (locals (opaque ...))
  (source-cursor (opaque ...))
  (source-save-stack ((opaque-save-point ...)))
  (sink-state (opaque ...))
  (scratch (opaque ...))
  (runtime
    (shape-id 42)
    (mode strict|deferred)))
```

> t[format.exec.deopt-layout-versioned] Deopt snapshot layout MUST be versioned
> and ABI-coupled.

> t[format.exec.deopt-pc-precise] Snapshot `pc` MUST identify the exact
> interpreter resume instruction boundary.

> t[format.exec.deopt-stack-complete] Snapshot MUST include complete call-stack
> and path-stack state.

> t[format.exec.deopt-token-state-complete] Snapshot MUST include token-register
> validity and payload reference/state required by resume.

> t[format.exec.deopt-candidate-state-complete] Snapshot MUST include candidate
> disambiguation state (candidate mask and related decode locals) when used.

> t[format.exec.deopt-save-stack-complete] Snapshot MUST include source
> save-stack state when replay/savepoints are active.

> t[format.exec.deopt-locals-complete] Snapshot MUST include VM locals/registers
> required by resumed instructions.

> t[format.exec.deopt-sink-state-complete] Snapshot MUST include sink state when
> encode side effects are resumable at the deopt boundary.

> t[format.exec.deopt-cursor-replay-safe] Snapshot MUST include source-cursor
> state sufficient to continue token consumption without duplication or loss.

### Deopt Protocol

1. Detect guard failure or unsupported fast-path condition.
2. Materialize a `deopt-v1` snapshot.
3. Transfer control to interpreter resume entry.
4. Continue execution at snapshot `pc`.

> t[format.exec.deopt-no-duplicate-side-effects] Deopt points MUST occur only at
> instruction boundaries that prevent duplicating already-observed side effects.

> t[format.exec.deopt-no-lost-side-effects] Deopt points MUST preserve all
> side effects that were committed before snapshot materialization.

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

> t[format.exec.abi-source-context] Decode execution entry MUST accept
> mutable source context for token operations.

> t[format.exec.abi-sink-context] Execution entry MUST accept mutable sink
> context/writer state.

> t[format.exec.abi-scratch-context] Execution entry MUST accept scratch context
> for temporary owned fallbacks when needed.

## Backends and Platforms

> t[format.exec.backend-interface-stable] Runtime MUST expose a stable backend
> interface so interpreter and JIT backends are swappable without IR changes.

> t[format.exec.backend-v0-dynasmrt] v0 JIT backend SHOULD use `dynasmrt`.

> t[format.exec.backend-target-guarded] Backend-generated code MUST guard on
> target architecture and calling-convention assumptions used at compile time.

## v0 Bring-Up Gates

v0 implementation order is constrained:

1. VM verifier and loader for text/binary `Program`.
2. Interpreter coverage for all ABI-v1 opcodes used by target workloads.
3. Decode source adapter (`source-v1`) with save/restore and skip support.
4. Differential validation (interpreter vs reference backend) over corpus/fuzz.
5. Baseline JIT enablement behind hotness thresholds.

> t[format.exec.v0-interpreter-before-jit] JIT enablement MUST NOT be the first
> executable path; interpreter support MUST exist first for all targeted opcodes.

> t[format.exec.v0-json-source-before-jit] Fully JIT-targeted JSON decode MUST
> have a conforming `source-v1` adapter (including save/restore) before JIT is
> considered complete.

> t[format.exec.v0-diff-gate-before-default-jit] JIT MUST NOT become default for
> a workload class until differential tests for that class are passing.

### Fully JIT'd JSON (Definition)

For v0, "fully JIT'd JSON decode" means:

- hot JSON decode programs execute entirely in JIT code on steady-state paths;
- interpreter execution is entered only for guarded deopt/error cases;
- decode semantics remain equivalent to interpreter + `op-bridge` reference
  behavior.

> t[format.exec.v0-fully-jitted-json-definition] "Fully JIT'd JSON decode" MUST
> satisfy the definition above.

> t[format.exec.v0-fully-jitted-json-no-hidden-interpreter] Steady-state JIT
> decode paths MUST NOT perform hidden per-token fallbacks into interpreter.

## Testing and Hardening

### Differential Testing

> t[format.exec.test-interpreter-jit-diff] Test suites MUST include
> interpreter-vs-JIT differential checks over the same program/input corpus.

> t[format.exec.test-deopt-coverage] Test suites MUST cover deopt transitions
> from each guard class and verify resumed equivalence.

### Robustness

> t[format.exec.hardening-fuzz] Fuzzing MUST exercise interpreter, JIT, and
> deopt transitions on malformed and adversarial inputs.

> t[format.exec.hardening-no-rwx] JIT allocation policy MUST enforce W^X (no
> simultaneously writable and executable pages).

> t[format.exec.hardening-bounds-checks] Generated code MUST preserve required
> bounds/null checks unless proven redundant by verified analysis.

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
