+++
title = "VM IR"
insert_anchor_links = "heading"
weight = 30
+++

# VM IR

This document specifies the generic format encoding/decoding VM IR boundary
between `facet-format` and `trame`.

## Scope

This section defines:

- the shared IR dialect constraints for all formats
- encoding-side IR contract (`SerProgram`)
- decoding-side IR contract (`ParseProgram` + `BuildProgram`)
- ownership split between compiler, executor, and sinks

Machine-code generation is specified in `exec-jit.md`.

## Generic VM Requirement

All formats target one VM dialect.

> t[format.vm.single-vm-dialect] All supported formats MUST compile to one
> shared IR instruction dialect for encoding and decoding.

> t[format.vm.no-format-specific-ir] The system MUST NOT define per-format IR
> dialects.

> t[format.vm.format-is-compile-time-input] Format differences MUST be
> represented as compile-time lowering choices into the shared dialect.

> t[format.vm.delimiters-are-ir] Structural delimiters/separators (object,
> array, map, sequence boundaries) MUST be represented in IR control/data flow.

> t[format.vm.builtins-extension-point] Format-specific behavior MAY be exposed
> only through a constrained builtin set callable from the shared IR.

## Encoding IR (`SerProgram`)

### Model

Serialization is split into two phases:

1. compile (`facet-format`): `Shape` + profile/options -> `SerProgram`
2. execute (`trame`): `(ptr, shape, program)` -> sink events

`SerProgram` is an internal executable IR; it is not a wire format.

### Program Envelope

> t[format.ir.program-root-shape] A `SerProgram` MUST bind to exactly one root
> shape identity (`shape.id`) at compile time.

> t[format.ir.program-abi-version] A `SerProgram` MUST carry an explicit ABI
> version and MUST be rejected by executors with a mismatched ABI version.

> t[format.ir.program-profile] A `SerProgram` MUST encode exactly one format
> profile and execution MUST NOT reinterpret a program under another profile.

> t[format.ir.program-immutable] A compiled `SerProgram` MUST be immutable
> during execution.

#### Informative Layout

```rust
pub struct SerProgram {
    pub abi_version: u16,
    pub profile: SerProfile,
    pub root_shape_id: ConstTypeId,
    pub flags: SerProgramFlags,
    pub strings: Vec<StringAtom>,
    pub predicates: Vec<PredicateSpec>,
    pub field_plans: Vec<FieldPlan>,
    pub blocks: Vec<SerBlock>,
    pub entry_block: BlockId,
}
```

### Compiler Ownership

`facet-format` owns policy lowering into IR.

> t[format.ir.compile-policy-owner] Policy lowering (`skip`, rename, flatten,
> tagging templates, key policy) MUST happen in compile phase, not in sinks.

> t[format.ir.compile-static-skip] Fields marked with unconditional skip MUST be
> removed from emitted field plans during compile phase.

> t[format.ir.compile-rename-resolution] Effective field/variant names MUST be
> resolved during compile phase and stored in program string tables.

> t[format.ir.compile-flatten-lowering] Flattened fields MUST be lowered into
> explicit field-plan entries that define required runtime traversal.

> t[format.ir.compile-predicate-binding] Dynamic skip predicates (for example
> `skip_serializing_if`) MUST be represented as predicate table entries.

> t[format.ir.compile-profile-selection] Compile phase MUST encode
> profile-specific behavior explicitly in instructions or plan entries.

> t[format.ir.compile-validate-block-targets] Compile phase MUST reject programs
> with dangling block references.

> t[format.ir.compile-validate-indices] Compile phase MUST reject programs with
> invalid field or variant indexes for the bound shape.

### Executor Ownership

`trame` owns execution, pointer reads, and sink delivery.

> t[format.ir.exec-shape-guard] Executor MUST validate runtime shape id against
> `program.root_shape_id` before executing any instruction.

> t[format.ir.exec-entry-block] Execution MUST start at `entry_block`.

> t[format.ir.exec-stack-balance] Navigation instructions (`Enter*` / `Leave`)
> MUST preserve stack correctness; leaving root scope MUST be rejected.

> t[format.ir.exec-pointer-reads-in-engine] All pointer dereferences and pointer
> arithmetic MUST occur inside the executor.

> t[format.ir.exec-no-raw-pointer-to-sink] Executor MUST NOT expose raw pointers
> to sinks.

> t[format.ir.exec-safe-scalar-interface] Scalars delivered to sinks MUST be
> safe value views/copies, not raw memory addresses.

> t[format.ir.exec-predicate-in-engine] Predicate evaluation MUST execute inside
> the executor/runtime boundary and only affect control flow.

> t[format.ir.exec-control-flow-only-from-ir] Runtime traversal control flow
> MUST follow explicit IR instructions.

### Field Plans

> t[format.ir.field-plan-emit-entry] `EmitField` entries MUST reference a valid
> field index and output key/name identifier.

> t[format.ir.field-plan-predicate-optional] Field plan entries MAY carry an
> optional predicate id; if present, execution MUST evaluate it before emit.

> t[format.ir.field-plan-flatten-struct] `FlattenStruct` entries MUST reference
> a nested field plan valid for the referenced field's effective shape.

> t[format.ir.field-plan-flatten-map] `FlattenMap` entries MUST produce map
> entry emission semantics equivalent to current format behavior.

> t[format.ir.field-plan-flatten-enum] `FlattenEnum` entries MUST preserve the
> same visibility/tagging behavior as non-IR serializer semantics.

### Enum and Tagging

> t[format.ir.enum-active-variant] Executor MUST resolve active variant at
> runtime before executing variant-dependent branches.

> t[format.ir.enum-tagging-template] Tagging strategy selection MUST be encoded
> in the program at compile time.

> t[format.ir.enum-payload-control-flow] Payload emission for each variant MUST
> be represented by explicit control-flow targets.

### Sink Contract

> t[format.ir.sink-struct-balance] Every emitted structural `begin_*` event MUST
> have a corresponding `end` in well-formed execution.

> t[format.ir.sink-field-name-order] For object/struct-like emission, field-name
> events MUST be emitted immediately before their value emission.

> t[format.ir.sink-error-propagation] Sink errors MUST terminate execution and
> be surfaced as executor errors.

### Error Reporting

> t[format.ir.error-carries-pc] Execution errors MUST include block id and
> instruction index at failure point.

> t[format.ir.error-carries-path] Execution errors MUST include current logical
> traversal path when available.

> t[format.ir.error-predicate-context] Predicate failures MUST identify
> predicate id and triggering field/entry context.

### Caching

> t[format.ir.cache-key-shape-profile-options] Program caches MUST key at least
> by `(shape.id, profile, compile options, abi_version)`.

> t[format.ir.cache-in-process-v0] v0 caches MUST be in-process only.

> t[format.ir.cache-abi-invalidation] Cache entries MUST be invalidated on ABI
> version mismatch.

## Decoding IR (`ParseProgram` + `BuildProgram`)

### Model

Deserialization is split into phases:

1. compile decode semantics into `ParseProgram` (format-owned)
2. compile build semantics into `BuildProgram` (value/runtime-owned)
3. optionally fuse into one executable program
4. execute through engine; all writes happen under trame safety rules

### Parse Program

`ParseProgram` describes token handling, structural expectations, and decode
branching.

> t[format.parse.program-profile-bound] A `ParseProgram` MUST be bound to one
> format profile and MUST NOT be reused across incompatible profiles.

> t[format.parse.program-abi-version] A `ParseProgram` MUST carry an ABI
> version and MUST be rejected by incompatible executors.

### Build Program

`BuildProgram` describes value construction semantics compatible with trame
state transitions.

> t[format.parse.build-ops-equivalence] `BuildProgram` semantics MUST be
> equivalent to executing the corresponding `Op` sequence on `Trame`.

> t[format.parse.build-state-machine-preserved] Build execution MUST preserve
> trame state-machine rules (strict/deferred behavior, overwrite, container
> materialization, cleanup semantics).

### Fusion

> t[format.parse.fusion-semantic-equivalence] Fused decode/build execution MUST
> be semantically equivalent to non-fused parse-then-build execution.

> t[format.parse.fusion-optional] Fusion MAY be unavailable; execution MUST
> remain correct through unfused interpreter path.

### Safety

> t[format.parse.unsafe-contained] Unsafe writes and pointer arithmetic MUST be
> contained to the execution engine/runtime boundary.

> t[format.parse.no-direct-pointer-write-in-format] Format crates MUST NOT
> perform direct raw-pointer writes to destination values.

> t[format.parse.apply-through-runtime] Materialization effects MUST flow
> through runtime/build semantics, not ad hoc writes in format backends.

### Error Semantics

> t[format.parse.error-token-context] Decode errors MUST include source token
> context when available.

> t[format.parse.error-build-context] Build failures MUST include value-path
> context when available.

> t[format.parse.error-stop-on-failure] Execution MUST stop on first
> unrecoverable decode/build failure and MUST NOT continue mutating state.

### Performance

> t[format.parse.batch-friendly-execution] Engine execution MUST support
> batching/chunked processing to reduce per-token/per-op overhead.

> t[format.parse.hotpath-jit-eligibility] Hot fused decode/build programs MAY be
> JIT-compiled under the same safety and equivalence constraints as read-side
> execution.
