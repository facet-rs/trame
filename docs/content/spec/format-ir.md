+++
title = "Format IR"
insert_anchor_links = "heading"
weight = 30
+++

This document specifies the serialization IR boundary between `facet-format`
and `trame`.

## Scope

This section defines:

- the shape of `SerProgram`
- compiler responsibilities (policy lowering)
- executor responsibilities (safe value reads + sink emission)

This section does not define machine-code JIT behavior. See `exec-jit.md`.

## Model

Serialization is split into two phases:

1. compile (`facet-format`): `Shape` + profile/options -> `SerProgram`
2. execute (`trame`): `(ptr, shape, program)` -> sink events

`SerProgram` is an internal executable IR; it is not a wire format.

## Conventions

This section uses **MUST** for normative requirements.
Normative requirements are expressed as `t[...]` rules.

## Program Envelope

> t[format.ir.program-root-shape] A `SerProgram` MUST bind to exactly one root
> shape identity (`shape.id`) at compile time.

> t[format.ir.program-abi-version] A `SerProgram` MUST carry an explicit ABI
> version and MUST be rejected by executors with a mismatched ABI version.

> t[format.ir.program-profile] A `SerProgram` MUST encode exactly one format
> profile (for example JSON or postcard) and execution MUST NOT reinterpret a
> program under a different profile.

> t[format.ir.program-immutable] A compiled `SerProgram` MUST be immutable
> during execution.

### Informative Layout

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

## Compiler Ownership

`facet-format` owns policy lowering into IR.

> t[format.ir.compile-policy-owner] Policy lowering (`skip`, rename, flatten,
> tagging templates, key policy) MUST happen in compile phase, not in sinks.

> t[format.ir.compile-static-skip] Fields marked with unconditional skip MUST be
> removed from emitted field plans during compile phase.

> t[format.ir.compile-rename-resolution] Effective field/variant names MUST be
> resolved during compile phase and stored in program string tables.

> t[format.ir.compile-flatten-lowering] Flattened fields MUST be lowered into
> explicit field-plan entries that define the required runtime traversal.

> t[format.ir.compile-predicate-binding] Dynamic skip predicates (for example
> `skip_serializing_if`) MUST be represented as predicate table entries in the
> program, not as sink callbacks.

> t[format.ir.compile-profile-selection] Compile phase MUST encode
> profile-specific behavior (human-readable vs binary semantics) explicitly in
> instructions or plan entries.

> t[format.ir.compile-validate-block-targets] Compile phase MUST reject programs
> with dangling block references.

> t[format.ir.compile-validate-indices] Compile phase MUST reject programs with
> invalid field or variant indexes for the bound shape.

## Executor Ownership

`trame` owns execution, pointer reads, and sink delivery.

> t[format.ir.exec-shape-guard] Executor MUST validate that runtime shape id
> matches `program.root_shape_id` before executing any instruction.

> t[format.ir.exec-entry-block] Execution MUST start at `entry_block`.

> t[format.ir.exec-stack-balance] Navigation instructions (`Enter*` / `Leave`)
> MUST preserve stack correctness; leaving root scope MUST be rejected.

> t[format.ir.exec-pointer-reads-in-engine] All pointer dereferences and pointer
> arithmetic MUST occur inside the executor.

> t[format.ir.exec-no-raw-pointer-to-sink] Executor MUST NOT expose raw pointers
> to sinks.

> t[format.ir.exec-safe-scalar-interface] Scalars delivered to sinks MUST be
> passed as safe value views/copies (for example bool/int/str/bytes), not as
> raw memory addresses.

> t[format.ir.exec-predicate-in-engine] Predicate evaluation MUST execute inside
> the executor/runtime boundary and MUST return only control-flow decisions to
> program execution.

> t[format.ir.exec-control-flow-only-from-ir] Runtime control flow MUST follow
> explicit IR instructions (`Jump`, conditional jumps, loops); sink code MUST
> NOT control traversal semantics.

## Field Plans

Field plans encode precomputed policy for struct-like emission.

> t[format.ir.field-plan-emit-entry] `EmitField` entries MUST reference a valid
> field index and an output key/name identifier.

> t[format.ir.field-plan-predicate-optional] Field plan entries MAY carry an
> optional predicate id; if present, execution MUST evaluate predicate before
> emitting that field entry.

> t[format.ir.field-plan-flatten-struct] `FlattenStruct` entries MUST reference
> a nested field plan that is valid for the referenced field's effective shape.

> t[format.ir.field-plan-flatten-map] `FlattenMap` entries MUST produce map
> entry emission semantics equivalent to current format behavior.

> t[format.ir.field-plan-flatten-enum] `FlattenEnum` entries MUST preserve the
> same visibility/tagging behavior as non-IR serializer semantics.

## Enum and Tagging

> t[format.ir.enum-active-variant] Executor MUST resolve active variant at
> runtime before executing variant-dependent branches.

> t[format.ir.enum-tagging-template] Tagging strategy selection (external,
> internal, adjacent, untagged, numeric where applicable) MUST be encoded in the
> program at compile time.

> t[format.ir.enum-payload-control-flow] Payload emission for each variant MUST
> be represented by explicit control-flow targets in the program.

## Sink Contract

> t[format.ir.sink-struct-balance] Every `begin_*` structural sink event emitted
> by executor MUST have a corresponding `end` event in well-formed execution.

> t[format.ir.sink-field-name-order] For object/struct-like emission, field-name
> events MUST be emitted immediately before the field value emission they name.

> t[format.ir.sink-error-propagation] Sink errors MUST terminate execution and
> MUST be surfaced as executor errors without partial success reporting.

## Error Reporting

> t[format.ir.error-carries-pc] Execution errors MUST include block id and
> instruction index (program counter) at failure point.

> t[format.ir.error-carries-path] Execution errors MUST include current logical
> traversal path (field/element breadcrumbs) when available.

> t[format.ir.error-predicate-context] Predicate failures MUST identify predicate
> id and the field/entry context that triggered evaluation.

## Caching

> t[format.ir.cache-key-shape-profile-options] Program caches MUST key at least
> by `(shape.id, profile, compile options, abi_version)`.

> t[format.ir.cache-in-process-v0] v0 caches MUST be treated as in-process only.

> t[format.ir.cache-abi-invalidation] Cache entries MUST be invalidated on ABI
> version mismatch.
