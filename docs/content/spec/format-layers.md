+++
title = "Typed Decode Layer Contracts"
insert_anchor_links = "heading"
weight = 40
+++

# Typed Layered Decode IR (Shape + Format in One Program)

This document defines decode-layer architecture boundaries between:

- shape-driven semantic lowering
- format-driven lexical/stateful lowering
- the shared executable IR contract
- execution backends (interpreter and JIT)

It is a review-first architecture draft. Existing behavior in
`vm-ir.md` and `exec-jit.md` remains authoritative until this contract is
adopted.

## Scope and Non-Goals

This section governs decode architecture boundaries only.

It does not define:

- final textual syntax for every IR addition
- target-specific machine-code details
- migration sequencing for existing experimental crates

Current `trame-json` and `trame-postcard` split APIs are considered
experimental and non-normative for the long-term architecture.

## Layer Responsibilities

Shape and format responsibilities are separate but compile into one executable
program.

> t[format.layers.shape-owns-semantic-routing] Shape lowering MUST own semantic
> decode/build routing (for example: required/default handling, flatten, enum
> tagging strategy, untagged candidate selection, and destination path writes).

> t[format.layers.format-owns-lexical-state] Format lowering MUST own lexical
> and syntax-state transitions (for example: delimiter handling, keyed vs
> ordered field discovery, indentation/table navigation, anchor/alias
> resolution).

> t[format.layers.shape-format-shared-isa] Shape and format lowering MUST target
> one shared executable decode ISA within one `Program`.

> t[format.layers.no-rust-helper-semantics] Decode parse semantics MUST NOT
> depend on host-language helper objects/functions that are outside the IR
> semantic model.

## Program Model

> t[format.layers.single-program-holistic] Each decode entrypoint MUST compile
> to one holistic decode program containing all required shape and format
> control flow.

> t[format.layers.no-wrapper-element-model] Decode correctness MUST NOT rely on
> an element-program-plus-wrapper execution model.

> t[format.layers.root-shape-any] Decode program generation MUST support any
> valid root shape category (including scalar, struct, enum, list, map, set,
> and pointer-capable roots), not only struct roots.

The holistic program model exists to make flatten/untagged/replay/global state
explicit in one CFG.

## Typed IR State Model

The decode ISA is extended with an explicit `state` section that declares all
persistent parser/build state used by program procedures.

Canonical conceptual shape:

```lisp
(vmir
  (abi 1)
  (kind decode)
  (shape-id 123)
  (state
    (slots
      (s0 (type u32))
      (s1 (type bool))
      (s2 (type (array u8 32)))
      (s3 (type (stack u32 64)))
      (s4 (type (ring (record ((k u32) (v u32))) 128)))
      (s5 (type (bitset 256)))
      (s6 (type (table key-u32 value-u32 cap-512)))
      (s7 (type (record ((cursor u32) (depth u16) (flags u16)))))))
  (code ...))
```

> t[format.layers.state-section-required] Decode programs that require
> cross-procedure parser/build state MUST declare that state in an explicit IR
> `state` section.

> t[format.layers.state-types-closed-v1] ABI v1 state slot kinds MUST be
> limited to a closed, verifier-known set (fixed-width scalars/booleans, fixed
> arrays, bounded stacks, bounded rings/queues, bitsets, records, and bounded
> tables/maps).

> t[format.layers.state-layout-static] All declared state slots MUST have static
> layout/size determined at verify time.

> t[format.layers.state-bounds-verified] Verifier/runtime MUST reject programs
> where state-slot accesses can violate declared bounds/capacities.

No host-opaque runtime parser struct is allowed to be the semantic source of
truth for parse state. Any state needed for correctness must be represented in
IR state slots.

## Shape Code Contract

Shape lowering is compile-time and queries shape abstractions (`IShape` family
and related interfaces).

Shape code may:

- read/write typed IR state slots
- branch on parse outcomes and shape metadata
- emit build/navigation actions

Shape code must not:

- own lexical tokenization rules
- hide candidate routing state outside IR

> t[format.layers.flatten-untagged-state-explicit] Flatten/untagged candidate
> selection state (for example masks, candidate indices, replay markers) MUST be
> represented in explicit IR state/registers.

## Format Code Contract

Format code is also compiled into IR procedures in the same decode ISA.

Format procedures may implement:

- token boundaries and delimiters
- keyed/ordered field transitions
- sequence/map framing
- format-specific state machines

### Stateful Format Requirements

TOML-like reopening and deferred navigation state must be explicit in IR state.

> t[format.layers.toml-reopen-state-explicit] TOML-style table reopening and
> pending navigation state MUST be represented by explicit IR state slots (for
> example path stacks, pending-event queues, reopen bookkeeping).

YAML-like anchor/alias bookkeeping must be explicit in IR state.

> t[format.layers.yaml-anchor-state-explicit] YAML-style anchor/alias resolution
> state MUST be represented by explicit IR state slots (for example anchor
> tables and alias worklists).

## Call ABI Between Shape and Format Procedures

Shape and format procedures communicate through explicit call boundaries in IR.

Canonical conceptual proc form:

```lisp
(proc p17
  (sig
    (params (r0:u32 r1:u32))
    (returns (r0:u32))
    (clobbers (r2 r3 r4))
    (preserves (r5 r6))
    (errors (decode-eof decode-invalid-token decode-no-match)))
  (body ...))
```

> t[format.layers.call-signature-explicit] Every callable decode procedure MUST
> declare a typed call signature (params and returns).

> t[format.layers.call-clobbers-explicit] Every callable decode procedure MUST
> explicitly declare register/state clobber behavior.

> t[format.layers.call-preserve-contract-explicit] Every callable decode
> procedure MUST explicitly declare preserved registers/state across call
> boundaries.

> t[format.layers.call-error-channel-explicit] Every callable decode procedure
> MUST expose a typed error/result channel in its ABI contract.

Unbounded recursion is disallowed in v1; lowering must use bounded/iterative
forms.

## Inlining Policy

Inlining is a backend optimization and must not affect observable semantics.

> t[format.layers.inline-semantic-equivalence] Inlined and non-inlined execution
> of the same decode program MUST be semantically equivalent.

> t[format.layers.inline-boundary-mapping-required] Backends MUST preserve
> debug/deopt mapping information for original call boundaries across inlining.

## Register Allocation Policy

Register allocation is global across merged shape+format control flow.

> t[format.layers.regalloc-global-space] Decode register allocation MUST operate
> on one global virtual register space across shape and format procedures in the
> merged program.

> t[format.layers.regalloc-post-inline] Register allocation MUST run after
> integration/inlining decisions for the compiled decode CFG.

> t[format.layers.regalloc-deterministic] For identical input IR and target
> profile, register allocation decisions MUST be deterministic.

Spills/reloads must preserve typed semantics and declared call-clobber rules.

## Backends and Hidden State

Backends execute the IR contract and cannot introduce hidden parser state that
changes semantics.

> t[format.layers.backend-no-hidden-parser-state] Execution backends MUST NOT
> rely on hidden parser state outside declared IR state/register model.

> t[format.layers.interpreter-jit-equivalence-required] Interpreter and JIT
> backends MUST be semantically equivalent for programs conforming to this
> layer contract.

## Error and Recovery Contract

Decode errors are typed and flow through declared procedure error channels.

Required model:

- typed error classes (format + semantic)
- explicit rollback/poison behavior for state slots that require it
- replay/save-restore based on declared state slots, not host-opaque objects

## Conformance Matrix (Minimum)

Implementations claiming conformance to this layer contract must demonstrate
all scenarios below against interpreter and JIT:

1. keyed self-describing decode with flatten/default handling.
2. ordered hinted decode with length-prefixed sequence framing.
3. untagged enum candidate-mask + replay flow in one holistic program.
4. TOML reopening with interleaved table navigation and deferred pending queues.
5. YAML anchors/aliases with successful resolution and explicit failure paths.

These scenarios are conformance minimums for this document and are additive to
existing VM IR and execution conformance requirements.

## Review Checklist

Use this checklist when reviewing implementations against this spec:

1. No correctness rule depends on external Rust helper parser objects.
2. All correctness-critical parse/build state is declared in IR `state`.
3. Shape and format procedures both use shared ISA and explicit call ABI.
4. Wrapper element-program model is absent from semantics.
5. Global post-inline deterministic register allocation is enforceable.

