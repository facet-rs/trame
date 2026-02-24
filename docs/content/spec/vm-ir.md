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
- unified executable program contract for encode/decode directions
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

## IR Representations

The same program may have:

- a canonical text form (S-expression) for review/snapshots
- a canonical binary form for execution/cache

> t[format.vm.text-form-sexp] The canonical text representation of VM IR MUST be
> S-expression based.

> t[format.vm.binary-form-canonical] The executable representation MUST have a
> canonical binary encoding keyed by ABI version.

> t[format.vm.text-binary-roundtrip] For well-formed programs within the active
> ABI version, text and binary forms MUST round-trip without semantic loss.

### Canonical S-expression Shape

Top-level program root:

```lisp
(vmir
  (abi 1)
  (kind encode|decode)
  (shape-id 1234567890)
  (consts
    (strings (...))
    (predicates (...)))
  (code
    (procs (...))
    (entry-proc f0)))
```

> t[format.vm.sexp-root-tag] Program text form MUST use `vmir` as the root tag.

> t[format.vm.sexp-key-order] Root keys MUST be emitted in canonical order:
> `abi`, `kind`, `shape-id`, `consts`, `code`.

> t[format.vm.execution-profile-free] VM execution semantics MUST NOT depend on a
> profile field in program representation.

> t[format.vm.sexp-canonical-print] Canonical printer output MUST be
> deterministic (byte-identical for semantically identical programs).

### Program Components

`vmir` is logic plus constant tables:

- `strings`: interned UTF-8 literals referenced by code (field names, tag keys,
  fixed symbols).
- `predicates`: symbolic predicate descriptors used by conditional branches.
- `procs`: executable procedures.
- `blocks`: executable basic blocks inside procedures.
- `entry-proc`: procedure label where execution starts.

> t[format.vm.sexp-consts-not-logic] `strings` and `predicates` are constant
> tables and MUST NOT contain executable control-flow instructions.

> t[format.vm.sexp-code-is-logic] Executable logic MUST be represented in
> `code.procs`/`blocks` with explicit control flow.

> t[format.vm.sexp-entry-proc] Executable programs MUST define exactly one
> `entry-proc`.

### Atoms and Literals

> t[format.vm.sexp-symbol-style] Instruction and field symbols MUST use
> lowercase kebab-case.

> t[format.vm.sexp-int-format] Integer literals MUST be base-10 without leading
> plus signs; negative values use leading `-`.

> t[format.vm.sexp-bool-format] Booleans MUST be encoded as `true` or `false`.

> t[format.vm.sexp-string-format] Strings MUST be UTF-8 and escaped with
> JSON-compatible escapes.

> t[format.vm.sexp-bytes-format] Byte blobs MUST be encoded as lowercase hex via
> `#x...` tokens.

### References

> t[format.vm.sexp-block-labels] Block references MUST use canonical `bN`
> labels.

> t[format.vm.sexp-predicate-labels] Predicate references MUST use canonical
> `pN` labels.

> t[format.vm.sexp-proc-labels] Procedure references MUST use canonical `fN`
> labels.

### Comments and Whitespace

> t[format.vm.sexp-parser-whitespace] Parsers MUST accept arbitrary ASCII
> whitespace between forms.

> t[format.vm.sexp-parser-comments] Parsers MAY accept `;` line comments.

> t[format.vm.sexp-canonical-no-comments] Canonical printer output MUST NOT
> include comments.

### Minimal Example

```lisp
(vmir
  (abi 1)
  (kind encode)
  (shape-id 42)
  (consts
    (strings ("id" "name"))
    (predicates ()))
  (code
    (procs
      ((f0
        (entry b0)
        (blocks
          ((b0
            (emit-begin-struct (fields 2))
            (emit-field-name (string 0))
            (enter-field (index 0))
            (emit-scalar)
            (leave)
            (emit-field-name (string 1))
            (enter-field (index 1))
            (emit-scalar)
            (leave)
            (emit-end)
            (ret)))))))
    (entry-proc f0)))
```

## Program Contract

### Model

VM execution is split into two phases:

1. compile (`facet-format`): `Shape` + format + direction + options ->
   executable `Program`
2. execute (`trame`): `(ptr, shape, program, io-context)` where:
   - encode direction emits sink events
   - decode direction consumes source tokens and applies build actions

`Program` is an internal executable IR; it is not a wire format.

> t[format.ir.single-program-type] The architecture MUST define one executable
> `Program` type shared by encode and decode directions.

> t[format.ir.direction-is-compile-input] Direction (`encode` or `decode`) MUST
> be a compile input, not a separate IR type family.

### Program Envelope

> t[format.ir.program-root-shape] A `Program` MUST bind to exactly one root
> shape identity (`shape.id`) at compile time.

> t[format.ir.program-abi-version] A `Program` MUST carry an explicit ABI
> version and MUST be rejected by executors with a mismatched ABI version.

> t[format.ir.program-self-contained] A `Program` MUST be self-contained for
> execution semantics; executor behavior MUST be determined by program contents
> plus ABI/runtime capability checks.

> t[format.ir.program-immutable] A compiled `Program` MUST be immutable
> during execution.

#### Program Layout (S-expression)

```lisp
(vmir
  (abi 1)
  (kind encode)
  (shape-id 1234567890)
  (consts
    (strings (...))
    (predicates (...)))
  (code
    (procs (...))
    (entry-proc f0)))
```

### Compiler Ownership

`facet-format` owns policy lowering into IR.

> t[format.ir.compile-policy-owner] Policy lowering (`skip`, rename, flatten,
> tagging rules, key policy) MUST happen in compile phase, not in sinks.

> t[format.ir.compile-static-skip] Fields marked with unconditional skip MUST be
> removed from emitted executable control flow during compile phase.

> t[format.ir.compile-rename-resolution] Effective field/variant names MUST be
> resolved during compile phase and stored in program string tables.

> t[format.ir.compile-flatten-lowering] Flattened fields MUST be lowered into
> explicit executable control flow that defines required runtime traversal.

> t[format.ir.compile-predicate-binding] Dynamic skip predicates (for example
> `skip_serializing_if`) MUST be represented as predicate table entries.

> t[format.ir.compile-lowering-selection] Compile phase MUST encode lowering
> policy choices explicitly in executable instructions/control flow.

> t[format.ir.compile-validate-block-targets] Compile phase MUST reject programs
> with dangling block references.

> t[format.ir.compile-validate-proc-targets] Compile phase MUST reject programs
> with dangling procedure references.

> t[format.ir.compile-validate-indices] Compile phase MUST reject programs with
> invalid field or variant indexes for the bound shape.

> t[format.ir.compile-validate-candidate-layout] Compile phase MUST validate
> candidate count, candidate-id assignment, and all candidate mask widths.

> t[format.ir.compile-validate-dispatch-cases] Compile phase MUST validate that
> `cand-dispatch` case ids are unique and in range for program candidate count.

> t[format.ir.compile-solver-lowering-owner] Variant-disambiguation logic
> (flatten/untagged/tagged solving) MUST be lowered by the compiler into
> executable decode control flow; it MUST NOT remain an external runtime solve
> step.

> t[format.ir.compile-solver-equivalence] Lowered decode disambiguation MUST be
> semantically equivalent to the candidate-elimination behavior defined by
> facet-solver semantics for the same shape and format policy.

### Executor Ownership

`trame` owns execution, pointer reads, and sink delivery.

> t[format.ir.exec-shape-guard] Executor MUST validate runtime shape id against
> `program.root_shape_id` before executing any instruction.

> t[format.ir.exec-entry-proc] Execution MUST start at `entry-proc`.

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

### Procedures and Loops

> t[format.ir.proc-cfg-only] Executable programs MUST be expressible as
> procedures containing basic blocks and explicit control-flow edges.

> t[format.ir.proc-call-explicit] Procedure calls/returns (if present) MUST be
> explicit instructions in the IR.

> t[format.ir.loop-via-cfg-backedge] Loops MUST be represented by explicit CFG
> back-edges.

> t[format.ir.no-plan-indirection] Executable IR MUST NOT depend on plan-like
> indirection at runtime.

### Enum and Tagging

> t[format.ir.enum-active-variant] Executor MUST resolve active variant at
> runtime before executing variant-dependent branches.

> t[format.ir.enum-tagging-policy] Tagging strategy selection MUST be encoded
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

> t[format.ir.cache-key-shape-options] Program caches MUST key at least
> by `(shape.id, compile options, abi_version)`.

> t[format.ir.cache-in-process-v0] v0 caches MUST be in-process only.

> t[format.ir.cache-abi-invalidation] Cache entries MUST be invalidated on ABI
> version mismatch.

## Decode Direction Requirements

### Model

Decode direction compiles into one executable `Program`:

1. compile format token semantics and shape-aware build semantics into one CFG
2. execute through engine; all writes happen under trame safety rules

> t[format.parse.program-self-contained] A decode-direction `Program` MUST be
> self-contained for execution semantics; executor behavior MUST be determined by
> program contents plus ABI/runtime capability checks.

> t[format.parse.program-abi-version] A decode-direction `Program` MUST carry an
> ABI version and MUST be rejected by incompatible executors.

> t[format.parse.single-program-token-and-build] Decode token dispatch and build
> actions MUST be expressed in one executable program.

> t[format.parse.build-ops-equivalence] Decode build semantics MUST be
> equivalent to executing the corresponding `Op` sequence on `Trame`.

> t[format.parse.build-state-machine-preserved] Decode execution MUST preserve
> trame state-machine rules (strict/deferred behavior, overwrite, container
> materialization, cleanup semantics).

### Disambiguation in Decode CFG

> t[format.parse.disambiguation-in-program] Key-based and value-based
> disambiguation MUST execute as instructions in the decode `Program`.

> t[format.parse.no-runtime-solver-api] Runtime execution MUST NOT depend on
> calling facet-solver APIs during decoding.

> t[format.parse.disambiguation-state-explicit] Candidate-set state and
> narrowing state used for disambiguation MUST be represented as explicit VM
> state (registers/locals/stack), not hidden host state.

### Safety

> t[format.parse.unsafe-contained] Unsafe writes and pointer arithmetic MUST be
> contained to the execution engine/runtime boundary.

> t[format.parse.no-direct-pointer-write-in-format] Format crates MUST NOT
> perform direct raw-pointer writes to destination values.

> t[format.parse.apply-through-runtime] Materialization effects MUST flow
> through runtime decode semantics, not ad hoc writes in format backends.

### Error Semantics

> t[format.parse.error-token-context] Decode errors MUST include source token
> context when available.

> t[format.parse.error-build-context] Build failures MUST include value-path
> context when available.

> t[format.parse.error-stop-on-failure] Execution MUST stop on first
> unrecoverable decode failure and MUST NOT continue mutating state.

### Performance

> t[format.parse.batch-friendly-execution] Engine execution MUST support
> batching/chunked processing to reduce per-token/per-op overhead.

> t[format.parse.hotpath-jit-eligibility] Hot decode programs MAY be JIT
> compiled under the same safety and equivalence constraints as interpreter
> decode execution.

## Core Instruction Set

Executable VM IR uses one opcode vocabulary across encode/decode programs.

### Execution State

Programs execute against explicit VM state:

- `pc`: `(proc-id, block-id, instr-index)`.
- `call-stack`: return addresses for `call`/`ret`.
- `path-stack`: current logical value path.
- `%tok`: current token register for decode control flow.
- `%cand-mask`: active candidate-set bitmask for decode disambiguation.
- `locals`: typed local/register slots for temporary decode state.
- `source-cursor`: decode input cursor.
- `source-save-stack`: source save points for replay.
- `sink-state`: encode sink writer state.
- `scratch`: bounded runtime scratch arena/context.

> t[format.vm.state-explicit] Instruction semantics MUST be defined in terms of
> explicit VM state.

> t[format.vm.state-resumable] VM state MUST be representable in a form that
> allows interpreter resume at an instruction boundary.

> t[format.vm.state-allocation-bounded] VM execution MUST NOT require arbitrary
> unbounded heap allocation; transient state MUST fit in locals/stacks/scratch.

### Storage Model

`call-stack` is the only control stack and is mutated only by `call`/`ret`.
`path-stack` is the only traversal stack and is mutated only by navigation
instructions.

Programs do not have a general heap-allocation opcode in v1.

> t[format.vm.storage-no-hidden-stacks] Implementations MUST NOT use hidden
> control or path stacks that diverge from VM-state stack contents at an
> instruction boundary.

> t[format.vm.storage-no-program-heap-op] v1 programs MUST NOT request arbitrary
> heap allocation through IR instructions.

> t[format.vm.storage-scratch-runtime-owned] Scratch allocation policy is runtime
> owned; IR execution MAY request scratch usage only through op semantics that
> explicitly permit it.

> t[format.vm.storage-scratch-bounded] Scratch usage MUST be bounded by
> runtime-configured limits and fail deterministically on exhaustion.

### Opcode Families and Legality

> t[format.vm.opcodes-explicit-set] The executable opcode set MUST be explicit
> and versioned by ABI.

> t[format.vm.opcodes-no-implicit-control] Opcodes MUST NOT imply hidden control
> flow; all non-fallthrough control flow MUST be encoded via explicit branch,
> jump, call, ret, fail, or halt instructions.

Families:

- Control flow:
  - `jump`, `branch`, `call`, `ret`, `halt`, `fail`
- Value navigation:
  - `enter-field`, `enter-index`, `enter-key`, `enter-value`, `enter-variant`, `leave`
- Encode emission:
  - `emit-begin-*`, `emit-field-name`, `emit-scalar`, `emit-null`, `emit-end`
- Decode token/control:
  - `read-token`, `expect-token`, `match-token`, `match-key`, `skip-value`, `source-save`, `source-restore`
- Decode disambiguation:
  - `cand-init`, `cand-key`, `cand-tag-eq`, `cand-dispatch`
- Build actions:
  - `build-set-imm`, `build-default`, `build-stage`, `build-end`, `build-begin-deferred`, `build-finish-deferred`

> t[format.vm.kind-metadata-only] Program `kind` MUST NOT define a different
> instruction set or a different compiler pipeline.

> t[format.vm.kind-io-contract-only] Program `kind` MAY constrain required I/O
> context at entry (sink for encode, source for decode), but opcode semantics
> MUST remain identical.

> t[format.vm.opcode-semantics-normative] Each opcode MUST have normative
> operand typing, preconditions, effects, and failure conditions.

> t[format.vm.opcode-failures-explicit] Runtime failure modes MUST be explicit
> and stable across interpreter and JIT tiers.

### Control Flow and Loops

> t[format.vm.cfg-single-terminator] Every basic block MUST end with exactly one
> terminator (`jump`, `branch`, `ret`, `halt`, or `fail`).

> t[format.vm.cfg-loop-backedge-valid] Loop back-edges MUST target a valid block
> in the current procedure.

> t[format.vm.cfg-call-target-valid] `call` targets MUST reference valid
> procedure labels.

### Normative Opcode Inventory

#### Control Flow

| Opcode | Operands | Effect | Failure |
| --- | --- | --- | --- |
| `jump` | `(to bN)` | Set `pc` to target block entry. | Invalid block id. |
| `branch` | `(pred pN) (then bN) (else bN)` | Evaluate predicate and jump to selected block. | Invalid predicate id, predicate error. |
| `call` | `(fN)` | Push return address, jump to callee entry block. | Invalid procedure id, stack overflow. |
| `ret` | none | Pop return address and continue; if entry proc frame, terminate success. | Empty call stack outside entry proc return. |
| `halt` | none | Terminate success immediately. | None. |
| `fail` | `(code symbol)` | Terminate with structured execution error. | None. |

#### Value Navigation

| Opcode | Operands | Effect | Failure |
| --- | --- | --- | --- |
| `enter-field` | `(index u32)` | Push `Field(index)` onto `path-stack`. | Index invalid for current shape node. |
| `enter-index` | `(index u32)` | Push fixed positional index segment. | Index invalid for current shape node. |
| `enter-key` | `(string u32)` | Push map-key segment resolved from `strings`. | Invalid string id. |
| `enter-value` | none | Push value segment for map entry payload. | Invalid container context. |
| `enter-variant` | `(index u32)` | Push enum variant segment for selected variant payload path. | Index invalid for current enum shape node. |
| `leave` | none | Pop one segment from `path-stack`. | Underflow at root path. |

#### Encode Emission

| Opcode | Operands | Effect | Failure |
| --- | --- | --- | --- |
| `emit-begin-struct` | `(fields u32)` | Emit struct/object-begin sink event. | Sink error. |
| `emit-begin-seq` | `(len u32\|unknown)` | Emit sequence-begin sink event. | Sink error. |
| `emit-begin-map` | `(len u32\|unknown)` | Emit map-begin sink event. | Sink error. |
| `emit-field-name` | `(string u32)` | Emit field-name/key sink event. | Invalid string id, sink error. |
| `emit-scalar` | none | Load scalar at current path and emit scalar sink event. | Type mismatch, sink error. |
| `emit-null` | none | Emit explicit null/unit sink event. | Sink error. |
| `emit-end` | none | Emit structural end sink event. | Structural imbalance, sink error. |

#### Decode Token and Dispatch

| Opcode | Operands | Effect | Failure |
| --- | --- | --- | --- |
| `read-token` | none | Consume next token from source into `%tok`. | Source error, unexpected EOF. |
| `expect-token` | `(kind K)` | Consume next token; fail unless `kind == K`; store in `%tok`. | Kind mismatch, source error. |
| `match-token` | `(kind K) (then bN) (else bN)` | Branch by `%tok.kind` without consuming additional input. | `%tok` unset. |
| `match-key` | `(string u32) (then bN) (else bN)` | Branch by key equality between `%tok` key and `strings[idx]`. | `%tok` not key, invalid string id. |
| `skip-value` | none | Consume exactly one full decode value (including nested structure). | Source error, malformed input. |
| `source-save` | none | Push current `source-cursor` onto `source-save-stack`. | Source does not support save, source error. |
| `source-restore` | none | Pop save point and restore `source-cursor`. | Empty save stack, invalid save point, source error. |

#### Decode Disambiguation

| Opcode | Operands | Effect | Failure |
| --- | --- | --- | --- |
| `cand-init` | `(mask #x...)` | Initialize `%cand-mask` with candidate bitmask. | Empty/invalid mask for current program. |
| `cand-key` | `(keep #x...)` | Narrow candidates: `%cand-mask &= keep`. | `%cand-mask` unset. |
| `cand-tag-eq` | `(string u32) (then-keep #x...) (else-keep #x...)` | Read `%tok` scalar string and narrow `%cand-mask` with `then-keep` or `else-keep`. | `%tok` not scalar string, invalid string id, `%cand-mask` unset. |
| `cand-dispatch` | `(case N bN)... (ambiguous bN) (none bN)` | If exactly one candidate remains, jump to its case; else jump `ambiguous` or `none`. | `%cand-mask` unset, dangling case target. |

#### Build Actions

| Opcode | Operands | Effect | Failure |
| --- | --- | --- | --- |
| `build-set-imm` | none | Convert `%tok` scalar payload to destination type and set current path. | `%tok` unset/non-scalar, conversion error, build error. |
| `build-default` | none | Apply default at current path. | Build error. |
| `build-stage` | `(capacity u32\|unknown)` | Stage node at current path with optional capacity hint. | Build error. |
| `build-end` | none | Close current staged node (equivalent to `End`). | Build error, structural imbalance. |
| `build-begin-deferred` | none | Enter deferred build mode for subsequent build actions. | Invalid mode transition, build error. |
| `build-finish-deferred` | none | Exit deferred build mode and validate deferred subtree. | Not in deferred mode, validation/build error. |

### Selected Opcode Semantics (Normative Details)

#### `match-token`

`match-token` compares `%tok.kind` against exactly one `kind` operand and does
not consume source input.

Multi-way token dispatch is represented by explicit chains/trees of
`match-token` blocks.

> t[format.vm.match-token-single-kind] `match-token` MUST test exactly one token
> kind per instruction.

> t[format.vm.match-token-multiway-by-cfg] Multi-way token dispatch MUST be
> represented via explicit CFG composition, not hidden switch semantics.

#### `emit-scalar`

`emit-scalar` resolves scalar kind from shape metadata at `current_path`, reads
the value through runtime-safe access, and emits one sink scalar event.

> t[format.vm.emit-scalar-kind-shape-driven] `emit-scalar` scalar kind MUST be
> selected from shape metadata at current path.

> t[format.vm.emit-scalar-single-event] `emit-scalar` MUST emit exactly one sink
> scalar callback or fail.

#### `cand-dispatch`

`cand-dispatch` inspects `%cand-mask` as follows:

1. if exactly one candidate bit is set, branch to its case target.
2. if zero bits are set, branch to `none`.
3. otherwise branch to `ambiguous`.

> t[format.vm.cand-dispatch-popcount] `cand-dispatch` solved/none/ambiguous
> outcomes MUST be determined by bit population count of `%cand-mask`.

> t[format.vm.cand-dispatch-no-implicit-narrow] `cand-dispatch` MUST NOT mutate
> `%cand-mask`.

#### `enter-variant`

`enter-variant` is explicit enum payload navigation. It does not perform hidden
variant solving; solver/disambiguation logic remains ordinary CFG instructions.

> t[format.vm.enter-variant-explicit-navigation] Enum payload traversal MUST use
> explicit `enter-variant` path navigation when variant-specific payload paths
> are required.

> t[format.vm.enter-variant-no-hidden-solver] `enter-variant` MUST NOT trigger
> implicit runtime variant solving or candidate narrowing.

## Decode Instruction Semantics

Decode programs consume a token stream and drive build actions.

### Token Interaction

> t[format.parse.token-read-explicit] Token consumption MUST happen only through
> explicit token-read/match/expect instructions.

> t[format.parse.token-kind-checked] `expect-token` MUST fail if the next token
> kind does not match expectation.

> t[format.parse.key-match-deterministic] `match-key` dispatch MUST be
> deterministic for a given key table and input token.

> t[format.parse.unknown-field-policy-explicit] Unknown-field behavior MUST be
> encoded explicitly in decode control flow (reject, skip, or collect).

> t[format.parse.token-match-no-implicit-read] `match-token` and `match-key`
> MUST NOT consume additional input beyond what `read-token`/`expect-token`
> consumed.

> t[format.parse.skip-value-single-tree] `skip-value` MUST consume exactly one
> syntactic value subtree and leave the source cursor at the next sibling token.

> t[format.parse.source-save-restore-explicit] Decode replay MUST use explicit
> `source-save`/`source-restore` instructions.

> t[format.parse.source-restore-exactness] `source-restore` MUST restore cursor
> state so replay observes the same token sequence from the saved point.

> t[format.parse.source-save-stack-balance] `source-save`/`source-restore` MUST
> be stack-balanced in all successful control-flow paths.

> t[format.parse.source-save-restore-lifo] Source save points MUST obey strict
> LIFO behavior.

> t[format.parse.source-restore-invalidates-token] `source-restore` MUST
> invalidate `%tok` and any parser peek buffer state.

> t[format.parse.source-restore-span-deterministic] Replay after
> `source-restore` MUST reproduce token spans/locations as well as token kinds
> and payloads when source locations are available.

### Disambiguation

> t[format.parse.candidate-state-explicit] Candidate elimination state MUST live
> in explicit VM state (`%cand-mask` and locals), not hidden host state.

> t[format.parse.candidate-narrowing-monotonic] Candidate narrowing operations
> (`cand-key`, `cand-tag-eq`) MUST be monotonic (`mask_next subseteq mask_prev`).

> t[format.parse.candidate-dispatch-total] `cand-dispatch` MUST handle all three
> outcomes: solved (single candidate), ambiguous (multiple), and no-match.

> t[format.parse.candidate-id-canonical] Candidate ids MUST be contiguous
> integers in `[0, candidate_count)` assigned in compiler-emitted canonical
> order.

> t[format.parse.candidate-bit-order] Candidate bit `i` MUST map to candidate id
> `i` using least-significant-bit-first ordering in `%cand-mask`.

> t[format.parse.candidate-mask-width] `%cand-mask` width MUST be exactly
> `ceil(candidate_count / 8)` bytes for the current decode program.

> t[format.parse.candidate-mask-nonempty] `cand-init` masks MUST include at
> least one set bit.

> t[format.parse.candidate-dispatch-cases-valid] `cand-dispatch` case ids MUST
> reference valid candidate ids for the current program.

> t[format.parse.candidate-count-consistent] Candidate count MUST be consistent
> across all candidate-related instructions in a decode program.

> t[format.parse.candidate-mask-width-consistent] All candidate masks used by
> `cand-init`, `cand-key`, and `cand-tag-eq` in one program MUST use identical
> mask byte width.

### Build Actions

> t[format.parse.build-action-explicit] All construction-side effects MUST be
> performed through explicit build instructions.

> t[format.parse.build-action-op-equivalent] Each build instruction MUST have
> normative equivalence to one or more `Op` state-machine effects.

> t[format.parse.build-stage-balance] `build-stage`/`build-end` nesting MUST be
> structurally balanced.

> t[format.parse.build-deferred-balance] `build-begin-deferred` and
> `build-finish-deferred` MUST be balanced.

> t[format.parse.build-deferred-semantics] Deferred build mode MUST be
> semantically equivalent to trame deferred-mode behavior for the same path and
> action sequence.

### Scalar Conversion (`build-set-imm`)

`build-set-imm` consumes `%tok` scalar payload and converts it using the target
shape at `current_path`.

v1 scalar source kinds:

- `bool`
- `int`
- `uint`
- `float`
- `string`
- `bytes`

v1 conversion rules:

- target bool:
  - accepts only source `bool`.
- target signed integer:
  - accepts source `int` if in-range;
  - accepts source `uint` if in-range;
  - rejects source `float`, `string`, `bytes`, `bool`, and token-kind `null`.
- target unsigned integer:
  - accepts source `uint` if in-range;
  - rejects source `int`, `float`, `string`, `bytes`, `bool`, and token-kind `null`.
- target float:
  - accepts source `float`;
  - accepts source `int`/`uint` using IEEE-754 round-to-nearest-ties-to-even;
  - rejects if result would be non-finite from finite input;
  - rejects source `string`, `bytes`, `bool`, and token-kind `null`.
- target string:
  - accepts only source `string`.
- target bytes:
  - accepts only source `bytes`.
- target null/unit:
  - accepts only token-kind `null`.

> t[format.parse.scalar-source-kinds-v1] `build-set-imm` conversion MUST use
> the scalar source kinds above in v1.

> t[format.parse.scalar-conversion-no-string-coercion] v1 MUST NOT perform
> implicit string-to-number/string-to-bool coercions.

> t[format.parse.scalar-conversion-overflow-error] Integer range overflow during
> conversion MUST be an execution error.

> t[format.parse.scalar-conversion-target-shape-driven] Conversion behavior MUST
> be selected from target shape at `current_path`, not from backend-specific
> heuristics.

### Decode Action Semantics

> t[format.parse.single-flow-token-and-build] Decode control flow MUST directly
> interleave token operations and build actions in one instruction stream.

> t[format.parse.build-stream-order] Build action order MUST match decode
> control flow order and MUST preserve exactly-once action delivery.

### Probe and Replay

> t[format.parse.probe-replay-single-program] Probe and replay phases MUST be
> represented as procedures/blocks in one decode `Program`.

> t[format.parse.probe-no-build-side-effects] Probe phase MUST NOT commit build
> side effects other than disambiguation state updates.

> t[format.parse.replay-after-dispatch] Replay/build phase MUST begin only after
> candidate dispatch resolves to a single candidate.

### Enum Tag Handling

Tag interpretation policy is compiler-owned and must be lowered explicitly in
decode CFG.

> t[format.parse.enum-tag-policy-explicit] Enum tag strategy (external/internal/
> adjacent/untagged) MUST be explicit in compiled decode control flow.

> t[format.parse.enum-tag-unknown-explicit] Unknown tag handling MUST be
> explicit in decode control flow (reject, fallback, or collect); it MUST NOT be
> implicit backend behavior.

> t[format.parse.enum-tag-order-explicit] For tag strategies with separate
> payload fields (for example adjacent tag/content), handling of field-order
> permutations MUST be explicit in decode control flow.

> t[format.parse.enum-tag-order-replay-required] If accepted input allows payload
> before tag and variant is required to route payload writes, decode control flow
> MUST use replay/buffering semantics (`source-save`/`source-restore` or
> equivalent) before committing variant-dependent build actions.

### Unknown Field Handling

Unknown fields are not implicit behavior. Compiler policy must lower to one of:

- reject:
  - emit `fail` with unknown-field error code.
- skip:
  - execute `skip-value` and continue.
- collect:
  - navigate to catch-all destination and execute explicit build actions.

> t[format.parse.unknown-field-no-implicit-default] Unknown field policy MUST
> NOT be hidden in source/token adapters.

## VM-to-Op Bridge

Two execution strategies are allowed:

1. `op-bridge` reference mode (execute build instructions by emitting `Op`s to
   Trame API surface).
2. `direct-build` optimized mode (apply equivalent runtime effects directly).

> t[format.bridge.reference-mode-required] Implementations MUST provide an
> `op-bridge` reference mode.

> t[format.bridge.direct-build-optional] `direct-build` mode MAY be implemented.

> t[format.bridge.direct-build-equivalence] `direct-build` mode MUST be
> observationally equivalent to `op-bridge` mode for all valid programs/inputs.

> t[format.bridge.direct-build-fallback] If `direct-build` cannot prove
> preconditions at runtime, it MUST fall back to `op-bridge`.

> t[format.bridge.error-surface-equivalent] Error classes and poisoning behavior
> in decode execution MUST match the `Op` state-machine semantics.

> t[format.bridge.deferred-mode-primitive] `build-begin-deferred` and
> `build-finish-deferred` are runtime mode-transition primitives; they are not
> representable as plain `Set`/`End` op stream entries.

> t[format.bridge.reference-mode-runtime-calls] In `op-bridge` mode, deferred
> mode primitives MUST be executed via explicit runtime calls around the emitted
> op stream.

### Reference Mapping (Normative)

In `op-bridge` mode, build actions map as:

| Build opcode | Required runtime effect |
| --- | --- |
| `build-stage` | `Set { dst: current_path, src: Stage(capacity) }` |
| `build-default` | `Set { dst: current_path, src: Default }` |
| `build-set-imm` | `Set { dst: current_path, src: Imm(decoded_scalar) }` |
| `build-end` | `End` |
| `build-begin-deferred` | Enter deferred mode (`begin_deferred`) in decode runtime. |
| `build-finish-deferred` | Exit deferred mode (`finish_deferred`) and validate deferred subtree. |

`current_path` is derived from `path-stack` as the canonical `PathSegment`
sequence.

> t[format.bridge.path-derivation-canonical] Path derivation from navigation
> instructions to `Op.dst` MUST be canonical and deterministic.

> t[format.bridge.build-op-mapping-complete] Every build opcode legal in the
> current ABI MUST have one normative `op-bridge` mapping.

## Encode Sink and Decode Source Contracts

### Encode Sink

> t[format.sink.interface-stable] The encode sink interface MUST be explicitly
> versioned by ABI.

> t[format.sink.scalar-types] Sink scalar callbacks MUST support at least:
> bool, integers, floats, string, bytes, null/unit.

> t[format.sink.structure-order] Sink must observe structure ordering emitted by
> VM instructions without backend-side reordering.

Canonical sink event interface:

```lisp
(sink-v1
  (begin (kind struct|seq|map) (len u32|unknown))
  (field-name (string u32))
  (scalar (kind scalar-kind) (payload ...))
  (null)
  (end))
```

> t[format.sink.events-canonical] Encode sink contract MUST be expressible as
> the canonical event interface above.

> t[format.sink.events-total] Each sink callback MUST return either success or a
> structured error; no silent drops are allowed.

### Decode Source

> t[format.source.token-interface-stable] Decode source token interface MUST be
> explicitly versioned by ABI.

> t[format.source.token-kinds-minimum] Token interface MUST provide at least:
> object start/end, array start/end, key, scalar, null, eof.

> t[format.source-location-optional] Source location/span metadata MAY be
> provided; if provided, decode errors MUST preserve it.

Canonical source token interface:

```lisp
(source-v1
  (next-token -> token)
  (peek-token -> token|none)
  (skip-value)
  (save -> save-point)
  (restore (save-point))
  (scalar
    (kind bool|int|uint|float|string|bytes)
    (payload ...))
  (token
    (kind object-start|object-end|array-start|array-end|key|scalar|null|eof)
    (payload ...)
    (location optional)))
```

> t[format.source.peek-optional] `peek-token` MAY be absent; runtimes MUST be
> able to emulate it with a one-token buffer.

> t[format.source.skip-optional] `skip-value` MAY be absent; runtimes MUST be
> able to emulate it via token stepping.

> t[format.source.save-restore-required] Source interface MUST support
> save/restore for decode replay-capable programs.

> t[format.source.save-restore-deterministic] Restoring a save point MUST make
> subsequent token reads deterministic with respect to the original read stream.

> t[format.source.scalar-kind-tagged] Scalar tokens MUST carry an explicit
> scalar kind tag (`bool|int|uint|float|string|bytes`).

> t[format.source.save-restore-capability-guarded] Program loading/execution
> MUST fail if a decode program uses `source-save`/`source-restore` and the
> source backend does not provide save/restore capability.

### Save-Point Contract (v1)

`save-point` is runtime-scoped state produced by `save` and consumed by
`restore`.

Canonical logical save-point fields:

```lisp
(save-point-v1
  (source-instance-id u64)
  (cursor-offset u64)
  (epoch u32))
```

Field meaning:

- `source-instance-id`: stable id of the live source instance.
- `cursor-offset`: logical token-stream position for replay.
- `epoch`: monotonic invalidation generation for this source instance.

> t[format.source.save-point-shape-v1] `save-point` MUST be representable by the
> `save-point-v1` logical fields above.

> t[format.source.save-point-instance-scoped] `restore` MUST reject save points
> whose `source-instance-id` does not match the current source instance.

> t[format.source.save-point-epoch-checked] `restore` MUST reject stale save
> points whose `epoch` is no longer valid for the source instance.

> t[format.source.save-point-offset-deterministic] Restoring a valid save point
> MUST place source cursor at exactly `cursor-offset` logical position.

> t[format.source.save-point-deopt-serializable] Save points MUST be
> serializable/reconstructable for deopt snapshot transfer.

> t[format.source.payload-valid-until-next-read] Token payload borrows (for
> string/bytes) MUST remain valid until the next token-consuming operation or
> `source-restore`, whichever happens first.

> t[format.source.payload-invalidated-on-restore] `source-restore` MUST
> invalidate previously observed token payload borrows.

> t[format.source.payload-owned-copy-required] If build semantics require owning
> string/bytes data beyond token payload validity, runtime MUST copy into owned
> storage before payload invalidation.

> t[format.source.payload-no-borrow-leak] Executors and backends MUST NOT retain
> direct borrowed references to source payloads past documented validity bounds.

## Predicates

Predicates are constant-table entries referenced by control-flow branches.

### Predicate Forms

Canonical forms:

```lisp
(predicates
  ((p0 (builtin is-none (path (field 3))))
   (p1 (builtin str-empty (path (field 1))))
   (p2 (host 12 (args ((path (field 2))))))))
```

> t[format.pred.table-canonical] Predicates MUST be declared in `consts` and
> referenced by label (`pN`) from executable code.

> t[format.pred.pure] Predicate evaluation MUST be side-effect free.

> t[format.pred.bool-only] Predicates MUST produce boolean outcomes or explicit
> execution errors.

> t[format.pred.builtin-or-host] Predicate kinds MUST be either VM builtins or
> host callbacks identified by stable ids in the current ABI.

> t[format.pred.host-call-guarded] Host predicate calls MUST be guarded by ABI
> compatibility checks and capability checks.

> t[format.pred.arg-model-explicit] Predicate argument model MUST be explicit in
> IR (path references, constants, or token/value registers).

> t[format.pred.host-signature-stable] Host predicate callbacks MUST use a
> versioned ABI-stable signature that returns `bool` or structured error.

## Worked Decode Example

This example decodes a JSON object into shape `{ id: u32, name: string }`.
Unknown fields are skipped.

```lisp
(vmir
  (abi 1)
  (kind decode)
  (shape-id 42)
  (consts
    (strings ("id" "name"))
    (predicates ()))
  (code
    (procs
      ((f0
        (entry b0)
        (blocks
          ((b0
            (build-stage (capacity 2))
            (expect-token (kind object-start))
            (jump b1))
           (b1
            (read-token)
            (match-token (kind object-end) (then b9) (else b2)))
           (b2
            (match-key (string 0) (then b3) (else b4)))
           (b3
            (enter-field (index 0))
            (expect-token (kind scalar))
            (build-set-imm)
            (leave)
            (jump b1))
           (b4
            (match-key (string 1) (then b5) (else b6)))
           (b5
            (enter-field (index 1))
            (expect-token (kind scalar))
            (build-set-imm)
            (leave)
            (jump b1))
           (b6
            (skip-value)
            (jump b1))
           (b9
            (build-end)
            (halt)))))))
    (entry-proc f0)))
```

## Worked Decode Example (Probe + Replay for Flatten/Untagged)

This example shows one decode `Program` with:

- probe blocks (`b0..b13`): save, inspect keys, narrow candidates, restore, dispatch
- replay blocks for candidate 0 (`b20..b29`): route `{ id, x }` (variant `A`)
- replay blocks for candidate 1 (`b30..b39`): route `{ id, y }` (variant `B`)

Equivalent high-level shape:

```rust
struct Outer {
    id: u32,
    #[facet(flatten)]
    kind: Kind,
}

enum Kind {
    A { x: u32 },
    B { y: u32 },
}
```

```lisp
(vmir
  (abi 1)
  (kind decode)
  (shape-id 777)
  (consts
    (strings ("id" "x" "y" "type" "A"))
    (predicates ()))
  (code
    (procs
      ((f0
        (entry b0)
        (blocks
          ((b0
            (source-save)
            (cand-init (mask #x03))
            (expect-token (kind object-start))
            (jump b1))
           (b1
            (read-token)
            (match-token (kind object-end) (then b9) (else b2)))
           (b2
            (match-key (string 0) (then b3) (else b4)))
           (b3
            (cand-key (keep #x03))
            (skip-value)
            (jump b1))
           (b4
            (match-key (string 3) (then btag) (else b5)))
           (btag
            (expect-token (kind scalar))
            (cand-tag-eq (string 4) (then-keep #x01) (else-keep #x02))
            (jump b1))
           (b5
            (match-key (string 1) (then b6) (else b7)))
           (b6
            (cand-key (keep #x01))
            (skip-value)
            (jump b1))
           (b7
            (match-key (string 2) (then b8) (else b8u)))
           (b8
            (cand-key (keep #x02))
            (skip-value)
            (jump b1))
           (b8u
            (skip-value)
            (jump b1))
           (b9
            (source-restore)
            (cand-dispatch
              (case 0 b20)
              (case 1 b30)
              (ambiguous b12)
              (none b13)))
           (b12 (fail (code decode-ambiguous)))
           (b13 (fail (code decode-no-match)))
           (b20
            (build-begin-deferred)
            (build-stage (capacity 2))
            (expect-token (kind object-start))
            (jump b21))
           (b21
            (read-token)
            (match-token (kind object-end) (then b29) (else b22)))
           (b22
            (match-key (string 0) (then b23) (else b24)))
           (b23
            (enter-field (index 0))
            (expect-token (kind scalar))
            (build-set-imm)
            (leave)
            (jump b21))
           (b24
            (match-key (string 1) (then b25) (else b26)))
           (b25
            (enter-field (index 1))
            (expect-token (kind scalar))
            (build-set-imm)
            (leave)
            (jump b21))
           (b26
            (skip-value)
            (jump b21))
           (b29
            (build-end)
            (build-finish-deferred)
            (halt))
           (b30
            (build-begin-deferred)
            (build-stage (capacity 2))
            (expect-token (kind object-start))
            (jump b31))
           (b31
            (read-token)
            (match-token (kind object-end) (then b39) (else b32)))
           (b32
            (match-key (string 0) (then b33) (else b34)))
           (b33
            (enter-field (index 0))
            (expect-token (kind scalar))
            (build-set-imm)
            (leave)
            (jump b31))
           (b34
            (match-key (string 2) (then b35) (else b36)))
           (b35
            (enter-field (index 1))
            (expect-token (kind scalar))
            (build-set-imm)
            (leave)
            (jump b31))
           (b36
            (skip-value)
            (jump b31))
           (b39
            (build-end)
            (build-finish-deferred)
            (halt)))))))
    (entry-proc f0)))
```

## Worked Decode Example (Adjacently Tagged Enum)

This example decodes:

- input shape: `{"type":"Pair","content":{"a":1,"b":2}}`
- enum:
  - `Pair { a: u32, b: u32 }`
  - `Unit`

This minimal example assumes tags lower to `Pair` or `Unit`.
It also enforces `type` before `content`; alternate field order requires
replay/buffering lowering per enum-tag ordering rules.

```lisp
(vmir
  (abi 1)
  (kind decode)
  (shape-id 9001)
  (consts
    (strings ("type" "content" "Pair" "a" "b"))
    (predicates ()))
  (code
    (procs
      ((f0
        (entry b0)
        (blocks
          ((b0
            (build-stage (capacity 1))
            (expect-token (kind object-start))
            (expect-token (kind key))
            (match-key (string 0) (then b1) (else b97)))
           (b1
            (expect-token (kind scalar))
            (cand-init (mask #x03))
            (cand-tag-eq (string 2) (then-keep #x01) (else-keep #x02))
            (expect-token (kind key))
            (match-key (string 1) (then b2) (else b97)))
           (b2
            (expect-token (kind object-start))
            (cand-dispatch
              (case 0 b10)
              (case 1 b20)
              (ambiguous b98)
              (none b99)))
           (b10
            (read-token)
            (match-token (kind object-end) (then b19) (else b11)))
           (b11
            (match-key (string 3) (then b12) (else b13)))
           (b12
            (enter-variant (index 0))
            (enter-field (index 0))
            (expect-token (kind scalar))
            (build-set-imm)
            (leave)
            (leave)
            (jump b10))
           (b13
            (match-key (string 4) (then b14) (else b15)))
           (b14
            (enter-variant (index 0))
            (enter-field (index 1))
            (expect-token (kind scalar))
            (build-set-imm)
            (leave)
            (leave)
            (jump b10))
           (b15 (fail (code decode-unknown-field)))
           (b19
            (expect-token (kind object-end))
            (build-end)
            (halt))
           (b20
            (expect-token (kind object-end))
            (enter-variant (index 1))
            (build-default)
            (leave)
            (expect-token (kind object-end))
            (build-end)
            (halt))
           (b97 (fail (code decode-expected-tag-content)))
           (b98 (fail (code decode-ambiguous)))
           (b99 (fail (code decode-no-match))))))))
    (entry-proc f0)))
```

## Binary Encoding v1

Binary VM IR is canonical and executable.

### Sections

Required top-level sections in canonical order:

1. Header (`abi`, `kind`, `shape-id`, flags)
2. String table
3. Predicate table
4. Procedure table (blocks/instructions)
5. Entry procedure id

> t[format.bin.section-order-canonical] Binary encoding MUST serialize sections
> in canonical order.

> t[format.bin.section-required] All required sections MUST be present exactly
> once.

### File Layout (v1)

Binary v1 is a single blob with fixed header then section payloads in canonical
order.

Header (fixed 32 bytes, little-endian):

- bytes `0..4`: ASCII magic `VMIR`
- bytes `4..6`: `abi_version` (`u16`)
- byte `6`: `kind` (`u8`; `0=encode`, `1=decode`)
- byte `7`: `flags` (`u8`)
- bytes `8..16`: `shape_id` (`u64`)
- bytes `16..20`: `strings_section_len` (`u32`)
- bytes `20..24`: `predicates_section_len` (`u32`)
- bytes `24..28`: `procs_section_len` (`u32`)
- bytes `28..32`: `entry_section_len` (`u32`)

Payload bytes follow immediately:

1. string table section bytes
2. predicate table section bytes
3. procedure table section bytes
4. entry procedure section bytes

> t[format.bin.magic-v1] Binary v1 MUST start with `VMIR` magic.

> t[format.bin.header-fixed-size] Binary v1 header MUST be exactly 32 bytes.

> t[format.bin.section-lens-trusted-after-verify] Section lengths MUST be bounds
> checked before section decoding.

### Section Payload Layouts

String table section:

- `string_count` (`u32`)
- repeated `string_count` times:
  - `byte_len` (`u32`)
  - `utf8_bytes[byte_len]`

Predicate table section:

- `predicate_count` (`u32`)
- repeated `predicate_count` times:
  - `pred_kind` (`u8`; `0=builtin`, `1=host`)
  - `pred_symbol_or_id` (canonical varint `u32`)
  - `arg_count` (canonical varint `u32`)
  - repeated args:
    - `arg_kind` (`u8`; `0=path`, `1=const-bool`, `2=const-int`, `3=const-string-id`, `4=token-reg`)
    - `arg_payload` (kind-specific bytes, canonical varint forms where integer ids appear)

Procedure table section:

- `proc_count` (`u32`)
- repeated `proc_count` times:
  - `proc_id` (canonical varint `u32`)
  - `entry_block_id` (canonical varint `u32`)
  - `block_count` (canonical varint `u32`)
  - repeated blocks:
    - `block_id` (canonical varint `u32`)
    - `instr_count` (canonical varint `u32`)
    - repeated instructions:
      - `opcode` (`u8`)
      - `operand_len` (canonical varint `u32`)
      - `operand_bytes[operand_len]` (opcode-specific encoding)

Entry section:

- `entry_proc_id` (canonical varint `u32`)

> t[format.bin.utf8-validated] String table entries MUST be validated UTF-8.

> t[format.bin.ids-in-range] Referenced ids (`proc`, `block`, `string`,
> `predicate`) MUST be in range after decoding.

### Opcode Byte Map (v1)

- `0x00 jump`
- `0x01 branch`
- `0x02 call`
- `0x03 ret`
- `0x04 halt`
- `0x05 fail`
- `0x10 enter-field`
- `0x11 enter-index`
- `0x12 enter-key`
- `0x13 enter-value`
- `0x14 leave`
- `0x15 enter-variant`
- `0x20 emit-begin-struct`
- `0x21 emit-begin-seq`
- `0x22 emit-begin-map`
- `0x23 emit-field-name`
- `0x24 emit-scalar`
- `0x25 emit-null`
- `0x26 emit-end`
- `0x30 read-token`
- `0x31 expect-token`
- `0x32 match-token`
- `0x33 match-key`
- `0x34 skip-value`
- `0x35 source-save`
- `0x36 source-restore`
- `0x40 cand-init`
- `0x41 cand-key`
- `0x42 cand-tag-eq`
- `0x43 cand-dispatch`
- `0x50 build-set-imm`
- `0x51 build-default`
- `0x52 build-stage`
- `0x53 build-end`
- `0x54 build-begin-deferred`
- `0x55 build-finish-deferred`

> t[format.bin.opcode-map-stable-v1] Opcode byte mapping above MUST be stable
> for ABI v1.

> t[format.bin.unknown-opcode-rejected] Unknown opcode bytes MUST be rejected at
> decode/verify time.

### Operand Encoding (v1)

Operand bytes are opcode-specific and use:

- ids/indexes/counts as canonical varint `u32`
- token kind as `u8` (`0=object-start`, `1=object-end`, `2=array-start`,
  `3=array-end`, `4=key`, `5=scalar`, `6=null`, `7=eof`)
- bitmask as `mask_len(varint u32)` + raw bytes (little-endian bit order)
- length mode as `u8` (`0=unknown`, `1=known`) followed by optional varint len

Per-op operand payloads:

- `jump`: `to_block_id`
- `branch`: `pred_id then_block_id else_block_id`
- `call`: `proc_id`
- `fail`: `error_code_string_id`
- `enter-field`: `field_index`
- `enter-index`: `index`
- `enter-key`: `string_id`
- `enter-variant`: `variant_index`
- `emit-begin-struct`: `field_count`
- `emit-begin-seq` / `emit-begin-map`: `len_mode [len_if_known]`
- `emit-field-name`: `string_id`
- `expect-token`: `token_kind`
- `match-token`: `token_kind then_block_id else_block_id`
- `match-key`: `string_id then_block_id else_block_id`
- `cand-init`: `mask_bytes`
- `cand-key`: `mask_bytes`
- `cand-tag-eq`: `string_id then_mask_bytes else_mask_bytes`
- `cand-dispatch`: `case_count`, repeated `(candidate_id block_id)`,
  then `ambiguous_block_id none_block_id`
- `build-stage`: `len_mode [capacity_if_known]`
- all other opcodes: empty operand payload

`fail` code encoding:

- text form uses `(fail (code symbol))`.
- binary form encodes the symbol via `error_code_string_id` into `strings`.

> t[format.bin.fail-code-interned] Binary `fail` operands MUST reference an
> interned error-code symbol in the string table.

> t[format.bin.fail-code-roundtrip] Text/binary round-trip MUST preserve `fail`
> code symbol identity.

> t[format.bin.operand-canonical] Operand payloads MUST use canonical forms and
> must not include trailing unused bytes.

### Predicate Argument Payload Encoding (v1)

`arg_payload` encoding is selected by `arg_kind`:

- `arg_kind=0` (`path`):
  - `seg_count` (canonical varint `u32`)
  - repeated `seg_count` times:
    - `seg_kind` (`u8`; `0=field`, `1=index`, `2=key`, `3=value`, `4=variant`)
    - `seg_payload`:
      - `field`: `field_index` (canonical varint `u32`)
      - `index`: `index` (canonical varint `u32`)
      - `key`: `string_id` (canonical varint `u32`)
      - `value`: empty
      - `variant`: `variant_index` (canonical varint `u32`)
- `arg_kind=1` (`const-bool`):
  - `bool_byte` (`u8`; `0=false`, `1=true`)
- `arg_kind=2` (`const-int`):
  - signed canonical zigzag varint (`i64` domain)
- `arg_kind=3` (`const-string-id`):
  - `string_id` (canonical varint `u32`)
- `arg_kind=4` (`token-reg`):
  - `token_view` (`u8`; `0=kind`, `1=scalar-payload`)

> t[format.bin.pred-arg-kind-known] Unknown predicate `arg_kind` values MUST be
> rejected by binary verifier for ABI v1.

> t[format.bin.pred-arg-path-segments-bounded] Predicate path segment count MUST
> be bounded by verifier limits.

> t[format.bin.pred-arg-key-string-in-range] Predicate path `key` segment string
> ids and `const-string-id` values MUST reference valid string table entries.

> t[format.bin.pred-arg-variant-index-in-range] Predicate path `variant` segment
> indexes MUST be valid for the referenced enum shape context.

> t[format.bin.pred-arg-token-view-known] Unknown `token_view` values MUST be
> rejected by binary verifier for ABI v1.

### Binary Verifier Requirements (v1)

Before execution, verifier MUST validate:

- CFG structure:
  - every block has exactly one terminator.
  - every block/proc target is in range.
- opcode/operand validity:
  - opcode byte known for ABI v1.
  - operand payload length matches opcode schema.
  - enum-like operand tags (token kinds, segment kinds, token views) are known.
- decode candidate invariants:
  - candidate mask widths are consistent within program.
  - candidate ids in `cand-dispatch` are unique and in range.
  - `cand-dispatch` has explicit `ambiguous` and `none` targets.

> t[format.bin.verify-cfg-terminators] Verifier MUST reject programs where any
> block lacks a valid single terminator.

> t[format.bin.verify-operand-schema] Verifier MUST reject instructions whose
> operand payload does not match opcode schema.

> t[format.bin.verify-candidate-invariants] Verifier MUST reject decode programs
> violating candidate-mask or dispatch invariants.

### Encoding Rules

> t[format.bin.endianness-fixed] Endianness for fixed-width fields MUST be
> little-endian.

> t[format.bin.varint-canonical] Variable-length integers MUST use a canonical
> varint encoding (no non-minimal encodings).

> t[format.bin.no-padding-by-default] Encoders MUST NOT emit unspecified padding
> bytes between fields/records.

> t[format.bin.verify-before-exec] Decoded binary programs MUST pass verifier
> checks before execution.

### Disk Cache

> t[format.bin.disk-cache-allowed] Binary IR MAY be cached to disk.

> t[format.bin.disk-cache-key-minimum] Disk cache keys MUST include at least:
> program hash, ABI version, engine version, and target/runtime capability set.

> t[format.bin.disk-cache-reject-on-mismatch] Cache loads MUST reject entries
> with mismatched ABI/runtime capability requirements.
