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

## Dialect Invariants

This section is verifier-facing: each invariant is checkable on a concrete
`Program`.

> t[format.vm.dialect-schema-fixed] Program root schema MUST contain only the
> canonical keys (`abi`, `kind`, `shape-id`, `consts`, `code`); unknown root
> keys MUST be rejected.

> t[format.vm.dialect-no-runtime-format-tag] Program representations MUST NOT
> carry a runtime `format`/`profile` selector that changes VM execution
> semantics.

> t[format.vm.dialect-opcode-space-shared] For one ABI version, the opcode
> inventory and binary opcode map define one shared executable dialect for all
> formats; unknown opcode ids MUST be rejected.

> t[format.vm.dialect-structural-work-explicit] Delimiter/separator handling
> and structural control (object/map/sequence boundaries) MUST be represented by
> explicit IR control flow plus lexical/emission opcodes; runtimes MUST NOT
> inject implicit parser events.

> t[format.vm.dialect-builtins-closed-v1] ABI v1 builtin behavior is limited to
> the builtin opcodes in this spec; host-defined per-format opcode extensions
> MUST be rejected.

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
> canonical backslash escapes.

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
   - decode direction consumes source bytes/code-units and applies build actions

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

1. compile format lexical+parse semantics and shape-aware build semantics into one CFG
2. execute through engine; all writes happen under trame safety rules

> t[format.parse.program-self-contained] A decode-direction `Program` MUST be
> self-contained for execution semantics; executor behavior MUST be determined by
> program contents plus ABI/runtime capability checks.

> t[format.parse.program-abi-version] A decode-direction `Program` MUST carry an
> ABI version and MUST be rejected by incompatible executors.

> t[format.parse.single-program-lex-and-build] Decode lexical/parsing and build
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

> t[format.parse.error-token-context] Decode errors MUST include source position
> context when available.

> t[format.parse.error-build-context] Build failures MUST include value-path
> context when available.

> t[format.parse.error-stop-on-failure] Execution MUST stop on first
> unrecoverable decode failure and MUST NOT continue mutating state.

### Performance

> t[format.parse.batch-friendly-execution] Engine execution MUST support
> batching/chunked processing to reduce per-byte/per-op overhead.

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
- `%byte`: current input byte/code-unit register for decode lexical control.
- `%scalar`: current decoded scalar register for decode/build actions.
- `%key`: current decoded object-key register for key matching.
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
- Decode lexical/parse control:
  - `read-byte`, `peek-byte`, `expect-byte`, `match-byte`, `match-byte-class`, `skip-byte-class`, `scan-string`, `scan-number`, `scan-literal`, `skip-value`, `match-key`, `source-save`, `source-restore`
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

#### Decode Lexical/Parse Control

| Opcode | Operands | Effect | Failure |
| --- | --- | --- | --- |
| `read-byte` | none | Consume next source byte/code-unit into `%byte`. | Source error. |
| `peek-byte` | none | Load next source byte/code-unit into `%byte` without consuming. | Source error. |
| `expect-byte` | `(byte u8)` | Consume next byte; fail unless byte equals operand; store in `%byte`. | Byte mismatch, source error. |
| `match-byte` | `(byte u8) (then bN) (else bN)` | Branch by equality between `%byte` and operand byte. | `%byte` unset. |
| `match-byte-class` | `(class C) (then bN) (else bN)` | Branch by whether `%byte` belongs to class `C`. | `%byte` unset, invalid class id. |
| `skip-byte-class` | `(class C)` | Consume zero or more consecutive bytes in class `C`. | Invalid class id, source error. |
| `scan-string` | none | Consume one string literal according to active decode grammar, decode escapes/UTF-8, store in `%scalar` (string) and `%key` when in key position. | Malformed string, source error. |
| `scan-number` | none | Consume one numeric literal according to active decode grammar and store in `%scalar` (`int`/`uint`/`float`). | Malformed number, overflow policy error, source error. |
| `scan-literal` | `(kind true\|false\|null)` | Consume exact scalar literal for the selected kind and store corresponding `%scalar` value. | Literal mismatch, source error. |
| `skip-value` | none | Consume exactly one grammar value subtree from source bytes. | Malformed input, source error. |
| `match-key` | `(string u32) (then bN) (else bN)` | Branch by equality between `%key` and `strings[idx]`. | `%key` unset, invalid string id. |
| `source-save` | none | Push current `source-cursor` onto `source-save-stack`. | Source does not support save, source error. |
| `source-restore` | none | Pop save point and restore `source-cursor`. | Empty save stack, invalid save point, source error. |

`scan-string` key-position behavior is defined by parse context:

- key position is true when parser state indicates "expecting keyed-field name"
  in the active decode grammar.
- in key position, `scan-string` MUST write decoded string into both
  `%scalar` and `%key`.
- outside key position, `scan-string` MUST update `%scalar` and MUST clear
  `%key`.

> t[format.parse.scan-string-key-position-explicit] Key-position detection
> for `scan-string` MUST be determined by explicit parser context state.

> t[format.parse.scan-string-key-reg-update] `scan-string` MUST update
> `%key` only in key position.

#### Decode Disambiguation

| Opcode | Operands | Effect | Failure |
| --- | --- | --- | --- |
| `cand-init` | `(mask #x...)` | Initialize `%cand-mask` with candidate bitmask. | Empty/invalid mask for current program. |
| `cand-key` | `(keep #x...)` | Narrow candidates: `%cand-mask &= keep`. | `%cand-mask` unset. |
| `cand-tag-eq` | `(string u32) (then-keep #x...) (else-keep #x...)` | Compare `%scalar` string with `strings[idx]` and narrow `%cand-mask` with `then-keep` or `else-keep`. | `%scalar` not string, invalid string id, `%cand-mask` unset. |
| `cand-dispatch` | `(case N bN)... (ambiguous bN) (none bN)` | If exactly one candidate remains, jump to its case; else jump `ambiguous` or `none`. | `%cand-mask` unset, dangling case target. |

#### Build Actions

| Opcode | Operands | Effect | Failure |
| --- | --- | --- | --- |
| `build-set-imm` | none | Convert `%scalar` payload to destination type and set current path. | `%scalar` unset/non-scalar, conversion error, build error. |
| `build-default` | none | Apply default at current path. | Build error. |
| `build-stage` | `(capacity u32\|unknown)` | Stage node at current path with optional capacity hint. | Build error. |
| `build-end` | none | Close current staged node (equivalent to `End`). | Build error, structural imbalance. |
| `build-begin-deferred` | none | Enter deferred build mode for subsequent build actions. | Invalid mode transition, build error. |
| `build-finish-deferred` | none | Exit deferred build mode and validate deferred subtree. | Not in deferred mode, validation/build error. |

### Selected Opcode Semantics (Normative Details)

#### `match-byte` / `match-byte-class`

`match-byte` and `match-byte-class` branch on `%byte` without consuming further
input.

Multi-way lexical dispatch is represented by explicit CFG chains/trees.

> t[format.vm.match-byte-single-test] `match-byte` MUST test exactly one byte
> value per instruction.

> t[format.vm.match-byte-class-known] `match-byte-class` MUST reference a known
> class id in the active ABI.

> t[format.vm.match-byte-multiway-by-cfg] Multi-way lexical dispatch MUST be
> represented via explicit CFG composition, not hidden switch semantics.

#### Lex Builtins

`scan-string`, `scan-number`, `scan-literal`, and
`skip-value` are semantic builtins in the shared VM dialect.

They are optimization helpers only: each builtin MUST be observationally
equivalent to an explicit byte-level parser lowering in the same VM.

> t[format.vm.lex-builtin-format-neutral] Core lex builtin opcode names and
> contracts MUST be format-neutral; format identity MUST NOT be encoded in opcode
> names.

> t[format.vm.lex-builtin-semantic-equivalence] Lex builtins MUST be
> observationally equivalent to explicit byte-level IR lowering.

> t[format.vm.lex-builtin-optional-in-compiler] Compilers MAY choose builtins or
> fully expanded byte-level CFG, but resulting semantics MUST match.

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

> t[format.vm.enter-variant-ops-mapping-compatible] `enter-variant` semantics
> MUST be lowerable to ops path semantics where enum/variant navigation is
> represented with `Field` segments only.

> t[format.vm.enter-variant-ops-mapping-canonical] In `op-bridge` mode,
> `enter-variant(n)` MUST map to enum `Field(n)` selection; if payload descent
> occurs, bridge derivation MUST insert the canonical payload-root `Field(0)`
> step exactly once before the first payload child segment.

## Decode Instruction Semantics

Decode programs consume source bytes/code-units and drive build actions.

### Lexical and Parse Interaction

> t[format.parse.byte-read-explicit] Source-byte consumption MUST happen only
> through explicit lexical/parse instructions.

> t[format.parse.expect-byte-checked] `expect-byte` MUST fail if the next input
> byte does not match expectation.

> t[format.parse.key-match-deterministic] `match-key` dispatch MUST be
> deterministic for a given key table and decoded key value.

> t[format.parse.unknown-field-policy-explicit] Unknown-field behavior MUST be
> encoded explicitly in decode control flow (reject, skip, or collect).

> t[format.parse.byte-match-no-implicit-read] `match-byte`,
> `match-byte-class`, and `match-key` MUST NOT consume additional input beyond
> explicit consuming instructions.

> t[format.parse.skip-value-single-tree] `skip-value` MUST consume exactly
> one syntactic value subtree in the active grammar and leave source cursor at
> the next sibling
> position.

> t[format.parse.source-save-restore-explicit] Decode replay MUST use explicit
> `source-save`/`source-restore` instructions.

> t[format.parse.source-restore-exactness] `source-restore` MUST restore cursor
> state so replay observes the same input-byte sequence from the saved point.

> t[format.parse.source-save-stack-balance] `source-save`/`source-restore` MUST
> be stack-balanced in all successful control-flow paths.

> t[format.parse.source-save-restore-lifo] Source save points MUST obey strict
> LIFO behavior.

> t[format.parse.source-restore-invalidates-token] `source-restore` MUST
> invalidate `%byte`, `%key`, `%scalar`, and any parser lookahead state.

> t[format.parse.source-restore-span-deterministic] Replay after
> `source-restore` MUST reproduce byte offsets/spans and decoded values
> deterministically when source locations are available.

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

`build-set-imm` consumes `%scalar` payload and converts it using the target
shape at `current_path`.

v1 scalar source kinds:

- `bool`
- `int`
- `uint`
- `float`
- `string`
- `bytes`
- `null`

v1 conversion rules:

- target bool:
  - accepts only source `bool`.
- target signed integer:
  - accepts source `int` if in-range;
  - accepts source `uint` if in-range;
  - rejects source `float`, `string`, `bytes`, `bool`, and `null`.
- target unsigned integer:
  - accepts source `uint` if in-range;
  - rejects source `int`, `float`, `string`, `bytes`, `bool`, and `null`.
- target float:
  - accepts source `float`;
  - accepts source `int`/`uint` using IEEE-754 round-to-nearest-ties-to-even;
  - rejects if result would be non-finite from finite input;
  - rejects source `string`, `bytes`, `bool`, and `null`.
- target string:
  - accepts only source `string`.
- target bytes:
  - accepts only source `bytes`.
- target null/unit:
  - accepts only source `null`.

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
> interleave lexical/parse operations and build actions in one instruction
> stream.

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
> NOT be hidden in source adapters.

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

> t[format.bridge.enum-variant-path-expansion] Bridge derivation for
> `enter-variant` MUST follow canonical enum path expansion compatible with ops
> field-only enum navigation rules.

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

> t[format.source.byte-interface-stable] Decode source byte interface MUST be
> explicitly versioned by ABI.

> t[format.source.location-optional] Source location/span metadata MAY be
> provided; if provided, decode errors MUST preserve it.

Canonical source byte interface:

```lisp
(source
  (next-byte -> u8|eof)
  (peek-byte -> u8|eof)
  (save -> save-point)
  (restore (save-point))
  (slice (start u64) (end u64) -> bytes optional)
  (location optional))
```

> t[format.source.peek-optional] `peek-byte` MAY be absent; runtimes MUST be
> able to emulate it with a one-byte buffer.

> t[format.source.save-restore-required] Source interface MUST support
> save/restore for decode replay-capable programs.

> t[format.source.save-restore-deterministic] Restoring a save point MUST make
> subsequent byte reads deterministic with respect to the original read stream.

> t[format.source.no-host-tokenization] Source interface MUST provide raw
> bytes/code-units to IR; host-side structural tokenization MUST NOT be required
> for decode execution.

> t[format.source.save-restore-capability-guarded] Program loading/execution
> MUST fail if a decode program uses `source-save`/`source-restore` and the
> source backend does not provide save/restore capability.

### Save-Point Contract

`save-point` is runtime-scoped state produced by `save` and consumed by
`restore`.

Canonical logical save-point fields:

```lisp
(save-point
  (source-instance-id u64)
  (cursor-offset u64)
  (epoch u32))
```

Field meaning:

- `source-instance-id`: stable id of the live source instance.
- `cursor-offset`: logical byte-stream position for replay.
- `epoch`: monotonic invalidation generation for this source instance.

> t[format.source.save-point-shape] `save-point` MUST be representable by the
> logical fields above.

> t[format.source.save-point-instance-scoped] `restore` MUST reject save points
> whose `source-instance-id` does not match the current source instance.

> t[format.source.save-point-epoch-checked] `restore` MUST reject stale save
> points whose `epoch` is no longer valid for the source instance.

> t[format.source.save-point-offset-deterministic] Restoring a valid save point
> MUST place source cursor at exactly `cursor-offset` logical position.

> t[format.source.save-point-deopt-serializable] Save points MUST be
> serializable/reconstructable for deopt snapshot transfer.

> t[format.source.payload-valid-until-next-read] Decoded payload borrows (for
> `%key`/`%scalar` string/bytes values) MUST remain valid until the next
> byte-consuming operation or
> `source-restore`, whichever happens first.

> t[format.source.payload-invalidated-on-restore] `source-restore` MUST
> invalidate previously observed decoded payload borrows.

> t[format.source.payload-owned-copy-required] If build semantics require owning
> string/bytes data beyond decoded payload validity, runtime MUST copy into owned
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
> IR (path references, constants, or decode registers).

> t[format.pred.host-signature-stable] Host predicate callbacks MUST use a
> versioned ABI-stable signature that returns `bool` or structured error.

## Worked Decode Example

This example decodes a keyed object into shape `{ id: u32, name: string }`.
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
            (skip-byte-class (class ws))
            (expect-byte (byte #x7b)) ; {
            (jump b1))
           (b1
            (skip-byte-class (class ws))
            (peek-byte)
            (match-byte (byte #x7d) (then b9e) (else b2)))
           (b2
            (scan-string)
            (match-key (string 0) (then b3) (else b4)))
           (b3
            (skip-byte-class (class ws))
            (expect-byte (byte #x3a)) ; :
            (skip-byte-class (class ws))
            (scan-number)
            (enter-field (index 0))
            (build-set-imm)
            (leave)
            (jump b7))
           (b4
            (match-key (string 1) (then b5) (else b6)))
           (b5
            (skip-byte-class (class ws))
            (expect-byte (byte #x3a)) ; :
            (skip-byte-class (class ws))
            (scan-string)
            (enter-field (index 1))
            (build-set-imm)
            (leave)
            (jump b7))
           (b6
            (skip-byte-class (class ws))
            (expect-byte (byte #x3a)) ; :
            (skip-byte-class (class ws))
            (skip-value)
            (jump b7))
           (b7
            (skip-byte-class (class ws))
            (peek-byte)
            (match-byte (byte #x2c) (then b8) (else b9e)))
           (b8
            (read-byte) ; consume comma
            (jump b1))
           (b9e
            (expect-byte (byte #x7d)) ; }
            (jump b9))
           (b9
            (build-end)
            (halt)))))))
    (entry-proc f0)))
```

## Worked Decode Example (Probe + Replay for Flatten/Untagged)

This example shows one decode `Program` with byte-level probing and replay:

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
            (skip-byte-class (class ws))
            (expect-byte (byte #x7b)) ; {
            (jump b1))
           (b1
            (skip-byte-class (class ws))
            (peek-byte)
            (match-byte (byte #x7d) (then b9) (else b2)))
           (b2
            (scan-string)
            (match-key (string 0) (then b3) (else b4)))
           (b3
            (skip-byte-class (class ws))
            (expect-byte (byte #x3a))
            (skip-byte-class (class ws))
            (cand-key (keep #x03))
            (skip-value)
            (jump b7))
           (b4
            (match-key (string 3) (then btag) (else b5)))
           (btag
            (skip-byte-class (class ws))
            (expect-byte (byte #x3a))
            (skip-byte-class (class ws))
            (scan-string)
            (cand-tag-eq (string 4) (then-keep #x01) (else-keep #x02))
            (jump b7))
           (b5
            (match-key (string 1) (then b6) (else b7)))
           (b6
            (skip-byte-class (class ws))
            (expect-byte (byte #x3a))
            (skip-byte-class (class ws))
            (cand-key (keep #x01))
            (skip-value)
            (jump b7c))
           (b7
            (match-key (string 2) (then b8) (else b8u)))
           (b8
            (skip-byte-class (class ws))
            (expect-byte (byte #x3a))
            (skip-byte-class (class ws))
            (cand-key (keep #x02))
            (skip-value)
            (jump b7c))
           (b8u
            (skip-byte-class (class ws))
            (expect-byte (byte #x3a))
            (skip-byte-class (class ws))
            (skip-value)
            (jump b7c))
           (b7c
            (skip-byte-class (class ws))
            (peek-byte)
            (match-byte (byte #x2c) (then b7d) (else b1)))
           (b7d
            (read-byte)
            (jump b1))
           (b9
            (source-restore)
            (cand-dispatch
              (case 0 b20) ; replay route {id,x}
              (case 1 b30) ; replay route {id,y}
              (ambiguous b12)
              (none b13)))
           (b12 (fail (code decode-ambiguous)))
           (b13 (fail (code decode-no-match)))
           (b20 (call f1) (halt))
           (b30 (call f2) (halt)))))
       (f1
        (entry ba0)
        (blocks
          ((ba0
            (build-begin-deferred)
            (build-stage (capacity 2))
            ; byte-level replay parse with route {id,x}
            (build-end)
            (build-finish-deferred)
            (ret)))))
       (f2
        (entry bb0)
        (blocks
          ((bb0
            (build-begin-deferred)
            (build-stage (capacity 2))
            ; byte-level replay parse with route {id,y}
            (build-end)
            (build-finish-deferred)
            (ret)))))))
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
            (skip-byte-class (class ws))
            (expect-byte (byte #x7b))
            (skip-byte-class (class ws))
            (scan-string)
            (match-key (string 0) (then b1) (else b97)))
           (b1
            (skip-byte-class (class ws))
            (expect-byte (byte #x3a))
            (skip-byte-class (class ws))
            (scan-string)
            (cand-init (mask #x03))
            (cand-tag-eq (string 2) (then-keep #x01) (else-keep #x02))
            (skip-byte-class (class ws))
            (expect-byte (byte #x2c))
            (skip-byte-class (class ws))
            (scan-string)
            (match-key (string 1) (then b2) (else b97)))
           (b2
            (skip-byte-class (class ws))
            (expect-byte (byte #x3a))
            (skip-byte-class (class ws))
            (expect-byte (byte #x7b))
            (cand-dispatch
              (case 0 b10)
              (case 1 b20)
              (ambiguous b98)
              (none b99)))
           (b10
            (skip-byte-class (class ws))
            (peek-byte)
            (match-byte (byte #x7d) (then b19) (else b11)))
           (b11
            (scan-string)
            (match-key (string 3) (then b12) (else b13)))
           (b12
            (enter-variant (index 0))
            (enter-field (index 0))
            (skip-byte-class (class ws))
            (expect-byte (byte #x3a))
            (skip-byte-class (class ws))
            (scan-number)
            (build-set-imm)
            (leave)
            (leave)
            (jump b17))
           (b13
            (scan-string)
            (match-key (string 4) (then b14) (else b15)))
           (b14
            (enter-variant (index 0))
            (enter-field (index 1))
            (skip-byte-class (class ws))
            (expect-byte (byte #x3a))
            (skip-byte-class (class ws))
            (scan-number)
            (build-set-imm)
            (leave)
            (leave)
            (jump b17))
           (b15 (fail (code decode-unknown-field)))
           (b17
            (skip-byte-class (class ws))
            (peek-byte)
            (match-byte (byte #x2c) (then b18) (else b10)))
           (b18
            (read-byte)
            (jump b10))
           (b19
            (expect-byte (byte #x7d))
            (skip-byte-class (class ws))
            (expect-byte (byte #x7d))
            (build-end)
            (halt))
           (b20
            (expect-byte (byte #x7d))
            (enter-variant (index 1))
            (build-default)
            (leave)
            (skip-byte-class (class ws))
            (expect-byte (byte #x7d))
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

Flags byte (`flags`) layout in v1:

- bit `0`: `has_location_spans` hint.
- bits `1..7`: reserved.

> t[format.bin.flags-v1-defined] Binary v1 header flags layout MUST follow the
> definition above.

> t[format.bin.flags-v1-reserved-zero] Reserved flag bits (`1..7`) MUST be zero
> in v1 encodings.

> t[format.bin.flags-v1-unknown-rejected] Non-zero reserved flags MUST be
> rejected by verifier for ABI v1.

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
    - `arg_kind` (`u8`; `0=path`, `1=const-bool`, `2=const-int`, `3=const-string-id`, `4=decode-reg`)
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
- `0x30 read-byte`
- `0x31 peek-byte`
- `0x32 expect-byte`
- `0x33 match-byte`
- `0x34 match-byte-class`
- `0x35 skip-byte-class`
- `0x36 scan-string`
- `0x37 scan-number`
- `0x38 scan-literal`
- `0x39 skip-value`
- `0x3a match-key`
- `0x3b source-save`
- `0x3c source-restore`
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
- literal kind as `u8` (`0=true`, `1=false`, `2=null`)
- byte class as `u8` (`0=ws`, `1=digit`, `2=hex`, `3=quote`)
- bitmask as `mask_len(varint u32)` + raw bytes (little-endian bit order)
- length mode as `u8` (`0=unknown`, `1=known`) followed by optional varint len

Canonical varint (v1):

- unsigned varint is ULEB128 base-128 continuation encoding.
- canonical form is shortest possible byte sequence for the value.
- decoders MUST reject non-canonical encodings (for example redundant leading
  zero continuation groups).
- signed integers use canonical zigzag transform followed by canonical unsigned
  varint.

> t[format.bin.varint-format-defined] Canonical varint encoding MUST be ULEB128
> with shortest-form requirement.

> t[format.bin.varint-noncanonical-rejected] Non-canonical varint encodings MUST
> be rejected by verifier.

Byte class definitions (v1):

- `ws`: `{0x09, 0x0a, 0x0d, 0x20}`
- `digit`: `{0x30..0x39}`
- `hex`: `{0x30..0x39, 0x41..0x46, 0x61..0x66}`
- `quote`: `{0x22}`

> t[format.bin.byte-class-defined] Byte class ids MUST map to the sets above in
> ABI v1.

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
- `expect-byte`: `byte`
- `match-byte`: `byte then_block_id else_block_id`
- `match-byte-class`: `class_id then_block_id else_block_id`
- `skip-byte-class`: `class_id`
- `scan-literal`: `literal_kind`
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
- `arg_kind=4` (`decode-reg`):
  - `decode_view` (`u8`; `0=byte`, `1=key`, `2=scalar`)

> t[format.bin.pred-arg-kind-known] Unknown predicate `arg_kind` values MUST be
> rejected by binary verifier for ABI v1.

> t[format.bin.pred-arg-path-segments-bounded] Predicate path segment count MUST
> be bounded by verifier limits.

> t[format.bin.pred-arg-key-string-in-range] Predicate path `key` segment string
> ids and `const-string-id` values MUST reference valid string table entries.

> t[format.bin.pred-arg-variant-index-in-range] Predicate path `variant` segment
> indexes MUST be valid for the referenced enum shape context.

> t[format.bin.pred-arg-decode-view-known] Unknown `decode_view` values MUST be
> rejected by binary verifier for ABI v1.

### Binary Verifier Requirements (v1)

Before execution, verifier MUST validate:

- CFG structure:
  - every block has exactly one terminator.
  - every block/proc target is in range.
- opcode/operand validity:
  - opcode byte known for ABI v1.
  - operand payload length matches opcode schema.
  - enum-like operand tags (literal kinds, byte classes, segment kinds, decode views) are known.
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
