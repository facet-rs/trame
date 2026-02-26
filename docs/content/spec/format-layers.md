+++
title = "Typed Decode Layer Contracts"
insert_anchor_links = "heading"
weight = 40
+++

# Typed Layer Contracts for trame Runtime IR

## Introduction and Context

The trame runtime is a serialization runtime for moving between external wire
formats and Rust values using facet reflection metadata.

The practical target includes postcard and json and must scale to stateful
formats like toml and yaml.

This section is intentionally explicit about lineage and constraints:

1. [`serde`](https://serde.rs/) (crate: [`serde`](https://crates.io/crates/serde)) is the baseline in Rust serialization. It uses a generic visitor architecture with format-specific deserializer implementations and a fixed high-level data model.
2. [`facet`](https://facet.rs/) (crate: [`facet`](https://crates.io/crates/facet)) provides rich reflection over Rust types (field metadata, offsets, sizes/layout, type operations, and vtable-backed behavior), but pure runtime decision-making can carry overhead.
3. `facet-jit` (historical label) specialized decode paths with [`cranelift`](https://cranelift.dev/) (for example [`cranelift-jit`](https://docs.rs/cranelift-jit/latest/cranelift_jit/)), through the stack in [`facet-format`](https://github.com/facet-rs/facet/tree/main/facet-format) and [`facet-json`](https://github.com/facet-rs/facet/tree/main/facet-json). It demonstrated that specialization can be competitive with serde on relevant workloads ([perf.facet.rs](https://perf.facet.rs)).
4. [`trame`](https://github.com/facet-rs/trame) pursues the same broad goal (reflection + specialization) with a different runtime architecture: one typed IR, explicit layer contracts, and thin native code emission via [`dynasm`](https://docs.rs/dynasm/latest/dynasm/) / [`dynasmrt`](https://docs.rs/dynasmrt/latest/dynasmrt/) instead of relying on heavier JIT stacks.

Key trame crates for this architecture:

- [`trame-ir`](https://github.com/facet-rs/trame/tree/main/trame-ir)
- [`trame-interpreter`](https://github.com/facet-rs/trame/tree/main/trame-interpreter)
- [`trame-dynasm`](https://github.com/facet-rs/trame/tree/main/trame-dynasm)
- [`trame-exec`](https://github.com/facet-rs/trame/tree/main/trame-exec)
- [`trame-json`](https://github.com/facet-rs/trame/tree/main/trame-json)
- [`trame-postcard`](https://github.com/facet-rs/trame/tree/main/trame-postcard)

## Problem Statement

How do we deserialize and serialize multiple wire formats into/from Rust values
using facet reflection information, while staying competitive with serde,
keeping JIT compilation cheap enough, and maintaining strong safety/correctness
guarantees?

The problem is not only throughput. Generated machine code is outside many
normal Rust safety tools, so runtime contracts must make memory and state
behavior explicit and testable.

## Success Criteria

The primary success criterion is replacement viability in real projects, not
microbenchmark headlines.

- Projects currently using facet format crates can switch to trame runtime behavior without semantic regressions.
- This includes postcard-heavy workloads like roam rpc.
- Error reporting remains high quality.
- Performance wins matter only when correctness and safety hold.

## IR-First Model

This document is IR-first. It defines the executable artifact directly.

Syntax note for this draft: examples use rust-like `tir` pseudocode with `//`
comments. Comments are written before the line they explain.

> t[format.layers.ir-surface-first] The spec MUST define executable decode
> behavior from IR program surface examples before discussing lowering details.

> t[format.layers.single-program-shape-format] Shape and format functions for a
> decode target MUST be emitted into one shared `program` artifact.

### Program Surface (v1 Draft)

#### 1. Syntax

```ebnf
Program      = "program" "{" Abi Decl* Fn* "}" ;
Abi          = "abi" ":" Unsigned ;
Decl         = ConstDecl | TypeDecl | StateDecl ;
ConstDecl    = "const" Ident ":" Type "=" ConstExpr ;
TypeDecl     = StructDecl | EnumDecl ;
StructDecl   = "struct" Ident "{" FieldDecl* "}" ;
EnumDecl     = "enum" Ident "{" VariantDecl* "}" ;
StateDecl    = "state" "{" SlotDecl* "}" ;
SlotDecl     = Ident ":" SlotType ;
SlotType     = Type | "stack" "<" Type "," Unsigned ">" | "ring" "<" Type "," Unsigned ">" | "table" "<" Type "," Type "," Unsigned ">" | "bitmap" "<" Unsigned ">" ;
Fn           = "fn" Ident "(" ParamList? ")" "->" ResultType Block ;
ResultType   = "Result" "<" RetType "," ErrType ">" ;
RetType      = "void" | Type | TupleType ;
Block        = "{" Stmt* "}" ;
Stmt         = Assign | If | Loop | Break | RetOk | RetErr | ExprStmt ;
ExprStmt     = Expr ;
Expr         = Literal | Var | Field | Call | Binary | Unary | PtrAdd | PtrSub ;
```

> t[format.layers.input-shape-arbitrary] Program generation input MUST accept any
> facet shape category supported by the runtime.

> t[format.layers.output-program-specialized] Generated IR output MUST be
> specialized for that concrete shape and format profile.

#### 2. State Model

- Program state is only: locals, `state` slots, constants.
- No hidden host parser/runtime state is allowed.
- Shape-derived layout data is materialized as constants in output IR.
- Root/child container semantics are represented in explicit slots or locals.
- Slot storage class is explicit: fixed (`Type`) or bounded dynamic (`stack/ring/table/bitmap`).

> t[format.layers.state-explicit-only] All runtime state required for semantics
> MUST be explicit in locals or declared state slots.

> t[format.layers.state-shape-layout-materialized] Field offsets, field indexes,
> and required/default masks MUST be materialized as constants in output IR.

#### 3. Op Semantics

- Memory model: untyped byte-addressed memory.
- Width ops: `ld8/ld16/ld32/ld64`, `st8/st16/st32/st64`.
- Pointer ops: `ptr_add`, `ptr_sub`, `ptr_diff`.
- Integer ops: `add/sub/mul/and/or/xor/shl/shr`, compare branches.
- Conversion ops: `zext`, `sext`, `trunc`, `bitcast`.
- Control ops: `if`, `loop`, `break`, `ret ok`, `ret err`.
- Calls: `call fn(...)` only for declared IR functions/intrinsics.
- No tag+payload event union is required as ABI between shape and format code.

> t[format.layers.memory-width-explicit] All memory reads/writes MUST use
> explicit-width ops.

> t[format.layers.no-hidden-helper-calls] Semantics-critical behavior MUST NOT
> rely on backend-inserted helper calls absent from IR.

#### 4. Lowering Constraints

- v1 targets: `x86_64` little-endian, `aarch64` little-endian.
- Each op has total lowering mapping for each target.
- Lowering is deterministic for identical IR + target profile.
- Lowering preserves explicit error branches and memory op order.
- Backends may optimize instruction selection but not observable semantics.

> t[format.layers.lowering-op-total] Every defined op MUST have lowering rules
> for each supported v1 target.

> t[format.layers.lowering-deterministic] Lowering MUST be deterministic for
> identical IR and target profile.

```rust
// Example compiled output for one concrete shape+format request.
// Target shape: User { id: u32, active: bool }
// Format profile: json object.
program {
  abi: 1

  struct Span { start: u32, len: u32 }
  enum DecodeError {
    eof { cursor: u32 }
    invalid_byte { cursor: u32, got: u8 }
    unknown_key { key: Span }
    duplicate_field { field_ix: u32 }
    missing_required { field_ix: u32 }
    type_mismatch { field_ix: u32 }
  }

  struct JsonState { in_ptr: ptr, in_len: u32, cursor: u32 }
  state { json: JsonState }

  const FIELD_ID_IX: u32 = 0
  const FIELD_ACTIVE_IX: u32 = 1
  const FIELD_ID_OFFSET: u32 = 0
  const FIELD_ACTIVE_OFFSET: u32 = 4
  const SEEN_ID_MASK: u64 = 1
  const SEEN_ACTIVE_MASK: u64 = 2

  fn decode_root(in_ptr: ptr, in_len: u32, out_ptr: ptr) -> Result<void, DecodeError> {
    seen: u64 = 0
    call json_begin(in_ptr, in_len)
    call json_expect_byte('{')

    loop fields {
      call json_skip_ws()
      if call json_try_byte('}') { break }

      key = call json_parse_key_span()
      call json_skip_ws()
      call json_expect_byte(':')

      if call span_eq_ascii(key, "id") {
        if (seen & SEEN_ID_MASK) != 0 { ret err duplicate_field { field_ix: FIELD_ID_IX } }
        id_v = call json_parse_u32()
        st32(ptr_add(out_ptr, FIELD_ID_OFFSET), id_v)
        seen = seen | SEEN_ID_MASK
      } else if call span_eq_ascii(key, "active") {
        if (seen & SEEN_ACTIVE_MASK) != 0 { ret err duplicate_field { field_ix: FIELD_ACTIVE_IX } }
        active_v = call json_parse_bool()
        st8(ptr_add(out_ptr, FIELD_ACTIVE_OFFSET), zext_u8(active_v))
        seen = seen | SEEN_ACTIVE_MASK
      } else {
        ret err unknown_key { key: key }
      }

      call json_skip_ws()
      call json_try_byte(',')
    }

    if (seen & SEEN_ID_MASK) == 0 { ret err missing_required { field_ix: FIELD_ID_IX } }
    if (seen & SEEN_ACTIVE_MASK) == 0 { ret err missing_required { field_ix: FIELD_ACTIVE_IX } }
    ret ok
  }
}
```
