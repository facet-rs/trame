# Trame Specification

This document specifies the behavior of trame, a library for safe, verified
partial construction of Rust values using facet reflection.

## Conventions

This spec uses **MUST** to indicate normative requirements. Normative
requirements appear in blockquotes with a rule identifier (e.g. `r[...]`).
All other text is informative. Rule of thumb: if you can't write a test for
it, it's not a rule.

## Background

### Primer: Facet Shapes

Facet provides comprehensive reflection over Rust types via `&'static Shape`.
A `Shape` exposes:
- Identity: stable-in-compilation type IDs (`id`, `decl_id`)
- Layout: size and alignment (enough to allocate uninitialized memory)
- Operations: drop/default/clone via vtables and `type_ops`
- Type category: `ty` plus a `def` that enables struct/enum/list/map access
- Field/variant access: per-field offsets and per-field shapes for structs
- Marker traits: Copy/Send/Sync and related flags
- Names and location: type name, module path, source file/line/column
- Generics and metadata: type params, docs, custom attributes
- Serialization hints: tags, rename, proxy types, format proxies
- Wrappers/builders: `inner` newtype shape, optional builder shape
- Variance and flags: variance description and common attribute flags

Trame primarily relies on layout, field access, and drop/default operations,
but the full `Shape` is available for higher-level tooling.

### Primer: Rust Memory Operations

Rust exposes low-level memory operations (alloc/dealloc, pointer arithmetic,
`copy_nonoverlapping`, `drop_in_place`) that operate on untyped bytes. These
operations are powerful but unsafe without strict invariants. For the reflection
side of the story, see the Primer: Facet Shapes.

### Design Goal

By combining facet reflection with low-level memory operations, trame aims to
make incremental construction of arbitrary Rust values possible without causing
UB or otherwise unsoundness. The API surface is intentionally small and
verified.

### Mental Model: A Tree of Nodes

A partially built value is modeled as a tree of nodes. Each node represents a
subshape at a specific pointer (often an offset into the same allocation).
Nested nodes are not separately allocated; they are offsets into the parent
allocation. The root node is the entry point for construction.

## Normative Specification

### Terminology

Allocate means reserving memory in the uninitialized state. Initialize means
transitioning a byte range to initialized. Drop means transitioning a byte
range back to uninitialized for the given shape.

Nodes are **Staged** or **Sealed**. Staged nodes can be mutated; sealed nodes
are finalized. A node is **fully initialized** when all of its fields are
initialized directly or via sealed child nodes.

### Tree model

Trame models construction as a tree of nodes with a cursor pointing at the
current node. The diagrams below show node state explicitly.

Legend: `⟨...⟩` node, `○` uninitialized, `●` initialized, `🔒` sealed, `▶` cursor.

#### Scalar: `u32`

A new `Trame<u32>` starts with a single root node that is not initialized yet
(`○`).

```
▶ ⟨Root: u32⟩ ○
```

Once you have a Trame, you apply operations. Each node has a data pointer, and
`Set` writes through that pointer.

For a `u32`, the two useful `Set` modes are:
- Immediate source: writes the provided `u32` bytes (for example, `42`) into
  the node's data and records that the node is now initialized (`●`).
- Default source: writes the type's default value into the node's data and
  records the same initialized state (`●`).

```
▶ ⟨Root: u32⟩ ●
```

`build()` requires the root node to be fully initialized. In this example it
is, because we just set it, so `build()` returns a `HeapValue` that can be
materialized as a `u32`.

#### Structs (strict mode)

Rust allows grouping several values in a struct. For example:

```rust
struct Pair {
    a: u32,
    b: u32,
}
```

This struct is a single allocation, but Trame models it as three possible
nodes: the root node for the struct itself and one node for each field. We
start with a single node that represents the struct, and that root node starts
staged.

Initial state (only the root node exists; fields are uninitialized slots `○`):

```
▶ ⟨Root: Pair⟩
  ├─ a ○
  └─ b ○
```

Set the entire struct directly (immediate or default source):

```rust
set(&[], imm(some_struct))
```

```
▶ ⟨Root: Pair⟩
  ├─ a ●
  └─ b ●
```

Set a single field directly with a path of size one:

```rust
set(&[Field(0)], imm(13))
```

```
▶ ⟨Root: Pair⟩
  ├─ a ●
  └─ b ○
```

After setting the entire struct, the node is fully initialized but still
staged, so a later field `Set` overwrites just that field.

#### Smart pointers

Smart pointers follow the same mental model, but the node representing the
pointer owns its own allocation, and sealing the pointer implies its inner
value has been fully constructed.

## Verification Abstractions

Trame is parameterized over a small set of interfaces so the same construction
logic can run against a real implementation or a verified one. This is how the
project proves safety properties without changing core logic.

### Shape

Production uses `&'static Shape`. Verification uses a bounded dynamic shape
store that implements the same `IShape` interface. The point of `IShape` is to
let `Trame` and `Heap` operate over “a shape” without caring whether it is a
real static shape or a generated one.

Dynamic shapes exist to generate arbitrary shapes for verification. Without
them, tests are limited to the finite set of shapes present in the program at
compile time. The previous fuzzing approach in trame declared many static types
and hoped for enough coverage; dynamic shapes replace that with true shape
generation.

The shape store indirection (handles into a store) allows recursive shapes
without recursive Rust types, and enables `Arbitrary` generation of bounded
shape graphs.

### Heap

`Heap` defines the memory operations used by construction:
- `alloc`
- `dealloc`
- `memcpy`
- `mark_init`
- `drop_in_place`
- `is_init`

The verified heap tracks, per allocation:
- which shape was allocated
- which byte ranges are initialized

### Arena

The arena is also abstracted for verification. Production uses a growable
arena; verification uses a fixed-size arena with explicit occupancy checks.

### Zero-Cost Swap

The `Trame` type is instantiated with either the real implementations or the
verified ones. This makes verification a compile-time swap with zero runtime
cost in production builds.

## Notes

TODO: Deferred mode + maps. Sealed is one-way: a sealed map cannot be
re-entered or re-staged. Rationale: re-entering would allow mutating keys or
values after finalization, or re-staging would clear the staged tuples and
lose data.
