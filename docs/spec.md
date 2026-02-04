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

## Mental Model: A Tree of Nodes

### Terminology

Allocate means reserving memory in the uninitialized state. Initialize means
transitioning a byte range to initialized. Drop means transitioning a byte
range back to uninitialized for the given shape.

Nodes are **Open** or **Closed**. Open nodes can be mutated; closed nodes are
finalized. A node is **fully initialized** when all of its fields are
initialized directly or via closed child nodes.

### Example constructions

Trame models construction as a tree of nodes with a cursor pointing at the
current node. The diagrams below show node state explicitly.

Legend: `⟨...⟩` node, `○` uninitialized, `●` initialized, `🔒︎` closed, `▶` cursor, `✨` newly changed, `📦` owns allocation.

### Simple Scalar

A new `Trame<u32>` starts with a single root node that is not initialized yet
(`○`).

```
▶ ⟨Root: u32⟩ ○ ✨
```

Once you have a Trame, you apply operations. Each node has a data pointer, and
`Set` writes through that pointer.

For a `u32`, the two useful `Set` modes are:
- Immediate source: writes the provided `u32` bytes (for example, `42`) into
  the node's data and records that the node is now initialized (`●`).
- Default source: writes the type's default value into the node's data and
  records the same initialized state (`●`).

```rust
set(&[], imm(42))
```
```
▶ ⟨Root: u32⟩ ● ✨
```

`build()` requires the root node to be fully initialized. In this example it
is, because we just set it, so `build()` returns a `HeapValue` that can be
materialized as a `u32`.

### Simple Struct

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
open.

Initial state (only the root node exists; fields are uninitialized slots `○`):

```
▶ ⟨Root: Pair⟩ ✨
  ├─ a ○
  └─ b ○
```

Just like the scalar before, we can use set to initialize the entire struct in one go:

```rust
set(&[], imm(some_struct))
```

```
▶ ⟨Root: Pair⟩
  ├─ a ● ✨
  └─ b ● ✨
```

But starting back from the initial state, we can also initialize the struct one field at a time:

```rust
set(&[Field(0)], imm(13))
```

```
▶ ⟨Root: Pair⟩
  ├─ a ● ✨
  └─ b ○
```

If we were to call `build()` at this point in time, it would return an error.
`build()` takes ownership of the Trame, so there are only two outcomes:
- A `HeapValue` that is fully valid and fully initialized.
- An error.

If there is an error, the Trame drops and cleans up:
- anything that was initialized is cleanly de-initialized
- anything that was allocated is cleanly deallocated

### Nested Struct

Now consider a struct that contains another struct:

```rust
struct Outer {
    inner: Pair,
    c: u32,
}
```

The root node starts open. To build `inner` incrementally, we stage that field,
which creates a child node and moves the cursor to it.

```
▶ ⟨Root: Outer⟩ ✨
  ├─ inner ○
  └─ c ○
```

```rust
set(&[Field(0)], stage())
```

```
  ⟨Root: Outer⟩
▶ ├─ inner → ⟨Child: Pair⟩ ✨
  │         ├─ a ○
  │         └─ b ○
  └─ c ○
```

With the cursor on the child, paths are relative to the child node.

```rust
set(&[Field(0)], imm(1))
```

```
  ⟨Root: Outer⟩
▶ ├─ inner → ⟨Child: Pair⟩
  │         ├─ a ● ✨
  │         └─ b ○
  └─ c ○
```

We now call `end()`. What happens next depends on the mode.

**Strict mode (default)** prioritizes correctness, then performance. It fails
early and noisily, which is great for diagnostics.

**Folding is an optimization.** It replaces the child with a single
initialized field in the parent and removes the child from the tree. This
keeps memory usage low and the tree small.

**Safety requirement.** To safely fold, the child must be fully initialized.
Otherwise we could lose track of uninitialized bytes and return a partially
initialized value to safe Rust.

With only `a` initialized, validation fails, so `end()` errors. That error
poisons the Trame:
- everything is de-initialized
- everything is de-allocated
- the tree is gone

```rust
end() // error
```

```
∅  (Trame poisoned; no tree remains)
```

If we finish `inner` first, the child can be folded into the parent and
removed from the tree.

```rust
set(&[Field(1)], imm(2))
```

```
  ⟨Root: Outer⟩
▶ ├─ inner → ⟨Child: Pair⟩
  │         ├─ a ●
  │         └─ b ● ✨
  └─ c ○
```

```rust
end()
```

```
▶ ⟨Root: Outer⟩
  ├─ inner ● ✨
  └─ c ○
```

**Deferred mode** is a secondary mode of operation. It exists to handle
`#[facet(flatten)]`, where fields from an inner struct are lifted to the same
level as the outer struct. In that world, valid inputs can arrive out of order.

For example, with:

```rust
struct Outer {
    #[facet(flatten)]
    inner: Pair,
    c: u32,
}
```

The flattened JSON can interleave fields like this:

```json
{ "a": 1, "c": 9, "b": 2 }
```

That order forces us to enter `inner` for `a`, exit to set `c`, then re-enter
`inner` for `b`. Deferred mode makes that possible by keeping child nodes
alive after `end()`.

Deferred mode starts from the same initial tree:

```
▶ ⟨Root: Outer⟩
  ├─ inner ○
  └─ c ○
```

```rust
set(&[Field(0)], stage())
```

```
  ⟨Root: Outer⟩
▶ ├─ inner → ⟨Child: Pair⟩ ✨
  │         ├─ a ○
  │         └─ b ○
  └─ c ○
```

```rust
set(&[Field(0)], imm(1))
```

```
  ⟨Root: Outer⟩
▶ ├─ inner → ⟨Child: Pair⟩
  │         ├─ a ● ✨
  │         └─ b ○
  └─ c ○
```

```rust
end()
```

```
▶ ⟨Root: Outer⟩ ✨
  ├─ inner → ⟨Child: Pair⟩
  │         ├─ a ●
  │         └─ b ○
  └─ c ○
```

```rust
set(&[Field(1)], imm(9))
```

```
▶ ⟨Root: Outer⟩
  ├─ inner → ⟨Child: Pair⟩
  │         ├─ a ●
  │         └─ b ○
  └─ c ● ✨
```

Re-enter `inner` by staging the same field again:

```rust
set(&[Field(0)], stage())
```

```
  ⟨Root: Outer⟩
▶ ├─ inner → ⟨Child: Pair⟩
  │         ├─ a ●
  │         └─ b ○
  └─ c ●
```

```rust
set(&[Field(1)], imm(2))
```

```
  ⟨Root: Outer⟩
▶ ├─ inner → ⟨Child: Pair⟩
  │         ├─ a ●
  │         └─ b ● ✨
  └─ c ●
```

```rust
end()
```

```
▶ ⟨Root: Outer⟩ ✨
  ├─ inner → ⟨Child: Pair⟩
  │         ├─ a ●
  │         └─ b ●
  └─ c ●
```

Validation is postponed until we exit deferred mode. A later section explains
how to enter deferred mode, how to exit it, and how final validation works.

```rust
end()
```
```
▶ ⟨Root: Outer⟩ ✨
  ├─ inner → ⟨Child: Pair⟩
  │         ├─ a ●
  │         └─ b ○
  └─ c ○
```

### Box

A `Box<T>` is modeled as two nodes: one for the box itself, and one for the
inner `T`. The box node owns a separate allocation (`📦`) and has a single
child slot (`Field(0)`).

Initial state:

```
▶ ⟨Root: Box<Pair>⟩ ○
```

To build incrementally, stage the box's only field. This allocates heap
storage for `T` (uninitialized) and creates the child node.

```rust
set(&[Field(0)], stage())
```

```
▶ ⟨Root: Box<Pair>⟩ 📦 ✨
  └─ 0 → ⟨Child: Pair⟩ ✨
      ├─ a ○
      └─ b ○
```

With the cursor on the child, paths are relative to the inner `Pair`.

```rust
set(&[Field(0)], imm(1))
```

```
▶ ⟨Root: Box<Pair>⟩ 📦
  └─ 0 → ⟨Child: Pair⟩
      ├─ a ● ✨
      └─ b ○
```

```rust
set(&[Field(1)], imm(2))
```

```
▶ ⟨Root: Box<Pair>⟩ 📦
  └─ 0 → ⟨Child: Pair⟩
      ├─ a ●
      └─ b ● ✨
```

In strict mode, `end()` on the inner `Pair` folds it into the box and removes
the child node from the tree.

```rust
end()
```

```
▶ ⟨Root: Box<Pair>⟩ 📦
  └─ 0 ● ✨
```

If you already have a complete box, you can set it directly and skip staging:

```rust
set(&[], imm(some_box))
```

```
▶ ⟨Root: Box<Pair>⟩ 📦 ● ✨
```

### Lists and Sets

Lists and sets use the same staging model. The list/set node owns a staging
allocation (`📦`). `Append` creates a new element frame at the end of the
staging buffer and moves the cursor into it. The caller tracks the element
index (track synthesis) for later re-entry.

Example: `Vec<Pair>` (the same model applies to sets).

Initial state:

```
▶ ⟨Root: Vec<Pair>⟩ ○
```

Append element 0:

```rust
set(&[Append], stage())
```

```
▶ ⟨Root: Vec<Pair>⟩ 📦 ● ✨
  └─ 0 → ⟨Child: Pair⟩ ✨
      ├─ a ○
      └─ b ○
```

```rust
set(&[Field(0)], imm(1))
```

```
▶ ⟨Root: Vec<Pair>⟩ 📦 ●
  └─ 0 → ⟨Child: Pair⟩
      ├─ a ● ✨
      └─ b ○
```

In deferred mode, `end()` returns to the list without folding the element, so
it can be re-entered later by index:

```rust
end()
```

```
▶ ⟨Root: Vec<Pair>⟩ 📦 ✨
  └─ 0 → ⟨Child: Pair⟩
      ├─ a ●
      └─ b ○
```

Re-enter element 0:

```rust
set(&[Field(0)], stage())
```

```
  ⟨Root: Vec<Pair>⟩ 📦
▶ └─ 0 → ⟨Child: Pair⟩
      ├─ a ●
      └─ b ○
```

```rust
set(&[Field(1)], imm(2))
```

```
  ⟨Root: Vec<Pair>⟩ 📦
▶ └─ 0 → ⟨Child: Pair⟩
      ├─ a ●
      └─ b ● ✨
```

In strict mode, once the element is fully initialized, `end()` folds it into
the list and removes the child node from the tree:

```rust
end()
```

```
▶ ⟨Root: Vec<Pair>⟩ 📦
  └─ 0 ● ✨
```

Finalization turns the staged elements into the actual list or set (strict:
on list/set `end()`, deferred: when exiting deferred mode).

### Stable addresses

Each element frame points into the list's staging allocation. If we used a
single contiguous staging buffer, a grow would relocate that buffer and
invalidate every descendant pointer.

Single staging buffer (safe before growth):

```
▶ ⟨Root: Vec<Pair>⟩ 📦
  └─ 0 → ⟨Child: Pair⟩
      ├─ a ●
      └─ b ○
```

After growth, the staging buffer moves, but child nodes still point at the old
address. That pointer is now stale, and the tree no longer represents reality.

```
▶ ⟨Root: Vec<Pair>⟩ 📦 ✨
  └─ 0 → ⟨Child: Pair⟩  (stale pointer)
      ├─ a ●
      └─ b ○
```

One possible option would be to patch every descendant pointer on every grow,
which is both expensive and fragile.

Instead, we use a **rope of staging chunks**: a list/set owns multiple fixed
allocations (`📦0`, `📦1`, …). New elements go into the next chunk, and existing
pointers remain stable.

```
▶ ⟨Root: Vec<Pair>⟩ 📦0 📦1 ✨
  ├─ 0 → ⟨Child: Pair⟩
  │     ├─ a ●
  │     └─ b ○
  └─ 1 → ⟨Child: Pair⟩
        ├─ a ● ✨
        └─ b ● ✨
```

Finalization flattens the rope into the actual vector/set in one pass, with a
preallocated target sized from the total element count.

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

TODO: Deferred mode + maps. Closed is one-way: a closed map cannot be
re-entered or re-opened. Rationale: re-entering would allow mutating keys or
values after finalization, or re-opening would clear the staged tuples and
lose data.
