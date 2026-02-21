+++
title = "Specification"
insert_anchor_links = "heading"
+++

This document specifies the behavior of trame, a library for safe, verified
partial construction of Rust values using facet reflection.

## Conventions

This spec uses **MUST** to indicate normative requirements. Normative
requirements appear in blockquotes with a rule identifier (e.g. `t[...]`).
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

Lists and sets use the same staging model. The container node owns a **rope of
stable staging buffers** (`📦0`, `📦1`, ...), not the final runtime container.
`Append` allocates a new staging slot and creates an element frame pointing to
that stable slot. The caller tracks the element index (track synthesis) for
later re-entry.

Example: `Vec<Pair>` (the same model applies to sets).

If you already have a complete list or set, you can set it directly. This
closes the list/set (`🔒︎`): staging and `Append` are no longer allowed, but
`Set` with `Imm` or `Default` remains valid and overwrites the whole list/set.

Initial state:

```
▶ ⟨Root: Vec<Pair>⟩ ○
```

Append element 0:

```rust
set(&[Append], stage())
```

```
▶ ⟨Root: Vec<Pair>⟩ 📦0 ● ✨
  └─ 0 → ⟨Child: Pair⟩ ✨
      ├─ a ○
      └─ b ○
```

```rust
set(&[Field(0)], imm(1))
```

```
▶ ⟨Root: Vec<Pair>⟩ 📦0 ●
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
▶ ⟨Root: Vec<Pair>⟩ 📦0 ✨
  └─ 0 → ⟨Child: Pair⟩
      ├─ a ●
      └─ b ○
```

Re-enter element 0:

```rust
set(&[Field(0)], stage())
```

```
  ⟨Root: Vec<Pair>⟩ 📦0
▶ └─ 0 → ⟨Child: Pair⟩
      ├─ a ●
      └─ b ○
```

```rust
set(&[Field(1)], imm(2))
```

```
  ⟨Root: Vec<Pair>⟩ 📦0
▶ └─ 0 → ⟨Child: Pair⟩
      ├─ a ●
      └─ b ● ✨
```

In strict mode, once the element is fully initialized, `end()` seals the
element frame and returns to the list. It does **not** push into a live
`Vec<T>` yet.

```rust
end()
```

```
▶ ⟨Root: Vec<Pair>⟩ 📦0
  └─ 0 ● ✨
```

Finalization turns staged elements into the actual list/set in one shot by
materializing from the rope (single-buffer fast path, multi-buffer copy/move
path). Strict mode performs this when ending the list/set node; deferred mode
does it when exiting deferred mode or at `build()`.

### Maps

Maps use the same stable-staging principle. The map node owns a rope of stable
entry buffers, and `Append` creates an entry frame with `key` and `value`
slots in stable memory. The caller tracks the entry index (track synthesis) for
later re-entry. At materialization, duplicate keys use **last wins**.

Initial state:

```
▶ ⟨Root: Map<String, Pair>⟩ ○
```

If you already have a complete map, you can set it directly. This closes the
map (`🔒︎`): staging and `Append` are no longer allowed, but `Set` with `Imm`
or `Default` remains valid and overwrites the whole map.

```rust
set(&[], imm(some_map))
```

```
▶ ⟨Root: Map<String, Pair>⟩ ● 🔒︎ ✨
```

To build incrementally, append an entry:

```rust
set(&[Append], stage())
```

```
▶ ⟨Root: Map<String, Pair>⟩ 📦0 ● ✨
  └─ 0 → ⟨Entry: (Key, Value)⟩ ✨
      ├─ key ○
      └─ value ○
```

```rust
set(&[Field(0)], imm("a"))
```

```
▶ ⟨Root: Map<String, Pair>⟩ 📦0 ●
  └─ 0 → ⟨Entry: (Key, Value)⟩
      ├─ key ● ✨
      └─ value ○
```

In deferred mode, `end()` returns to the map without folding the entry, so it
can be re-entered later by index:

```rust
end()
```

```
▶ ⟨Root: Map<String, Pair>⟩ 📦0 ✨
  └─ 0 → ⟨Entry: (Key, Value)⟩
      ├─ key ●
      └─ value ○
```

Re-enter entry 0:

```rust
set(&[Field(0)], stage())
```

```
  ⟨Root: Map<String, Pair>⟩ 📦0
▶ └─ 0 → ⟨Entry: (Key, Value)⟩
      ├─ key ●
      └─ value ○
```

```rust
set(&[Field(1)], imm(some_pair))
```

```
  ⟨Root: Map<String, Pair>⟩ 📦0
▶ └─ 0 → ⟨Entry: (Key, Value)⟩
      ├─ key ●
      └─ value ● ✨
```

In strict mode, `end()` on a complete entry seals the entry and returns to the
map node. Materialization into the runtime map occurs when ending the map node
(or later, in deferred mode).

```rust
end()
```

```
▶ ⟨Root: Map<String, Pair>⟩ 📦0
  └─ 0 ● ✨
```

### Tuples

A tuple is like a struct with numbered fields. Each element is addressed with
`Field(n)` in order.

Initial state for `(u32, u32)`:

```
▶ ⟨Root: (u32, u32)⟩ ○
  ├─ 0 ○
  └─ 1 ○
```

```rust
set(&[Field(0)], imm(10))
```

```
▶ ⟨Root: (u32, u32)⟩
  ├─ 0 ● ✨
  └─ 1 ○
```

### Enums

We model enums as a three-level tree: **Enum → Variant → Payload**. Variant
selection uses `Field(n)`, and variant payloads are staged (no `Imm` for the
payload itself).

Selecting a variant is a state transition, not just navigation. When
`Field(n)` is applied at an enum node, trame must make variant `n` active
immediately (including writing the enum discriminant/tag in memory) before any
payload writes for that variant occur.

Example:

```rust
enum E {
    Unit,
    Pair(u32, u32),
    Named { x: u32, y: u32 },
}
```

Variant indices follow declaration order (`Unit` = `Field(0)`, `Pair` =
`Field(1)`, `Named` = `Field(2)`).

Initial state:

```
▶ ⟨Root: E⟩ ○
```

You may set the entire enum directly:

```rust
set(&[], imm(some_enum))
```

```
▶ ⟨Root: E⟩ ● ✨
```

If you later stage the enum, re-entering the **same** variant resumes; staging
a **different** variant replaces the old one and resets its payload. Replacing
the old variant must drop any initialized bytes owned by the previous payload
before activating the new variant.

Unit variants are initialized by default and have no payload.

Select the `Pair` variant and stage its payload:

```rust
set(&[Field(1)], stage())
```

```
  ⟨Root: E⟩
▶ └─ variant 1 → ⟨Variant: Pair⟩ ✨
      └─ payload ○
```

```rust
set(&[Field(0)], stage())
```

```
  ⟨Root: E⟩
  └─ variant 1 → ⟨Variant: Pair⟩
▶     └─ payload → ⟨Child: (u32, u32)⟩ ✨
        ├─ 0 ○
        └─ 1 ○
```

```rust
set(&[Field(0)], imm(1))
```

```
  ⟨Root: E⟩
  └─ variant 1 → ⟨Variant: Pair⟩
▶     └─ payload → ⟨Child: (u32, u32)⟩
        ├─ 0 ● ✨
        └─ 1 ○
```

In deferred mode, `end()` returns to the parent without folding. From the
payload, the first `end()` returns to the variant:

```rust
end()
```

```
  ⟨Root: E⟩
▶ └─ variant 1 → ⟨Variant: Pair⟩
      └─ payload → ⟨Child: (u32, u32)⟩
        ├─ 0 ●
        └─ 1 ○
```

Another `end()` returns to the enum, keeping the variant and payload in the
tree:

```rust
end()
```

```
▶ ⟨Root: E⟩
  └─ variant 1 → ⟨Variant: Pair⟩
      └─ payload → ⟨Child: (u32, u32)⟩
        ├─ 0 ●
        └─ 1 ○
```

Re-enter the same variant and payload, then set the second field:

```rust
set(&[Field(1)], stage())
```

```
  ⟨Root: E⟩
▶ └─ variant 1 → ⟨Variant: Pair⟩
      └─ payload → ⟨Child: (u32, u32)⟩
        ├─ 0 ●
        └─ 1 ○
```

```rust
set(&[Field(0)], stage())
```

```
  ⟨Root: E⟩
  └─ variant 1 → ⟨Variant: Pair⟩
▶     └─ payload → ⟨Child: (u32, u32)⟩
        ├─ 0 ●
        └─ 1 ○
```

```rust
set(&[Field(1)], imm(2))
```

```
  ⟨Root: E⟩
  └─ variant 1 → ⟨Variant: Pair⟩
▶     └─ payload → ⟨Child: (u32, u32)⟩
        ├─ 0 ●
        └─ 1 ● ✨
```

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

This means Trame conceptually builds `Vec<T>` (or `Set<T>`/`Map<K,V>`) from
stable staged `T` values at close/finalize time. It does **not** construct a
partially initialized `Vec<MaybeUninit<T>>` by repeatedly mutating the live
container during element construction.

## Reference Semantics

This section captures the reference API and the formal semantics that the
examples above are illustrating.

### API

```rust
enum PathSegment {
    Field(u32),      // Struct field, tuple element, array index, enum variant
    Append,          // New list element, set element, or map entry
    Root,            // Jump to root node
}

type Path = &[PathSegment];

enum Source {
    Imm(Imm),               // Copy bytes from existing value
    Stage(Option<usize>),   // Push a node (optional capacity hint)
    Default,                // Call Default::default() in place
}

enum Op {
    Set { dst: Path, src: Source },
    End,
}

fn apply(op: Op) -> Result<(), Error>;
```

### Concrete helpers

The concrete API includes a `Path` value type and builder helpers. These are
convenience surfaces; the semantics match the `PathSegment`/`Op` model above.

```rust
struct Path(PathVec);

impl Path {
    fn empty() -> Self;
    fn field(n: u32) -> Self;
    fn append() -> Self;
    fn root() -> Self;
    fn then(self, seg: PathSegment) -> Self;
    fn then_field(self, n: u32) -> Self;
    fn then_append(self) -> Self;
    fn segments(&self) -> &[PathSegment];
}

impl Op<'_> {
    fn set() -> SetBuilder;
    fn end() -> Op<'static>;
}

struct SetBuilder;
```

`SetBuilder` creates `Set` operations with a fluent API (`at`, `append`,
`root`, `imm`, `default`, `stage`, `stage_with_capacity`). This is purely a
builder; it does not change semantics.

### Op batches

`OpBatch` exists to support an `apply_ops`/`apply_batch` entry point (semantics
equivalent to repeatedly calling `apply(op)`), while preserving correctness.

Operations are stored in a queue and consumed from the front as they are
applied. After `apply_batch` returns:
- Consumed operations have been removed from the batch. Callers must forget
  any source values they moved into those ops.
- Remaining operations were not consumed and should be dropped normally.

### Paths

Paths are relative to the current node.

`Field(n)` selects field/element `n` of the current node. `Append` creates a new
list/set element or map entry. `Root` jumps to the root node and is only legal
as the first segment.

Multi-level paths are allowed. Intermediate segments implicitly create nodes
as if `Stage` had been applied at each step. The cursor ends at the deepest
node implied by the final segment.

`Root` behaves like repeated `End` as it climbs. If it climbs out of a deferred
subtree, it triggers deferred validation.

### Borrowed values (`'facet`)

Trame can construct values that borrow from exactly one lifetime: `'facet`.
This allows building values like `&'facet str` (or structs containing them)
without allocating or copying. The `'facet` lifetime ties the constructed
value to the input it borrows from, enabling zero-copy parsing while keeping
construction sound.

### Sources and overwrite

`Imm` copies bytes from an existing value. `Stage` pushes a node for
incremental construction (with an optional capacity hint). `Default` writes a
default value in place.

Overwrite semantics: applying `Imm` or `Default` at a location that already
has a partially initialized subtree drops that subtree and overwrites it. This
applies in both strict and deferred modes.

Closed containers: setting a whole list/set/map with `Imm` or `Default` closes
it. Closed containers reject staging and `Append`, but still allow `Imm` or
`Default` to overwrite the whole container.

`Append` is a staging-only operation. `Set { dst: &[Append], src: Imm }` and
`Set { dst: &[Append], src: Default }` are invalid for list/set/map.

### Containers

Lists and sets use rope-backed stable staging buffers. `Append` creates a
staged element node in a stable slot. Elements can be re-entered by index
(`Field(n)`), and container materialization happens only when closing/finalizing
the container (strict: on container `End`, deferred: when exiting deferred mode
or at `build()`).

Maps use rope-backed stable entry staging. `Append` creates an entry node with
`key` and `value` slots, which can be built in any order. Duplicate keys use
"last wins" at materialization.

Capacity hints are advisory preallocation inputs for container staging. Correct
hints should commonly produce a single-buffer rope; inaccurate hints may produce
multiple buffers but must not change observable semantics.

### Enums

`Field(n)` at an enum node selects variant `n` (declaration order). The tree is
Enum → Variant → Payload. Payloads are staged (no `Imm` for the payload).
Unit variants are initialized by default and have no payload.

If a variant is selected again: re-entering the same variant resumes, while a
different variant replaces the old one and resets its payload.

Variant selection is also a memory update. Applying `Field(n)` at an enum node
must establish variant `n` as active immediately by writing/updating the
discriminant/tag before any payload field writes are performed.

If a different variant was active, the previous payload must be dropped (for
its initialized portion) before switching, then the new payload starts from its
initial state (unit payload initialized by definition; non-unit payload
uninitialized until written/defaulted).

For multi-segment paths (for example, `Field(1) / Field(0) / ...`), the enum
transition for `Field(1)` occurs before descending into payload segments.

### Node lifecycle

Nodes are created by `Stage`, by `Append`, or by multi-level path segments that
enter uninitialized children.

In strict mode, a complete node may be folded into its parent and removed from
the tree. In deferred mode, nodes are not eagerly folded if they might be
re-entered.

### Strict vs deferred

Strict mode requires the current node to be fully initialized at `End`. It
validates and then folds the node into its parent.

Deferred mode allows `End` on incomplete nodes; the node remains in the tree
for later re-entry, and validation is postponed until exiting the deferred
subtree or at `build()`.

### Validation and defaults

Validation timing:
- Strict mode validates at each `End`.
- Deferred mode validates when exiting the deferred subtree or at `build()`.

Defaults:
- Fields annotated with `#[facet(default)]` are default-initialized when missing.
- `Option` fields implicitly default to `None`.
- Missing fields without defaults are errors.
- A struct's own `Default` is not used to fill missing fields in a partial.

### Errors and poisoning

Any error poisons the Trame. After poisoning, all subsequent operations return
`Poisoned`.

### Build

`build()` climbs to the root, performing `End` semantics as it climbs. If it
exits a deferred subtree, that subtree is validated in full. If any incomplete
nodes remain, `build()` errors. On success, it returns the fully constructed
value.

## Normative State Rules

> `t[state.init.byte-tracking]` Trame MUST track initialization state for each
> node/slot it manages and MUST only treat a node as complete when all required
> fields/slots are initialized by value, default, or a finalized child.

> `t[state.machine.strict-end]` In strict mode, `End` on an incomplete node
> MUST fail and MUST poison the Trame.

> `t[state.machine.deferred-end]` In deferred mode, `End` on an incomplete node
> MUST return to the parent without folding, and validation MUST be deferred
> until exiting the deferred subtree or `build()`.

> `t[state.machine.overwrite]` Applying `Imm` or `Default` over an existing
> initialized subtree MUST drop the replaced initialized portion before writing
> replacement bytes.

> `t[state.machine.container-append-stage-only]` `Append` MUST accept only
> staging sources (`Stage` / deferred-stage variants). `Imm` and `Default` at
> `Append` MUST fail.

> `t[state.machine.container-stable-staging]` List/set/map incremental
> construction MUST use stable staging storage for in-progress elements/entries.
> Staged addresses MUST remain valid across capacity growth.

> `t[state.machine.container-no-live-partial]` Trame MUST NOT expose
> partially initialized user elements inside the live destination container
> representation during incremental construction.

> `t[state.machine.container-materialize-on-close]` Container elements/entries
> staged through `Append` MUST be materialized into the final runtime container
> only when the container closes/finalizes (strict close or deferred final
> validation), not per-element during staging.

> `t[state.machine.container-capacity-hint]` Capacity hints for staged
> containers MUST be semantic no-ops (performance-only): changing hints MUST NOT
> change accepted/rejected inputs or final value contents.

> `t[state.machine.map-last-wins]` For map materialization, if multiple staged
> entries resolve to equal keys, the final map MUST contain the value from the
> latest staged entry for that key ("last wins").

> `t[state.machine.set-uniqueness]` Set materialization MUST enforce set
> uniqueness. Duplicate staged values MAY appear during staging but MUST collapse
> according to set semantics in the final set.

> `t[state.machine.enum-select-writes-discriminant]` Selecting enum variant
> `n` via path navigation MUST perform the enum transition immediately by
> establishing discriminant/tag for `n` before payload writes.

> `t[state.machine.enum-switch-drops-previous]` Switching from one active enum
> variant to another MUST drop the previously active initialized payload before
> the new payload is constructed.

> `t[state.machine.enum-direct-then-switch]` If enum bytes were established via
> whole-value direct set (`Set` at root/enum node), a later variant switch MUST
> first drop the old whole enum value before writing the new variant payload.

> `t[state.machine.poison-cleanup]` Once poisoned, Trame MUST reject further
> operations and MUST perform cleanup of all tracked initialized data and owned
> allocations exactly once.

> `t[state.machine.cleanup-no-double-free]` Cleanup traversal MUST NOT recurse
> into child subtrees that are already semantically owned and dropped by a
> parent `drop_in_place` path, to avoid double drop/deallocation.

> `t[state.machine.build-finalization]` `build()` MUST be equivalent to
> repeatedly applying `End` to the root (including deferred-subtree validation),
> and MUST only succeed when no incomplete tracked nodes remain.

## Verification Abstractions

Trame is parameterized over a small set of interfaces so the same construction
logic can run against a real implementation or a verified one. This is how the
project proves safety properties without changing core logic.

### Shape

Production uses `&'static Shape`. Verification uses a bounded dynamic shape
store that implements the same `IShape` interface. The point of `IShape` is to
let `Trame` and `Heap` operate over "a shape" without caring whether it is a
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
