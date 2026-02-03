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
are finalized. A node is **structurally complete** when all of its fields are
initialized directly or via sealed child nodes.

### Heap Semantics

> r[t.heap.alloc]
>
> `alloc(shape)` MUST return a pointer to a contiguous allocation for `shape`,
> and all bytes in that allocation MUST start uninitialized.

> r[t.heap.memcpy]
>
> `memcpy(dst, src, len)` MUST require the source range to be initialized and
> the destination range to be uninitialized. After the copy, the destination
> range MUST be initialized and the source range MUST remain initialized.

> r[t.heap.mark_init]
>
> `mark_init(ptr, len)` MUST require the range to be uninitialized and MUST
> mark the range initialized.

> r[t.heap.drop]
>
> `drop_in_place(ptr, shape)` MUST require the shape-matching subrange to be
> initialized and MUST mark exactly that subrange uninitialized.

> r[t.heap.dealloc]
>
> `dealloc(ptr, shape)` MUST require `ptr` to point to the allocation start,
> the shape to match the allocation, and the full allocation to be uninitialized.

### Node Semantics

> r[t.node.structural_complete]
>
> A node MUST be considered structurally complete iff all of its fields are
> initialized directly or via sealed child nodes.

> r[t.node.sealed_requires_complete]
>
> A node MUST be structurally complete before it can be sealed.

### Data Model

Construction state is represented as nodes stored in an arena. Each node has a
shape, a pointer, and a kind: **Scalar** or **Struct**. Scalar nodes track a
single initialization bit. Struct nodes track per-field slots: Untracked,
Initialized, or Child (staged or sealed).

A `Trame` maintains a cursor to the current node. Operations are relative to
this cursor unless the path begins with `Root`.

### Operations and Paths

> r[t.ops.set]
>
> `Op::Set` MUST write a value into the current node (or a field within it):
> - `Source::Imm` copies bytes from a pointer and marks the destination initialized
> - `Source::Default` constructs a default value in place and marks it initialized
> - `Source::Stage` enters a nested struct node for incremental construction
>   and is valid only when the target node is staged

> r[t.ops.end]
>
> `Op::End` MUST seal the current node and move the cursor to its parent.
> It MUST fail if the current node is not structurally complete or if it is the root.

> r[t.paths]
>
> Paths MUST be resolved relative to the current node. If the first segment is
> `Root`, the cursor MUST be reset to the root before resolving the remainder.
> Only a single `Field(n)` segment is supported; other path segments MUST be rejected.

### Safety Requirements

> r[t.safety]
>
> The following are prohibited and MUST be prevented:
> - Reading or copying from uninitialized memory
> - Dropping uninitialized memory
> - Dropping the same bytes twice (double-drop)
> - Deallocating memory while any bytes are still initialized
> - Using pointers after deallocation (use-after-free)
> - Performing out-of-bounds pointer arithmetic
> - Using a mismatched shape when dropping (drop wrong type/size)
> - Calling `copy_nonoverlapping` on overlapping ranges
> - Forgetting to drop initialized subranges (memory leaks)

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

These checks are enforced:

> r[t.verify.heap.alloc]
>
> `alloc` MUST create a new allocation with all bytes uninitialized and record
> the allocation's shape for later verification.

> r[t.verify.heap.dealloc]
>
> `dealloc` MUST reject pointers that are not at allocation start, MUST reject
> mismatched shapes, and MUST reject deallocation if any bytes remain initialized.

> r[t.verify.heap.memcpy]
>
> `memcpy` MUST bounds-check both source and destination ranges, MUST require
> the source range to be initialized, and MUST require the destination range to
> be uninitialized before marking it initialized.

> r[t.verify.heap.drop]
>
> `drop_in_place` MUST bounds-check the target range, MUST verify the shape
> matches the allocation (or subshape), and MUST mark exactly the target range
> uninitialized.

> r[t.verify.heap.mark_init]
>
> `mark_init` MUST bounds-check the target range and MUST reject double-init.

> r[t.verify.heap.leaks]
>
> The verified heap MUST be able to assert that all allocations are freed
> (no leaks) when requested by tests.

### Arena

The arena is also abstracted for verification. Production uses a growable
arena; verification uses a fixed-size arena with explicit occupancy checks.

### Zero-Cost Swap

The `Trame` type is instantiated with either the real implementations or the
verified ones. This makes verification a compile-time swap with zero runtime
cost in production builds.
