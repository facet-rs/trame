# Pointer-Based Backend Design

This document describes the pointer-based backend design for trame's formally verified partial value construction.

## Problem Statement

The current implementation has `begin_field` calling `backend.alloc()` for nested structs. This is wrong - inline nested structs should live at an offset within the parent's allocation, not in a separate allocation.

Issue: https://github.com/facet-rs/trame/issues/3

## Core Insight

The Backend abstraction should model what actually happens in production:
- Allocations are contiguous byte regions
- Pointers are addresses into those regions
- Initialization happens via `memcpy`, not explicit `mark_init` calls
- The verified backend catches safety bugs (double-init, use-after-free, leaks) by tracking byte ranges

## Data Structures

### Ptr (Fat Pointer)

```rust
#[derive(Clone, Copy)]
struct Ptr {
    /// Which allocation this points into.
    alloc_id: u8,
    /// Current byte offset within the allocation.
    offset: u32,
    /// Total size of the allocation (for bounds checking).
    size: u32,
}

impl Ptr {
    /// Compute a new pointer at an offset from this one.
    /// Panics if resulting offset would exceed allocation size.
    fn offset(self, n: usize) -> Self {
        let new_offset = self.offset + n as u32;
        assert!(new_offset <= self.size, "pointer arithmetic out of bounds");
        Ptr {
            alloc_id: self.alloc_id,
            offset: new_offset,
            size: self.size,
        }
    }
    
    /// Returns the current offset within the allocation.
    fn offset_bytes(self) -> usize {
        self.offset as usize
    }
}
```

Key property: `Ptr` is `Copy` and contains all information needed for bounds checking. It doesn't borrow the Heap.

### IShape Trait (updated)

The `IShape` trait needs `PartialEq` so the Heap can verify shapes match, and `drop_in_place` for destruction:

```rust
pub trait IShape: Copy + PartialEq {
    type StructType: IStructType<Field = Self::Field>;
    type Field: IField<Shape = Self>;

    fn layout(&self) -> Layout;
    fn is_struct(&self) -> bool;
    fn as_struct(&self) -> Option<Self::StructType>;
    
    /// Drop the value at the given pointer.
    /// For types without destructors, this is a no-op.
    /// 
    /// # Safety
    /// - `ptr` must point to a valid, initialized value of this shape's type
    unsafe fn drop_in_place(&self, ptr: *mut u8);
}
```

For `&'static Shape`, this calls the vtable's `drop_in_place` function.
For `DynShapeView` (Kani verification), this is a no-op (we only track state, not actual values).

### Heap Trait

```rust
trait Heap<S: IShape> {
    type Ptr: Copy;
    
    /// Allocate a region for a value of the given shape.
    /// Returns a pointer to the start of the allocation.
    /// The entire region starts as uninitialized.
    /// The shape is stored for verification on dealloc/drop_in_place.
    fn alloc(&mut self, shape: S) -> Self::Ptr;
    
    /// Deallocate a region.
    /// The pointer must point to the start of an allocation (offset 0).
    /// The entire region must be uninitialized (all drops completed).
    /// Shape must match the shape passed to alloc.
    fn dealloc(&mut self, ptr: Self::Ptr, shape: S);
    
    /// Copy `len` bytes from `src` to `dst`.
    /// - `dst` range must be uninitialized (catches forgotten drops)
    /// - `src` range must be initialized (catches use of uninit memory)
    /// - After copy, `dst` range is marked initialized
    /// - `src` range remains initialized (it's a copy, not a move)
    fn memcpy(&mut self, dst: Self::Ptr, src: Self::Ptr, len: usize);
    
    /// Drop the value at `ptr`.
    /// - Range must be initialized
    /// - After drop, range is marked uninitialized
    /// - Shape must match the shape passed to alloc
    /// 
    /// For RealHeap: calls the shape's drop_in_place vtable function.
    /// For VerifiedHeap: verifies shape matches, then updates ByteRangeTracker.
    fn drop_in_place(&mut self, ptr: Self::Ptr, shape: S);
}
```

The Heap stores the shape alongside each allocation. On `dealloc` and `drop_in_place`, it verifies the shape matches. This catches bugs where you deallocate or drop with the wrong type.

### VerifiedHeap

```rust
const MAX_ALLOCS: usize = 8;

struct VerifiedHeap<S: IShape> {
    /// Each allocation's initialization state and shape.
    /// None = freed, Some((tracker, shape)) = live allocation.
    allocs: [Option<(ByteRangeTracker, S)>; MAX_ALLOCS],
    /// Next allocation ID to assign.
    next_id: u8,
}

impl<S: IShape> Heap<S> for VerifiedHeap<S> {
    type Ptr = Ptr;
    
    fn alloc(&mut self, shape: S) -> Ptr {
        let id = self.next_id;
        assert!((id as usize) < MAX_ALLOCS, "too many allocations");
        
        let layout = shape.layout();
        self.allocs[id as usize] = Some((ByteRangeTracker::new(), shape));
        self.next_id += 1;
        
        Ptr {
            alloc_id: id,
            offset: 0,
            size: layout.size() as u32,
        }
    }
    
    fn dealloc(&mut self, ptr: Ptr, shape: S) {
        assert_eq!(ptr.offset, 0, "dealloc requires pointer to allocation start");
        
        let (tracker, stored_shape) = self.allocs[ptr.alloc_id as usize]
            .as_ref()
            .expect("dealloc: allocation already freed");
        
        assert_eq!(*stored_shape, shape, "dealloc: shape mismatch");
        assert!(tracker.is_empty(), "dealloc: allocation still has initialized bytes");
        
        self.allocs[ptr.alloc_id as usize] = None;
    }
    
    fn memcpy(&mut self, dst: Ptr, src: Ptr, len: usize) {
        // Bounds checks
        assert!(dst.offset as usize + len <= dst.size as usize, "dst out of bounds");
        assert!(src.offset as usize + len <= src.size as usize, "src out of bounds");
        
        // Check src is initialized
        let (src_tracker, _) = self.allocs[src.alloc_id as usize]
            .as_ref()
            .expect("memcpy: src allocation freed");
        assert!(
            src_tracker.is_init(src.offset, src.offset + len as u32),
            "memcpy: src range not initialized"
        );
        
        // Check dst is uninitialized (catches forgotten drops!)
        let (dst_tracker, _) = self.allocs[dst.alloc_id as usize]
            .as_mut()
            .expect("memcpy: dst allocation freed");
        dst_tracker
            .mark_init(dst.offset, dst.offset + len as u32)
            .expect("memcpy: dst range already initialized (forgot to drop?)");
    }
    
    fn drop_in_place(&mut self, ptr: Ptr, shape: S) {
        let (tracker, stored_shape) = self.allocs[ptr.alloc_id as usize]
            .as_mut()
            .expect("drop_in_place: allocation freed");
        
        assert_eq!(*stored_shape, shape, "drop_in_place: shape mismatch");
        
        let layout = shape.layout();
        tracker
            .mark_uninit(ptr.offset, ptr.offset + layout.size() as u32)
            .expect("drop_in_place: range not initialized");
    }
}
```

### RealHeap

```rust
struct RealHeap;

impl<S: IShape> Heap<S> for RealHeap {
    type Ptr = *mut u8;
    
    fn alloc(&mut self, shape: S) -> *mut u8 {
        let layout = shape.layout();
        if layout.size() == 0 {
            layout.align() as *mut u8  // Dangling but aligned
        } else {
            unsafe { std::alloc::alloc(layout) }
        }
    }
    
    fn dealloc(&mut self, ptr: *mut u8, shape: S) {
        let layout = shape.layout();
        if layout.size() > 0 {
            unsafe { std::alloc::dealloc(ptr, layout) }
        }
    }
    
    fn memcpy(&mut self, dst: *mut u8, src: *mut u8, len: usize) {
        unsafe {
            std::ptr::copy_nonoverlapping(src, dst, len);
        }
    }
    
    fn drop_in_place(&mut self, ptr: *mut u8, shape: S) {
        // Use the shape's drop_in_place method
        unsafe { shape.drop_in_place(ptr) }
    }
}
```

## Frame Structure

With this design, Frame holds a `Ptr`:

```rust
struct Frame<H: Heap, S: IShape> {
    /// Pointer to where this frame's data lives.
    data: H::Ptr,
    /// Shape of the value being built.
    shape: S,
    /// What kind of value and its init state.
    kind: FrameKind<Self>,
    /// Parent frame index.
    parent: Idx<Self>,
    /// Field index within parent (for end_field to know which field to mark).
    field_in_parent: u32,
}
```

For inline nested structs:
- Root frame: `data` points to start of allocation
- Child frame: `data = parent.data.offset(field.offset())` - same allocation, different offset
- No new allocation for child!

## Public API: Operations

Following trame-ref's design, the public API is operations:

```rust
enum PathSegment {
    Field(u32),  // Struct field, tuple element, array index, enum variant
    Append,      // New list/set element or map entry
    Root,        // Jump to root frame
}

type Path = &[PathSegment];

enum Source<'a> {
    Imm(Ptr),              // Copy bytes from this pointer
    Stage(Option<usize>),  // Push frame for incremental construction
    Default,               // Use Default::default()
}

enum Op<'a> {
    Set { dst: Path, src: Source<'a> },
    End,
}

impl Partial {
    fn apply(&mut self, op: Op) -> Result<(), Error>;
}
```

## How Operations Work

### `Set { dst: Path::field(n), src: Imm(value_ptr) }`

1. Get current frame
2. Look up field `n` in frame's shape to get offset and size
3. Compute `dst_ptr = frame.data.offset(field.offset())`
4. Call `backend.memcpy(dst_ptr, value_ptr, field.size())`
5. Mark field as complete in frame's tracking state

### `Set { dst: Path::field(n), src: Stage(None) }`

1. Get current frame
2. Look up field `n` to get offset and shape
3. Compute `child_ptr = frame.data.offset(field.offset())`
4. Create child frame with `data = child_ptr` (NO ALLOCATION!)
5. Push child frame, make it current

### `End`

1. Verify current frame is complete
2. Pop to parent frame
3. Mark the corresponding field in parent as complete

### Overwrite (already initialized field)

Per DESIGN.md, overwriting drops the old value first:

1. Check if field is already initialized
2. If so: `heap.drop_in_place(field_ptr, field_layout)`
3. Then proceed with the Set operation

The `VerifiedHeap` catches if we forget step 2: the `memcpy` will fail because the destination range is already initialized.

## Clarifications

### Where does source data come from?

For `Set { dst, src: Imm(ptr) }`, the source pointer comes from an allocation through the same Heap. The caller allocates space, writes their value there, then provides the pointer. This keeps the abstraction clean - all pointers are Heap pointers.

### How does `Default` work?

The `Default` source uses the shape's vtable to call the type's `default()` function. The shape (via `IShape`) provides access to vtables. The Heap doesn't know about this - it just sees the resulting `memcpy` or direct initialization.

### How does `drop_in_place` work with type info?

The `drop_in_place` on the Heap trait is for tracking initialization state. The actual drop logic (calling destructors) is handled by Partial using the shape's vtable. For `RealHeap`, the method is a no-op since it doesn't track state. For `VerifiedHeap`, it marks the range as uninitialized.

### What about the IShape system?

The `IShape` trait system stays exactly as-is. It provides:
- Layout information (for alloc/dealloc)
- Field offsets (for pointer arithmetic)
- Field shapes (for recursive construction)
- Vtables (for drop, default, etc.)

The Backend is shape-agnostic. Shape info lives in Frame/Partial.

## What This Fixes

**Issue #3**: `begin_field` no longer allocates. It just does pointer arithmetic:
```rust
child_ptr = parent_ptr.offset(field.offset())
```

Same allocation ID, different offset. The `ByteRangeTracker` tracks which byte ranges within that single allocation are initialized.

## Verification Properties

The `VerifiedBackend` + `ByteRangeTracker` combination verifies:

1. **No double-init**: `memcpy` to already-initialized range fails
2. **No use-after-free**: `memcpy` from uninitialized range fails  
3. **No leaks**: `dealloc` with initialized bytes fails
4. **No out-of-bounds**: Pointer arithmetic checks bounds
5. **Proper drop discipline**: Must drop before overwrite

Kani can prove these properties hold for all possible operation sequences within bounded parameters.

## Migration Path

1. ✅ Create `ByteRangeTracker` with tests and proofs
2. Refactor `Backend` trait to use `Ptr` and `memcpy` model
3. Update `VerifiedBackend` to use `ByteRangeTracker`
4. Refactor `Partial` to use `Op`/`Path`/`Source` API
5. Remove old `begin_field`/`mark_field_init`/`end_field` API
6. Update existing tests and proofs
7. Add new proofs for nested struct scenarios
