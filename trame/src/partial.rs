//! Partial value construction.
//!
//! `Partial` manages incremental construction of a value, tracking which
//! fields have been initialized and ensuring proper cleanup on failure.

use crate::arena::{Arena, Idx};
use crate::backend::Backend;
use crate::dyn_shape::{IField, IShape, IStructType};
use core::marker::PhantomData;

/// Maximum fields tracked per frame (for verification).
pub const MAX_FRAME_FIELDS: usize = 8;

// ============================================================================
// FieldState - tracks which fields are initialized
// ============================================================================

/// Tracks initialization state of struct fields.
///
/// Each field slot is either:
/// - `Idx::NOT_STARTED` - field not touched
/// - `Idx::COMPLETE` - field fully initialized
/// - Valid `Idx<Frame>` - in-progress child frame
#[derive(Debug, Clone, Copy)]
pub struct FieldStates<F> {
    /// State of each field.
    slots: [Idx<F>; MAX_FRAME_FIELDS],
    /// Number of fields in the struct.
    pub count: u8,
}

impl<F> FieldStates<F> {
    /// Create field states for a struct with `count` fields.
    pub fn new(count: usize) -> Self {
        assert!(count <= MAX_FRAME_FIELDS, "too many fields");
        Self {
            slots: [Idx::NOT_STARTED; MAX_FRAME_FIELDS],
            count: count as u8,
        }
    }

    /// Mark a field as complete (initialized).
    pub fn mark_complete(&mut self, idx: usize) {
        debug_assert!(idx < self.count as usize);
        self.slots[idx] = Idx::COMPLETE;
    }

    /// Mark a field as not started.
    pub fn mark_not_started(&mut self, idx: usize) {
        debug_assert!(idx < self.count as usize);
        self.slots[idx] = Idx::NOT_STARTED;
    }

    /// Set a field's child frame index.
    pub fn set_child(&mut self, idx: usize, child: Idx<F>) {
        debug_assert!(idx < self.count as usize);
        self.slots[idx] = child;
    }

    /// Get a field's child frame index, if it has one.
    pub fn get_child(&self, idx: usize) -> Option<Idx<F>> {
        debug_assert!(idx < self.count as usize);
        let slot = self.slots[idx];
        if slot.is_valid() { Some(slot) } else { None }
    }

    /// Check if a field is complete (initialized).
    pub fn is_init(&self, idx: usize) -> bool {
        debug_assert!(idx < self.count as usize);
        self.slots[idx].is_complete()
    }

    /// Check if a field is not started.
    pub fn is_not_started(&self, idx: usize) -> bool {
        debug_assert!(idx < self.count as usize);
        self.slots[idx] == Idx::NOT_STARTED
    }

    /// Check if all fields are complete.
    pub fn all_init(&self) -> bool {
        (0..self.count as usize).all(|i| self.slots[i].is_complete())
    }

    /// Iterate over complete field indices.
    pub fn iter_init(&self) -> impl Iterator<Item = usize> + '_ {
        (0..self.count as usize).filter(|&i| self.slots[i].is_complete())
    }
}

// ============================================================================
// FrameKind - what kind of value we're building
// ============================================================================

/// What kind of value a frame is building.
#[derive(Debug, Clone, Copy)]
pub enum FrameKind<F> {
    /// Scalar value (no internal structure).
    Scalar { initialized: bool },
    /// Struct with tracked fields.
    Struct { fields: FieldStates<F> },
}

impl<F> FrameKind<F> {
    /// Check if the frame is fully initialized.
    pub fn is_complete(&self) -> bool {
        match self {
            FrameKind::Scalar { initialized } => *initialized,
            FrameKind::Struct { fields } => fields.all_init(),
        }
    }
}

// ============================================================================
// Frame - a node in the construction tree
// ============================================================================

/// A frame tracking construction of a single value.
pub struct Frame<B: Backend<S>, S: IShape> {
    /// The allocation this frame's data lives in.
    pub alloc: B::Alloc,
    /// Shape of the value being built.
    pub shape: S,
    /// What kind of value and its init state.
    pub kind: FrameKind<Self>,
    /// Parent frame index (NOT_STARTED if root).
    pub parent: Idx<Self>,
    /// Field index within parent (if applicable).
    pub field_in_parent: u32,
}

impl<B: Backend<S>, S: IShape> Frame<B, S> {
    /// Create the appropriate FrameKind for a shape.
    fn kind_for_shape(shape: S) -> FrameKind<Self> {
        if let Some(st) = shape.as_struct() {
            FrameKind::Struct {
                fields: FieldStates::new(st.field_count()),
            }
        } else {
            FrameKind::Scalar { initialized: false }
        }
    }

    /// Create a new root frame.
    pub fn new_root(alloc: B::Alloc, shape: S) -> Self {
        Self {
            alloc,
            shape,
            kind: Self::kind_for_shape(shape),
            parent: Idx::NOT_STARTED,
            field_in_parent: 0,
        }
    }
}

// ============================================================================
// Partial - the main builder
// ============================================================================

/// Manages incremental construction of a value.
pub struct Partial<B, A, S>
where
    B: Backend<S>,
    A: Arena<Frame<B, S>>,
    S: IShape,
{
    /// The backend for memory operations.
    backend: B,
    /// Arena holding frames.
    arena: A,
    /// Root frame index.
    root: Idx<Frame<B, S>>,
    /// Current frame we're building.
    current: Idx<Frame<B, S>>,
    /// Whether we've been poisoned (error occurred).
    poisoned: bool,
    /// Marker for shape type.
    _marker: PhantomData<S>,
}

/// Error during partial construction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PartialError {
    /// Tried to operate on a poisoned Partial.
    Poisoned,
    /// Field index out of bounds.
    FieldOutOfBounds { index: usize, count: usize },
    /// Field already initialized.
    FieldAlreadyInit { index: usize },
    /// Not all fields initialized when trying to build.
    Incomplete,
    /// Current frame is not a struct.
    NotAStruct,
    /// Current frame is not complete (for end_field).
    CurrentIncomplete,
    /// Already at root frame (can't end_field).
    AtRoot,
    /// Shape mismatch.
    ShapeMismatch,
}

impl<B, A, S> Partial<B, A, S>
where
    B: Backend<S>,
    A: Arena<Frame<B, S>>,
    S: IShape,
{
    /// Create a new Partial for the given shape.
    ///
    /// # Safety
    /// The caller must ensure the shape is valid and sized.
    pub unsafe fn new(mut backend: B, mut arena: A, shape: S) -> Self {
        let alloc = unsafe { backend.alloc(shape) };
        let frame = Frame::new_root(alloc, shape);
        let root = arena.alloc(frame);

        Self {
            backend,
            arena,
            root,
            current: root,
            poisoned: false,
            _marker: PhantomData,
        }
    }

    /// Check if poisoned, return error if so.
    fn check_poisoned(&self) -> Result<(), PartialError> {
        if self.poisoned {
            Err(PartialError::Poisoned)
        } else {
            Ok(())
        }
    }

    /// Mark a struct field as initialized (for scalar fields).
    ///
    /// Use this for fields that don't need recursive construction.
    /// For nested structs, use `begin_field` / `end_field` instead.
    ///
    /// # Safety
    /// The caller must have actually written a valid value to the field's memory.
    pub unsafe fn mark_field_init(&mut self, field_idx: usize) -> Result<(), PartialError> {
        self.check_poisoned()?;

        let frame = self.arena.get_mut(self.current);

        match &mut frame.kind {
            FrameKind::Struct { fields } => {
                if field_idx >= fields.count as usize {
                    return Err(PartialError::FieldOutOfBounds {
                        index: field_idx,
                        count: fields.count as usize,
                    });
                }
                if !fields.is_not_started(field_idx) {
                    return Err(PartialError::FieldAlreadyInit { index: field_idx });
                }

                // Mark in backend
                let alloc = frame.alloc;
                let slot = unsafe { self.backend.slot(alloc, field_idx) };
                unsafe { self.backend.mark_init(slot) };

                // Mark in frame as complete
                let frame = self.arena.get_mut(self.current);
                if let FrameKind::Struct { fields } = &mut frame.kind {
                    fields.mark_complete(field_idx);
                }

                Ok(())
            }
            FrameKind::Scalar { .. } => Err(PartialError::NotAStruct),
        }
    }

    /// Mark a scalar as initialized.
    ///
    /// # Safety
    /// The caller must have actually written a valid value to the memory.
    pub unsafe fn mark_scalar_init(&mut self) -> Result<(), PartialError> {
        self.check_poisoned()?;

        let frame = self.arena.get_mut(self.current);

        match &mut frame.kind {
            FrameKind::Scalar { initialized } => {
                if *initialized {
                    return Err(PartialError::FieldAlreadyInit { index: 0 });
                }

                // Mark in backend
                let alloc = frame.alloc;
                let slot = unsafe { self.backend.slot(alloc, 0) };
                unsafe { self.backend.mark_init(slot) };

                // Mark in frame
                let frame = self.arena.get_mut(self.current);
                if let FrameKind::Scalar { initialized } = &mut frame.kind {
                    *initialized = true;
                }

                Ok(())
            }
            FrameKind::Struct { .. } => Err(PartialError::NotAStruct),
        }
    }

    /// Begin constructing a struct field.
    ///
    /// This pushes a new frame for the field and makes it current.
    /// The field must be a struct type. For scalar fields, use `mark_field_init` directly.
    ///
    /// # Safety
    /// The caller must ensure the field is a struct type that needs recursive construction.
    pub unsafe fn begin_field(&mut self, field_idx: usize) -> Result<(), PartialError> {
        self.check_poisoned()?;

        let frame = self.arena.get(self.current);

        // Check current frame is a struct
        let field_count = match &frame.kind {
            FrameKind::Struct { fields } => fields.count as usize,
            FrameKind::Scalar { .. } => return Err(PartialError::NotAStruct),
        };

        // Check field bounds
        if field_idx >= field_count {
            return Err(PartialError::FieldOutOfBounds {
                index: field_idx,
                count: field_count,
            });
        }

        // Check field not already initialized or in-progress
        let frame = self.arena.get(self.current);
        if let FrameKind::Struct { fields } = &frame.kind {
            if !fields.is_not_started(field_idx) {
                return Err(PartialError::FieldAlreadyInit { index: field_idx });
            }
        }

        // Get the field's shape
        let frame = self.arena.get(self.current);
        let parent_shape = frame.shape;
        let field_shape = parent_shape
            .as_struct()
            .and_then(|st| st.field(field_idx))
            .map(|f| f.shape())
            .ok_or(PartialError::FieldOutOfBounds {
                index: field_idx,
                count: field_count,
            })?;

        // Allocate for the nested struct (or reuse parent's allocation at offset)
        // For now, we allocate separately - in production we'd compute offset
        let child_alloc = unsafe { self.backend.alloc(field_shape) };

        // Create child frame
        let child_frame = Frame {
            alloc: child_alloc,
            shape: field_shape,
            kind: Frame::<B, S>::kind_for_shape(field_shape),
            parent: self.current,
            field_in_parent: field_idx as u32,
        };

        // Push onto arena
        let child_idx = self.arena.alloc(child_frame);

        // Track the child in parent's field slots
        let frame = self.arena.get_mut(self.current);
        if let FrameKind::Struct { fields } = &mut frame.kind {
            fields.set_child(field_idx, child_idx);
        }

        // Update current
        self.current = child_idx;

        Ok(())
    }

    /// End constructing the current field and return to parent.
    ///
    /// This marks the field as initialized in the parent frame and pops back.
    /// The current frame must be complete.
    pub fn end_field(&mut self) -> Result<(), PartialError> {
        self.check_poisoned()?;

        let frame = self.arena.get(self.current);

        // Check current frame is complete
        if !frame.kind.is_complete() {
            return Err(PartialError::CurrentIncomplete);
        }

        // Check we're not at root
        let parent_idx = frame.parent;
        if !parent_idx.is_valid() {
            return Err(PartialError::AtRoot);
        }

        let field_in_parent = frame.field_in_parent as usize;

        // Mark the field as complete in parent
        let parent = self.arena.get_mut(parent_idx);
        if let FrameKind::Struct { fields } = &mut parent.kind {
            fields.mark_complete(field_in_parent);
        }

        // Mark in backend
        let parent_alloc = parent.alloc;
        let slot = unsafe { self.backend.slot(parent_alloc, field_in_parent) };
        unsafe { self.backend.mark_init(slot) };

        // Pop back to parent
        self.current = parent_idx;

        Ok(())
    }

    /// Get the current nesting depth (0 = root).
    pub fn depth(&self) -> usize {
        let mut depth = 0;
        let mut idx = self.current;
        while idx.is_valid() {
            let frame = self.arena.get(idx);
            if !frame.parent.is_valid() {
                break;
            }
            depth += 1;
            idx = frame.parent;
        }
        depth
    }

    /// Check if the current frame is complete.
    pub fn is_complete(&self) -> bool {
        if self.poisoned {
            return false;
        }
        let frame = self.arena.get(self.current);
        frame.kind.is_complete()
    }

    /// Finish building, returning ownership of backend and arena.
    ///
    /// Returns error if not all fields are initialized.
    pub fn finish(self) -> Result<(B, A, B::Alloc), PartialError> {
        self.check_poisoned()?;

        let frame = self.arena.get(self.current);
        if !frame.kind.is_complete() {
            return Err(PartialError::Incomplete);
        }

        let alloc = frame.alloc;

        // Don't run Drop - we're transferring ownership
        let backend = unsafe { core::ptr::read(&self.backend) };
        let arena = unsafe { core::ptr::read(&self.arena) };
        core::mem::forget(self);

        Ok((backend, arena, alloc))
    }

    /// Poison the partial, cleaning up all initialized fields.
    fn poison(&mut self) {
        if self.poisoned {
            return;
        }
        self.poisoned = true;

        // Clean up from root, depth-first (leaves before parents)
        self.cleanup_frame(self.root);
    }

    /// Recursively clean up a frame and all its children (depth-first).
    fn cleanup_frame(&mut self, idx: Idx<Frame<B, S>>) {
        if !idx.is_valid() {
            return;
        }

        // Collect children first to avoid borrow issues
        let mut children = [Idx::NOT_STARTED; MAX_FRAME_FIELDS];
        let mut child_count = 0;

        {
            let frame = self.arena.get(idx);
            if let FrameKind::Struct { fields } = &frame.kind {
                for i in 0..(fields.count as usize) {
                    if let Some(child_idx) = fields.get_child(i) {
                        children[child_count] = child_idx;
                        child_count += 1;
                    }
                }
            }
        }

        // Recursively clean up children first (depth-first)
        for i in 0..child_count {
            self.cleanup_frame(children[i]);
        }

        // Now clean up this frame's initialized slots
        let frame = self.arena.get(idx);
        let alloc = frame.alloc;

        match &frame.kind {
            FrameKind::Scalar { initialized: true } => {
                let slot = unsafe { self.backend.slot(alloc, 0) };
                unsafe { self.backend.mark_uninit(slot) };
            }
            FrameKind::Struct { fields } => {
                for i in fields.iter_init() {
                    let slot = unsafe { self.backend.slot(alloc, i) };
                    unsafe { self.backend.mark_uninit(slot) };
                }
            }
            _ => {}
        }

        // Dealloc this frame
        unsafe { self.backend.dealloc(alloc) };
    }
}

impl<B, A, S> Drop for Partial<B, A, S>
where
    B: Backend<S>,
    A: Arena<Frame<B, S>>,
    S: IShape,
{
    fn drop(&mut self) {
        if !self.poisoned {
            self.poison();
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arena::VerifiedArena;
    use crate::backend::VerifiedBackend;
    use crate::dyn_shape::{DynShapeDef, DynShapeStore, DynShapeView};
    use core::alloc::Layout;

    type S<'a> = DynShapeView<'a, DynShapeStore>;
    type TestBackend<'a> = VerifiedBackend<S<'a>>;
    type TestArena<'a> = VerifiedArena<Frame<TestBackend<'a>, S<'a>>, 8>;

    #[test]
    fn scalar_lifecycle() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let shape = store.view(h);

        let backend = TestBackend::new();
        let arena = TestArena::new();

        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        assert!(!partial.is_complete());
        unsafe { partial.mark_scalar_init().unwrap() };
        assert!(partial.is_complete());

        let (_, _, _alloc) = partial.finish().unwrap();
    }

    #[test]
    fn struct_lifecycle() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();

        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        assert!(!partial.is_complete());

        unsafe { partial.mark_field_init(0).unwrap() };
        assert!(!partial.is_complete());

        unsafe { partial.mark_field_init(1).unwrap() };
        assert!(partial.is_complete());

        let (_, _, _alloc) = partial.finish().unwrap();
    }

    #[test]
    fn struct_any_order() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def =
            DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h), (8, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();

        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        // Init in reverse order
        unsafe { partial.mark_field_init(2).unwrap() };
        unsafe { partial.mark_field_init(0).unwrap() };
        unsafe { partial.mark_field_init(1).unwrap() };

        assert!(partial.is_complete());
        let _ = partial.finish().unwrap();
    }

    #[test]
    fn double_init_fails() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();

        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        unsafe { partial.mark_field_init(0).unwrap() };
        let err = unsafe { partial.mark_field_init(0) };
        assert_eq!(err, Err(PartialError::FieldAlreadyInit { index: 0 }));
    }

    #[test]
    fn incomplete_finish_fails() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();

        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        unsafe { partial.mark_field_init(0).unwrap() };
        // Don't init field 1

        let err = partial.finish();
        assert!(matches!(err, Err(PartialError::Incomplete)));
    }

    #[test]
    fn drop_cleans_up() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();

        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        // Partial init
        unsafe { partial.mark_field_init(0).unwrap() };

        // Drop should clean up without panicking
        drop(partial);
    }

    // --- Nested struct tests ---

    #[test]
    fn nested_struct_begin_end() {
        let mut store = DynShapeStore::new();

        // Inner struct: { a: u32, b: u32 }
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let inner_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let inner_h = store.add(inner_def);

        // Outer struct: { x: u32, inner: Inner }
        let outer_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, inner_h)]);
        let outer_h = store.add(outer_def);
        let shape = store.view(outer_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();

        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        assert_eq!(partial.depth(), 0);
        assert!(!partial.is_complete());

        // Init scalar field x
        unsafe { partial.mark_field_init(0).unwrap() };
        assert!(!partial.is_complete());

        // Begin nested struct field
        unsafe { partial.begin_field(1).unwrap() };
        assert_eq!(partial.depth(), 1);
        assert!(!partial.is_complete()); // inner struct not complete

        // Init inner struct's fields
        unsafe { partial.mark_field_init(0).unwrap() }; // inner.a
        unsafe { partial.mark_field_init(1).unwrap() }; // inner.b
        assert!(partial.is_complete()); // inner struct complete

        // End nested struct
        partial.end_field().unwrap();
        assert_eq!(partial.depth(), 0);
        assert!(partial.is_complete()); // outer struct complete

        let _ = partial.finish().unwrap();
    }

    #[test]
    fn nested_struct_drop_cleans_up() {
        let mut store = DynShapeStore::new();

        // Inner struct: { a: u32 }
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let inner_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h)]);
        let inner_h = store.add(inner_def);

        // Outer struct: { inner: Inner }
        let outer_def = DynShapeDef::struct_with_fields(&store, &[(0, inner_h)]);
        let outer_h = store.add(outer_def);
        let shape = store.view(outer_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();

        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        // Begin nested struct but don't finish
        unsafe { partial.begin_field(0).unwrap() };
        unsafe { partial.mark_field_init(0).unwrap() }; // inner.a

        // Drop should clean up the child frame first, then root
        drop(partial);
        // No panic = success
    }

    #[test]
    fn nested_struct_partial_inner_cleanup() {
        let mut store = DynShapeStore::new();

        // Inner struct: { a: u32, b: u32 }
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let inner_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let inner_h = store.add(inner_def);

        // Outer struct: { inner: Inner }
        let outer_def = DynShapeDef::struct_with_fields(&store, &[(0, inner_h)]);
        let outer_h = store.add(outer_def);
        let shape = store.view(outer_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();

        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        // Begin nested struct, partially init
        unsafe { partial.begin_field(0).unwrap() };
        unsafe { partial.mark_field_init(0).unwrap() }; // inner.a only
        // inner.b NOT initialized

        // Drop should clean up correctly
        drop(partial);
    }

    #[test]
    fn begin_field_already_init_fails() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();

        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        // Init field 0 as scalar
        unsafe { partial.mark_field_init(0).unwrap() };

        // Now try to begin_field on it - should fail
        let err = unsafe { partial.begin_field(0) };
        assert_eq!(err, Err(PartialError::FieldAlreadyInit { index: 0 }));
    }

    #[test]
    fn end_field_incomplete_fails() {
        let mut store = DynShapeStore::new();

        // Inner struct: { a: u32, b: u32 }
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let inner_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let inner_h = store.add(inner_def);

        // Outer struct: { inner: Inner }
        let outer_def = DynShapeDef::struct_with_fields(&store, &[(0, inner_h)]);
        let outer_h = store.add(outer_def);
        let shape = store.view(outer_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();

        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        unsafe { partial.begin_field(0).unwrap() };
        unsafe { partial.mark_field_init(0).unwrap() }; // only inner.a
        // inner.b NOT initialized

        // Try to end_field - should fail because inner is incomplete
        let err = partial.end_field();
        assert_eq!(err, Err(PartialError::CurrentIncomplete));
    }

    #[test]
    fn end_field_at_root_fails() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();

        let mut partial = unsafe { Partial::new(backend, arena, shape) };
        unsafe { partial.mark_field_init(0).unwrap() };

        // Try to end_field at root - should fail
        let err = partial.end_field();
        assert_eq!(err, Err(PartialError::AtRoot));
    }
}

#[cfg(kani)]
mod kani_proofs {
    use super::*;
    use crate::arena::VerifiedArena;
    use crate::backend::VerifiedBackend;
    use crate::dyn_shape::{
        DynDef, DynFieldDef, DynShapeDef, DynShapeHandle, DynShapeStore, DynShapeView,
        DynStructDef, IField, IShape, IStructType, MAX_FIELDS,
    };
    use core::alloc::Layout;

    type S<'a> = DynShapeView<'a, DynShapeStore>;
    type TestBackend<'a> = VerifiedBackend<S<'a>>;
    type TestArena<'a> = VerifiedArena<Frame<TestBackend<'a>, S<'a>>, 4>;

    #[kani::proof]
    #[kani::unwind(10)]
    fn scalar_init_complete() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 4).unwrap()));
        let shape = store.view(h);

        let backend = TestBackend::new();
        let arena = TestArena::new();

        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        kani::assert(!partial.is_complete(), "not complete initially");
        unsafe { partial.mark_scalar_init().unwrap() };
        kani::assert(partial.is_complete(), "complete after init");
    }

    #[kani::proof]
    #[kani::unwind(10)]
    fn struct_all_fields_required() {
        let field_count: u8 = kani::any();
        kani::assume(field_count > 0 && field_count <= 3);

        let mut store = DynShapeStore::new();
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

        let mut fields_arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
        for i in 0..(field_count as usize) {
            fields_arr[i] = DynFieldDef::new(i * 4, scalar_h);
        }

        let struct_def = DynShapeDef {
            layout: Layout::from_size_align((field_count as usize) * 4, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count,
                fields: fields_arr,
            }),
        };
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        // Init all but one field
        let skip_field: u8 = kani::any();
        kani::assume(skip_field < field_count);

        for i in 0..(field_count as usize) {
            if i != skip_field as usize {
                unsafe { partial.mark_field_init(i).unwrap() };
            }
        }

        // Should not be complete
        kani::assert(!partial.is_complete(), "incomplete without all fields");

        // Init the skipped field
        unsafe { partial.mark_field_init(skip_field as usize).unwrap() };

        // Now should be complete
        kani::assert(partial.is_complete(), "complete with all fields");
    }

    /// Prove: double init of same field returns error
    #[kani::proof]
    #[kani::unwind(10)]
    fn double_init_rejected() {
        let mut store = DynShapeStore::new();
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

        let struct_def = DynShapeDef {
            layout: Layout::from_size_align(8, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 2,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, scalar_h);
                    arr[1] = DynFieldDef::new(4, scalar_h);
                    arr
                },
            }),
        };
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        let field_to_double: u8 = kani::any();
        kani::assume(field_to_double < 2);

        // First init succeeds
        let result1 = unsafe { partial.mark_field_init(field_to_double as usize) };
        kani::assert(result1.is_ok(), "first init succeeds");

        // Second init of same field fails
        let result2 = unsafe { partial.mark_field_init(field_to_double as usize) };
        kani::assert(result2.is_err(), "double init fails");
        kani::assert(
            matches!(result2, Err(PartialError::FieldAlreadyInit { .. })),
            "error is FieldAlreadyInit",
        );
    }

    /// Prove: finish fails if not all fields initialized
    #[kani::proof]
    #[kani::unwind(10)]
    fn incomplete_finish_fails() {
        let field_count: u8 = kani::any();
        kani::assume(field_count >= 2 && field_count <= 3);

        let mut store = DynShapeStore::new();
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

        let mut fields_arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
        for i in 0..(field_count as usize) {
            fields_arr[i] = DynFieldDef::new(i * 4, scalar_h);
        }

        let struct_def = DynShapeDef {
            layout: Layout::from_size_align((field_count as usize) * 4, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count,
                fields: fields_arr,
            }),
        };
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        // Init only first field
        unsafe { partial.mark_field_init(0).unwrap() };

        // finish should fail
        let result = partial.finish();
        kani::assert(result.is_err(), "finish fails when incomplete");
        kani::assert(
            matches!(result, Err(PartialError::Incomplete)),
            "error is Incomplete",
        );
    }

    /// Prove: field index out of bounds returns error
    #[kani::proof]
    #[kani::unwind(10)]
    fn out_of_bounds_rejected() {
        let field_count: u8 = kani::any();
        kani::assume(field_count > 0 && field_count <= 3);

        let mut store = DynShapeStore::new();
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

        let mut fields_arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
        for i in 0..(field_count as usize) {
            fields_arr[i] = DynFieldDef::new(i * 4, scalar_h);
        }

        let struct_def = DynShapeDef {
            layout: Layout::from_size_align((field_count as usize) * 4, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count,
                fields: fields_arr,
            }),
        };
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        // Try to init field beyond bounds
        let bad_idx: u8 = kani::any();
        kani::assume(bad_idx >= field_count);
        kani::assume(bad_idx < 10); // Keep bounded

        let result = unsafe { partial.mark_field_init(bad_idx as usize) };
        kani::assert(result.is_err(), "out of bounds fails");
        kani::assert(
            matches!(result, Err(PartialError::FieldOutOfBounds { .. })),
            "error is FieldOutOfBounds",
        );
    }

    /// Prove: any init order works for completion
    #[kani::proof]
    #[kani::unwind(10)]
    fn any_init_order_completes() {
        let mut store = DynShapeStore::new();
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

        let struct_def = DynShapeDef {
            layout: Layout::from_size_align(12, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 3,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, scalar_h);
                    arr[1] = DynFieldDef::new(4, scalar_h);
                    arr[2] = DynFieldDef::new(8, scalar_h);
                    arr
                },
            }),
        };
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        // Choose arbitrary init order
        let first: u8 = kani::any();
        let second: u8 = kani::any();
        let third: u8 = kani::any();
        kani::assume(first < 3 && second < 3 && third < 3);
        kani::assume(first != second && second != third && first != third);

        unsafe {
            partial.mark_field_init(first as usize).unwrap();
            partial.mark_field_init(second as usize).unwrap();
            partial.mark_field_init(third as usize).unwrap();
        }

        kani::assert(
            partial.is_complete(),
            "complete after all fields in any order",
        );

        let result = partial.finish();
        kani::assert(result.is_ok(), "finish succeeds when complete");
    }

    /// Prove: nested struct fields are properly tracked
    /// A struct containing another struct as a field should work correctly.
    #[kani::proof]
    #[kani::unwind(10)]
    fn nested_struct_field_tracking() {
        let mut store = DynShapeStore::new();

        // Inner struct: { a: u32, b: u32 }
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
        let inner_def = DynShapeDef {
            layout: Layout::from_size_align(8, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 2,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, scalar_h);
                    arr[1] = DynFieldDef::new(4, scalar_h);
                    arr
                },
            }),
        };
        let inner_h = store.add(inner_def);

        // Outer struct: { x: u32, inner: Inner }
        let outer_def = DynShapeDef {
            layout: Layout::from_size_align(12, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 2,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, scalar_h); // x: u32
                    arr[1] = DynFieldDef::new(4, inner_h); // inner: Inner (nested struct!)
                    arr
                },
            }),
        };
        let outer_h = store.add(outer_def);
        let shape = store.view(outer_h);

        // Verify shape structure via IShape trait
        kani::assert(shape.is_struct(), "outer is struct");
        let outer_st = shape.as_struct().unwrap();
        kani::assert(outer_st.field_count() == 2, "outer has 2 fields");

        // Field 1 should be the nested struct
        let field1 = outer_st.field(1).unwrap();
        let field1_shape = field1.shape();
        kani::assert(field1_shape.is_struct(), "field 1 is nested struct");

        // Construct the outer struct
        let backend = TestBackend::new();
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        // Init both fields (the nested struct field is treated as one unit at this level)
        unsafe {
            partial.mark_field_init(0).unwrap();
            partial.mark_field_init(1).unwrap();
        }

        kani::assert(partial.is_complete(), "outer complete after both fields");
        let result = partial.finish();
        kani::assert(result.is_ok(), "finish succeeds");
    }

    /// Prove: begin_field/end_field lifecycle works correctly
    #[kani::proof]
    #[kani::unwind(10)]
    fn begin_end_field_lifecycle() {
        let mut store = DynShapeStore::new();

        // Inner struct: { a: u32, b: u32 }
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
        let inner_def = DynShapeDef {
            layout: Layout::from_size_align(8, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 2,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, scalar_h);
                    arr[1] = DynFieldDef::new(4, scalar_h);
                    arr
                },
            }),
        };
        let inner_h = store.add(inner_def);

        // Outer struct: { x: u32, inner: Inner }
        let outer_def = DynShapeDef {
            layout: Layout::from_size_align(12, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 2,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, scalar_h);
                    arr[1] = DynFieldDef::new(4, inner_h);
                    arr
                },
            }),
        };
        let outer_h = store.add(outer_def);
        let shape = store.view(outer_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        // Verify initial state
        kani::assert(partial.depth() == 0, "starts at depth 0");
        kani::assert(!partial.is_complete(), "not complete initially");

        // Init scalar field
        unsafe { partial.mark_field_init(0).unwrap() };
        kani::assert(!partial.is_complete(), "not complete with one field");

        // Begin nested struct
        let result = unsafe { partial.begin_field(1) };
        kani::assert(result.is_ok(), "begin_field succeeds");
        kani::assert(partial.depth() == 1, "depth is 1 after begin");
        kani::assert(!partial.is_complete(), "inner not complete yet");

        // Init inner fields
        unsafe {
            partial.mark_field_init(0).unwrap();
            partial.mark_field_init(1).unwrap();
        }
        kani::assert(partial.is_complete(), "inner complete");

        // End nested struct
        let result = partial.end_field();
        kani::assert(result.is_ok(), "end_field succeeds");
        kani::assert(partial.depth() == 0, "back to depth 0");
        kani::assert(partial.is_complete(), "outer complete");

        let result = partial.finish();
        kani::assert(result.is_ok(), "finish succeeds");
    }

    /// Prove: begin_field on already-initialized field fails
    #[kani::proof]
    #[kani::unwind(10)]
    fn begin_field_already_init_fails() {
        let mut store = DynShapeStore::new();
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
        let inner_def = DynShapeDef {
            layout: Layout::from_size_align(4, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 1,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, scalar_h);
                    arr
                },
            }),
        };
        let inner_h = store.add(inner_def);

        let outer_def = DynShapeDef {
            layout: Layout::from_size_align(4, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 1,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, inner_h);
                    arr
                },
            }),
        };
        let outer_h = store.add(outer_def);
        let shape = store.view(outer_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        // Mark field 0 as init directly
        unsafe { partial.mark_field_init(0).unwrap() };

        // Now try begin_field on same field - should fail
        let result = unsafe { partial.begin_field(0) };
        kani::assert(result.is_err(), "begin on already init fails");
        kani::assert(
            matches!(result, Err(PartialError::FieldAlreadyInit { index: 0 })),
            "error is FieldAlreadyInit",
        );
    }

    /// Prove: end_field at root returns error
    #[kani::proof]
    #[kani::unwind(10)]
    fn end_field_at_root_fails() {
        let mut store = DynShapeStore::new();
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
        let struct_def = DynShapeDef {
            layout: Layout::from_size_align(4, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 1,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, scalar_h);
                    arr
                },
            }),
        };
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        unsafe { partial.mark_field_init(0).unwrap() };

        // Try end_field at root
        let result = partial.end_field();
        kani::assert(result.is_err(), "end_field at root fails");
        kani::assert(
            matches!(result, Err(PartialError::AtRoot)),
            "error is AtRoot",
        );
    }

    /// Prove: end_field with incomplete inner fails
    #[kani::proof]
    #[kani::unwind(10)]
    fn end_field_incomplete_inner_fails() {
        let mut store = DynShapeStore::new();
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
        let inner_def = DynShapeDef {
            layout: Layout::from_size_align(8, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 2,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, scalar_h);
                    arr[1] = DynFieldDef::new(4, scalar_h);
                    arr
                },
            }),
        };
        let inner_h = store.add(inner_def);

        let outer_def = DynShapeDef {
            layout: Layout::from_size_align(8, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 1,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, inner_h);
                    arr
                },
            }),
        };
        let outer_h = store.add(outer_def);
        let shape = store.view(outer_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        unsafe { partial.begin_field(0).unwrap() };
        unsafe { partial.mark_field_init(0).unwrap() }; // only one of two inner fields

        // Try end_field with incomplete inner
        let result = partial.end_field();
        kani::assert(result.is_err(), "end_field with incomplete inner fails");
        kani::assert(
            matches!(result, Err(PartialError::CurrentIncomplete)),
            "error is CurrentIncomplete",
        );
    }

    /// Prove: drop properly cleans up nested frames (depth-first)
    #[kani::proof]
    #[kani::unwind(12)]
    fn nested_drop_cleanup() {
        let mut store = DynShapeStore::new();
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
        let inner_def = DynShapeDef {
            layout: Layout::from_size_align(4, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 1,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, scalar_h);
                    arr
                },
            }),
        };
        let inner_h = store.add(inner_def);

        let outer_def = DynShapeDef {
            layout: Layout::from_size_align(4, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 1,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, inner_h);
                    arr
                },
            }),
        };
        let outer_h = store.add(outer_def);
        let shape = store.view(outer_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        // Enter nested struct
        unsafe { partial.begin_field(0).unwrap() };
        // Init inner field
        unsafe { partial.mark_field_init(0).unwrap() };
        // Don't end_field - just drop

        // Drop should clean up child frame before parent
        drop(partial);
        // No panic = cleanup order is correct
    }

    /// Prove: depth tracking is correct through nested begin/end
    #[kani::proof]
    #[kani::unwind(10)]
    fn depth_tracking_correct() {
        let mut store = DynShapeStore::new();
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
        let inner_def = DynShapeDef {
            layout: Layout::from_size_align(4, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 1,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, scalar_h);
                    arr
                },
            }),
        };
        let inner_h = store.add(inner_def);

        let outer_def = DynShapeDef {
            layout: Layout::from_size_align(4, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 1,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, inner_h);
                    arr
                },
            }),
        };
        let outer_h = store.add(outer_def);
        let shape = store.view(outer_h);

        let backend = TestBackend::new();
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(backend, arena, shape) };

        kani::assert(partial.depth() == 0, "initial depth is 0");

        unsafe { partial.begin_field(0).unwrap() };
        kani::assert(partial.depth() == 1, "depth is 1 after begin");

        unsafe { partial.mark_field_init(0).unwrap() };
        kani::assert(partial.depth() == 1, "depth still 1 after inner init");

        partial.end_field().unwrap();
        kani::assert(partial.depth() == 0, "depth back to 0 after end");
    }
}
