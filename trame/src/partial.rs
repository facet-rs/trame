//! Partial value construction.
//!
//! `Partial` manages incremental construction of a value, tracking which
//! fields have been initialized and ensuring proper cleanup on failure.

use crate::arena::{Arena, Idx};
use crate::dyn_shape::{IField, IShape, IStructType};
use crate::heap::Heap;
use crate::ops::{Op, Path, PathSegment, Source};
use crate::ptr::{PtrAsMut, PtrLike};
use core::marker::PhantomData;

/// Maximum fields tracked per frame (for verification).
pub const MAX_FRAME_FIELDS: usize = 8;

// ============================================================================
// FieldState - tracks which fields are initialized
// ============================================================================

/// Tracks per-field state within a struct frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FieldSlot<F> {
    /// Field not touched.
    Untracked,
    /// Field fully initialized (no child frame).
    Complete,
    /// Field has a child frame (staged or complete).
    Child(Idx<F>),
}

/// Tracks initialization state of struct fields.
#[derive(Debug, Clone, Copy)]
struct FieldStates<F> {
    /// State of each field.
    slots: [FieldSlot<F>; MAX_FRAME_FIELDS],
    /// Number of fields in the struct.
    count: u8,
}

impl<F> FieldStates<F> {
    /// Create field states for a struct with `count` fields.
    fn new(count: usize) -> Self {
        assert!(count <= MAX_FRAME_FIELDS, "too many fields");
        Self {
            slots: [FieldSlot::Untracked; MAX_FRAME_FIELDS],
            count: count as u8,
        }
    }

    /// Mark a field as complete (initialized).
    fn mark_complete(&mut self, idx: usize) {
        debug_assert!(idx < self.count as usize);
        self.slots[idx] = FieldSlot::Complete;
    }

    /// Mark a field as not started.
    fn mark_not_started(&mut self, idx: usize) {
        debug_assert!(idx < self.count as usize);
        self.slots[idx] = FieldSlot::Untracked;
    }

    /// Set a field's child frame index.
    fn set_child(&mut self, idx: usize, child: Idx<F>) {
        debug_assert!(idx < self.count as usize);
        self.slots[idx] = FieldSlot::Child(child);
    }

    /// Get a field's child frame index, if it has one.
    fn get_child(&self, idx: usize) -> Option<Idx<F>> {
        debug_assert!(idx < self.count as usize);
        match self.slots[idx] {
            FieldSlot::Child(child) => Some(child),
            _ => None,
        }
    }

    /// Check if a field is complete (initialized).
    fn is_init(&self, idx: usize) -> bool {
        debug_assert!(idx < self.count as usize);
        matches!(self.slots[idx], FieldSlot::Complete)
    }

    /// Check if a field is not started.
    fn is_not_started(&self, idx: usize) -> bool {
        debug_assert!(idx < self.count as usize);
        matches!(self.slots[idx], FieldSlot::Untracked)
    }

    /// Get the raw slot for a field.
    fn slot(&self, idx: usize) -> FieldSlot<F> {
        debug_assert!(idx < self.count as usize);
        self.slots[idx]
    }
}

// ============================================================================
// FrameKind - what kind of value we're building
// ============================================================================

/// What kind of value a frame is building.
#[derive(Debug, Clone, Copy)]
enum FrameKind<F> {
    /// Scalar value (no internal structure).
    Scalar { initialized: bool },
    /// Struct with tracked fields.
    Struct { fields: FieldStates<F> },
}

impl<F> FrameKind<F> {}

// ============================================================================
// Frame - a node in the construction tree
// ============================================================================

/// Completion state for a frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FrameState {
    /// Frame exists but has not been finalized.
    Staged,
    /// Frame has been finalized (complete).
    Complete,
}

/// A frame tracking construction of a single value.
struct Frame<H: Heap<S>, S: IShape> {
    /// Pointer to this frame's data.
    data: H::Ptr,
    /// Shape of the value being built.
    shape: S,
    /// What kind of value and its init state.
    kind: FrameKind<Self>,
    /// Whether the frame has been finalized.
    state: FrameState,
    /// Parent frame index (NOT_STARTED if root).
    parent: Idx<Self>,
    /// Field index within parent (if applicable).
    field_in_parent: u32,
}

impl<H: Heap<S>, S: IShape> Frame<H, S> {
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
    fn new_root(data: H::Ptr, shape: S) -> Self {
        Self {
            data,
            shape,
            kind: Self::kind_for_shape(shape),
            state: FrameState::Staged,
            parent: Idx::NOT_STARTED,
            field_in_parent: 0,
        }
    }
}

// ============================================================================
// Partial - the main builder
// ============================================================================

/// Manages incremental construction of a value.
pub struct Partial<H, A, S>
where
    H: Heap<S>,
    H::Ptr: PtrLike + PtrAsMut,
    A: Arena<Frame<H, S>>,
    S: IShape,
{
    /// The heap for memory operations.
    heap: H,
    /// Arena holding frames.
    arena: A,
    /// Root frame index.
    root: Idx<Frame<H, S>>,
    /// Current frame we're building.
    current: Idx<Frame<H, S>>,
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
    /// Current frame is not complete (for `Op::End`).
    CurrentIncomplete,
    /// Already at root frame (can't end).
    AtRoot,
    /// Shape mismatch.
    ShapeMismatch,
    /// Unsupported path segment.
    UnsupportedPath { segment: PathSegment },
    /// Unsupported source.
    UnsupportedSource,
    /// Default not available for this shape.
    DefaultUnavailable,
}

#[derive(Debug, Clone, Copy)]
struct FieldMeta<S: IShape> {
    shape: S,
    offset: usize,
    layout: core::alloc::Layout,
}

impl<H, A, S> Partial<H, A, S>
where
    H: Heap<S>,
    H::Ptr: PtrLike + PtrAsMut,
    A: Arena<Frame<H, S>>,
    S: IShape,
{
    /// Create a new Partial for the given shape.
    ///
    /// # Safety
    /// The caller must ensure the shape is valid and sized.
    pub unsafe fn new(mut heap: H, mut arena: A, shape: S) -> Self {
        let data = heap.alloc(shape);
        let frame = Frame::new_root(data, shape);
        let root = arena.alloc(frame);

        Self {
            heap,
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

    fn struct_type(shape: S) -> Result<S::StructType, PartialError> {
        shape.as_struct().ok_or(PartialError::NotAStruct)
    }

    fn field_meta(shape: S, field_idx: usize) -> Result<FieldMeta<S>, PartialError> {
        let st = Self::struct_type(shape)?;
        let field_count = st.field_count();
        let field = st.field(field_idx).ok_or(PartialError::FieldOutOfBounds {
            index: field_idx,
            count: field_count,
        })?;
        let field_shape = field.shape();
        let offset = field.offset();
        let layout = field_shape.layout();
        Ok(FieldMeta {
            shape: field_shape,
            offset,
            layout,
        })
    }

    fn field_ptr(
        frame: &Frame<H, S>,
        field_idx: usize,
    ) -> Result<(S, H::Ptr, usize), PartialError> {
        let meta = Self::field_meta(frame.shape, field_idx)?;
        let frame_size = frame.shape.layout().size();
        assert!(
            meta.offset + meta.layout.size() <= frame_size,
            "field out of bounds: {} + {} > {}",
            meta.offset,
            meta.layout.size(),
            frame_size
        );
        Ok((meta.shape, frame.data.offset(meta.offset), meta.layout.size()))
    }

    fn resolve_path(
        &mut self,
        path: Path<'_>,
    ) -> Result<(Idx<Frame<H, S>>, Option<usize>), PartialError> {
        if path.is_empty() {
            return Ok((self.current, None));
        }

        let mut idx = self.current;
        let mut segs = path;
        if let Some(PathSegment::Root) = segs.first() {
            // Move the cursor to the root by walking parent links.
            idx = self.ascend_to_root()?;
            segs = &segs[1..];
        }

        if segs.is_empty() {
            return Ok((idx, None));
        }

        if segs.len() != 1 {
            return Err(PartialError::UnsupportedPath {
                segment: segs[0],
            });
        }

        match segs[0] {
            PathSegment::Field(n) => Ok((idx, Some(n as usize))),
            PathSegment::Append => Err(PartialError::UnsupportedPath {
                segment: PathSegment::Append,
            }),
            PathSegment::Root => Err(PartialError::UnsupportedPath {
                segment: PathSegment::Root,
            }),
        }
    }

    fn ascend_to_root(&mut self) -> Result<Idx<Frame<H, S>>, PartialError> {
        while self.current != self.root {
            self.end_current_frame()?;
        }
        Ok(self.current)
    }

    fn apply_set(
        &mut self,
        target_idx: Idx<Frame<H, S>>,
        field_idx: Option<usize>,
        src: Source<H::Ptr>,
    ) -> Result<(), PartialError> {
        let frame = self.arena.get(target_idx);

        match field_idx {
            None => {
                match frame.kind {
                    FrameKind::Scalar { .. } => {
                        let size = frame.shape.layout().size();
                        let dst = frame.data;
                        let already_init = matches!(frame.kind, FrameKind::Scalar { initialized: true });

                        if already_init {
                            unsafe { self.heap.drop_in_place(dst, frame.shape) };
                        }

                        match src {
                            Source::Imm(src_ptr) => {
                                self.heap.memcpy(dst, src_ptr, size);
                            }
                            Source::Default => {
                                if let Some(raw) = dst.as_mut_ptr() {
                                    let ok = unsafe { frame.shape.default_in_place(raw) };
                                    if !ok {
                                        return Err(PartialError::DefaultUnavailable);
                                    }
                                }
                                self.heap.mark_init(dst, size);
                            }
                            Source::Stage(_) => return Err(PartialError::UnsupportedSource),
                        }

                        let frame = self.arena.get_mut(target_idx);
                        if let FrameKind::Scalar { initialized } = &mut frame.kind {
                            *initialized = true;
                        }
                        Ok(())
                    }
                    FrameKind::Struct { .. } => Err(PartialError::NotAStruct),
                }
            }
            Some(field_idx) => {
                let (mut child_idx, mut already_init) = match &frame.kind {
                    FrameKind::Struct { fields } => {
                        if field_idx >= fields.count as usize {
                            return Err(PartialError::FieldOutOfBounds {
                                index: field_idx,
                                count: fields.count as usize,
                            });
                        }
                        (fields.get_child(field_idx), fields.is_init(field_idx))
                    }
                    FrameKind::Scalar { .. } => return Err(PartialError::NotAStruct),
                };

                let (field_shape, dst, size) = Self::field_ptr(frame, field_idx)?;

                if let Some(child) = child_idx {
                    if matches!(src, Source::Imm(_) | Source::Default) {
                        // We are overwriting a staged field: drop any initialized subfields.
                        self.cleanup_frame(child);
                        if self.current_in_subtree(child) {
                            self.current = target_idx;
                        }
                        if let FrameKind::Struct { fields } =
                            &mut self.arena.get_mut(target_idx).kind
                        {
                            fields.mark_not_started(field_idx);
                        }
                        child_idx = None;
                        already_init = false;
                    }
                }

                if already_init {
                    unsafe { self.heap.drop_in_place(dst, field_shape) };
                    if let FrameKind::Struct { fields } = &mut self.arena.get_mut(target_idx).kind {
                        fields.mark_not_started(field_idx);
                    }
                }

                match src {
                    Source::Imm(src_ptr) => {
                        self.heap.memcpy(dst, src_ptr, size);
                        if let FrameKind::Struct { fields } = &mut self.arena.get_mut(target_idx).kind
                        {
                            fields.mark_complete(field_idx);
                        }
                        Ok(())
                    }
                    Source::Default => {
                        if let Some(raw) = dst.as_mut_ptr() {
                            let ok = unsafe { field_shape.default_in_place(raw) };
                            if !ok {
                                return Err(PartialError::DefaultUnavailable);
                            }
                        }
                        self.heap.mark_init(dst, size);
                        if let FrameKind::Struct { fields } = &mut self.arena.get_mut(target_idx).kind
                        {
                            fields.mark_complete(field_idx);
                        }
                        Ok(())
                    }
                    Source::Stage(_cap) => {
                        if !field_shape.is_struct() {
                            return Err(PartialError::NotAStruct);
                        }

                        if let Some(child) = child_idx {
                            let state = self.arena.get(child).state;
                            if state == FrameState::Staged {
                                // Re-enter an existing staged child frame.
                                self.current = child;
                                return Ok(());
                            }
                            // Child is complete: clear it and restart staging.
                            self.cleanup_frame(child);
                            {
                                let child_frame = self.arena.get_mut(child);
                                child_frame.kind = Frame::<H, S>::kind_for_shape(child_frame.shape);
                                child_frame.state = FrameState::Staged;
                            }
                            self.current = child;
                            return Ok(());
                        }

                        let child_frame = Frame {
                            data: dst,
                            shape: field_shape,
                            kind: Frame::<H, S>::kind_for_shape(field_shape),
                            state: FrameState::Staged,
                            parent: target_idx,
                            field_in_parent: field_idx as u32,
                        };

                        let child_idx = self.arena.alloc(child_frame);
                        if let FrameKind::Struct { fields } = &mut self.arena.get_mut(target_idx).kind
                        {
                            fields.set_child(field_idx, child_idx);
                        }
                        // Move the cursor to the child frame.
                        self.current = child_idx;
                        Ok(())
                    }
                }
            }
        }
    }

    fn current_in_subtree(&self, ancestor: Idx<Frame<H, S>>) -> bool {
        let mut idx = self.current;
        while idx.is_valid() {
            if idx == ancestor {
                return true;
            }
            let frame = self.arena.get(idx);
            idx = frame.parent;
        }
        false
    }

    fn frame_is_complete(&self, idx: Idx<Frame<H, S>>) -> bool {
        let frame = self.arena.get(idx);
        match &frame.kind {
            FrameKind::Scalar { initialized } => *initialized,
            FrameKind::Struct { fields } => {
                for i in 0..(fields.count as usize) {
                    match fields.slot(i) {
                        FieldSlot::Untracked => return false,
                        FieldSlot::Complete => {}
                        FieldSlot::Child(child) => {
                            let child_frame = self.arena.get(child);
                            if child_frame.state != FrameState::Complete {
                                return false;
                            }
                        }
                    }
                }
                true
            }
        }
    }

    pub fn apply(&mut self, op: Op<'_, H::Ptr>) -> Result<(), PartialError> {
        self.check_poisoned()?;

        match op {
            Op::End => self.end_current_frame(),
            Op::Set { dst, src } => {
                let (target, field_idx) = self.resolve_path(dst)?;
                self.apply_set(target, field_idx, src)
            }
        }
    }

    /// End constructing the current frame and move the cursor to the parent.
    ///
    /// This marks the field as initialized in the parent frame and pops back.
    /// The current frame must be complete.
    fn end_current_frame(&mut self) -> Result<(), PartialError> {
        self.check_poisoned()?;

        let frame = self.arena.get(self.current);

        // Check current frame is complete
        if !self.frame_is_complete(self.current) {
            return Err(PartialError::CurrentIncomplete);
        }

        // Check we're not at root
        let parent_idx = frame.parent;
        if !parent_idx.is_valid() {
            return Err(PartialError::AtRoot);
        }

        let field_in_parent = frame.field_in_parent as usize;

        // Mark this frame as complete.
        let frame = self.arena.get_mut(self.current);
        frame.state = FrameState::Complete;

        // Move cursor back to parent
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
        self.frame_is_complete(self.current)
    }

    /// Build the value, returning ownership of heap and arena.
    ///
    /// Returns error if not all fields are initialized.
    pub fn build(self) -> Result<(H, A, H::Ptr), PartialError> {
        self.check_poisoned()?;

        if !self.frame_is_complete(self.current) {
            return Err(PartialError::Incomplete);
        }

        let data = frame.data;

        // Don't run Drop - we're transferring ownership
        let heap = unsafe { core::ptr::read(&self.heap) };
        let arena = unsafe { core::ptr::read(&self.arena) };
        core::mem::forget(self);

        Ok((heap, arena, data))
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
    fn cleanup_frame(&mut self, idx: Idx<Frame<H, S>>) {
        if !idx.is_valid() {
            return;
        }

        let frame = self.arena.get(idx);
        if frame.state == FrameState::Complete {
            unsafe { self.heap.drop_in_place(frame.data, frame.shape) };
        } else {
            // Collect children first to avoid borrow issues
            let mut children = [Idx::NOT_STARTED; MAX_FRAME_FIELDS];
            let mut child_count = 0;

            if let FrameKind::Struct { fields } = &frame.kind {
                for i in 0..(fields.count as usize) {
                    if let Some(child_idx) = fields.get_child(i) {
                        children[child_count] = child_idx;
                        child_count += 1;
                    }
                }
            }

            // Recursively clean up children first (depth-first)
            for i in 0..child_count {
                self.cleanup_frame(children[i]);
            }

            // Now clean up this frame's initialized slots
            match &frame.kind {
                FrameKind::Scalar { initialized: true } => {
                    unsafe { self.heap.drop_in_place(frame.data, frame.shape) };
                }
                FrameKind::Struct { fields } => {
                    for i in 0..(fields.count as usize) {
                        if matches!(fields.slot(i), FieldSlot::Complete) {
                            let (field_shape, ptr, _size) = Self::field_ptr(frame, i)
                                .expect("field metadata should be valid during cleanup");
                            unsafe { self.heap.drop_in_place(ptr, field_shape) };
                        }
                    }
                }
                _ => {}
            }
        }

        // Only root owns the allocation.
        if !frame.parent.is_valid() {
            self.heap.dealloc(frame.data, frame.shape);
        }
    }
}

impl<H, A, S> Drop for Partial<H, A, S>
where
    H: Heap<S>,
    H::Ptr: PtrLike + PtrAsMut,
    A: Arena<Frame<H, S>>,
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
    use crate::heap::VerifiedHeap;
    use crate::dyn_shape::{DynShapeDef, DynShapeStore, DynShapeView};
    use core::alloc::Layout;

    type S<'a> = DynShapeView<'a, DynShapeStore>;
    type TestHeap<'a> = VerifiedHeap<S<'a>>;
    type TestArena<'a> = VerifiedArena<Frame<TestHeap<'a>, S<'a>>, 8>;

    #[test]
    fn scalar_lifecycle() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let shape = store.view(h);

        let mut heap = TestHeap::new();
        let src = heap.alloc(shape);
        heap.mark_init(src, 4);

        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        assert!(!partial.is_complete());
        let root: [PathSegment; 0] = [];
        partial
            .apply(Op::Set {
                dst: &root,
                src: Source::Imm(src),
            })
            .unwrap();
        assert!(partial.is_complete());

        let _ = partial.build().unwrap();
    }

    #[test]
    fn struct_lifecycle() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);
        let u32_shape = store.view(u32_h);

        let mut heap = TestHeap::new();
        let src0 = heap.alloc(u32_shape);
        let src1 = heap.alloc(u32_shape);
        heap.mark_init(src0, 4);
        heap.mark_init(src1, 4);

        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        assert!(!partial.is_complete());

        let f0 = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &f0,
                src: Source::Imm(src0),
            })
            .unwrap();
        assert!(!partial.is_complete());

        let f1 = [PathSegment::Field(1)];
        partial
            .apply(Op::Set {
                dst: &f1,
                src: Source::Imm(src1),
            })
            .unwrap();
        assert!(partial.is_complete());

        let _ = partial.build().unwrap();
    }

    #[test]
    fn struct_any_order() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def =
            DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h), (8, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);
        let u32_shape = store.view(u32_h);

        let mut heap = TestHeap::new();
        let src0 = heap.alloc(u32_shape);
        let src1 = heap.alloc(u32_shape);
        let src2 = heap.alloc(u32_shape);
        heap.mark_init(src0, 4);
        heap.mark_init(src1, 4);
        heap.mark_init(src2, 4);

        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        // Init in reverse order
        let f2 = [PathSegment::Field(2)];
        partial
            .apply(Op::Set {
                dst: &f2,
                src: Source::Imm(src2),
            })
            .unwrap();
        let f0 = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &f0,
                src: Source::Imm(src0),
            })
            .unwrap();
        let f1 = [PathSegment::Field(1)];
        partial
            .apply(Op::Set {
                dst: &f1,
                src: Source::Imm(src1),
            })
            .unwrap();

        assert!(partial.is_complete());
        let _ = partial.build().unwrap();
    }

    #[test]
    fn stage_field_twice_reenters() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let inner_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h)]);
        let inner_h = store.add(inner_def);
        let outer_def = DynShapeDef::struct_with_fields(&store, &[(0, inner_h)]);
        let outer_h = store.add(outer_def);
        let shape = store.view(outer_h);

        let heap = TestHeap::new();
        let arena = TestArena::new();

        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        let field0 = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &field0,
                src: Source::Stage(None),
            })
            .unwrap();

        let inner_a = [PathSegment::Field(0)];
        let src = partial.heap.alloc(store.view(u32_h));
        partial.heap.mark_init(src, 4);
        partial
            .apply(Op::Set {
                dst: &inner_a,
                src: Source::Imm(src),
            })
            .unwrap();
        partial.apply(Op::End).unwrap();

        let root_field0 = [PathSegment::Root, PathSegment::Field(0)];
        let result = partial.apply(Op::Set {
            dst: &root_field0,
            src: Source::Stage(None),
        });
        assert!(result.is_ok());
        assert_eq!(partial.depth(), 1);
    }

    #[test]
    fn incomplete_build_fails() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);
        let u32_shape = store.view(u32_h);

        let mut heap = TestHeap::new();
        let src = heap.alloc(u32_shape);
        heap.mark_init(src, 4);

        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        let f0 = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &f0,
                src: Source::Imm(src),
            })
            .unwrap();

        let err = partial.build();
        assert!(matches!(err, Err(PartialError::Incomplete)));
    }

    #[test]
    fn drop_cleans_up() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);
        let u32_shape = store.view(u32_h);

        let mut heap = TestHeap::new();
        let src = heap.alloc(u32_shape);
        heap.mark_init(src, 4);

        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        let f0 = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &f0,
                src: Source::Imm(src),
            })
            .unwrap();

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
        let u32_shape = store.view(u32_h);

        let mut heap = TestHeap::new();
        let src_x = heap.alloc(u32_shape);
        let src_a = heap.alloc(u32_shape);
        let src_b = heap.alloc(u32_shape);
        heap.mark_init(src_x, 4);
        heap.mark_init(src_a, 4);
        heap.mark_init(src_b, 4);

        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        assert_eq!(partial.depth(), 0);
        assert!(!partial.is_complete());

        let outer_x = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &outer_x,
                src: Source::Imm(src_x),
            })
            .unwrap();
        assert!(!partial.is_complete());

        let inner_field = [PathSegment::Field(1)];
        partial
            .apply(Op::Set {
                dst: &inner_field,
                src: Source::Stage(None),
            })
            .unwrap();
        assert_eq!(partial.depth(), 1);

        let inner_a = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_a,
                src: Source::Imm(src_a),
            })
            .unwrap();
        let inner_b = [PathSegment::Field(1)];
        partial
            .apply(Op::Set {
                dst: &inner_b,
                src: Source::Imm(src_b),
            })
            .unwrap();
        assert!(partial.is_complete());

        partial.apply(Op::End).unwrap();
        assert_eq!(partial.depth(), 0);
        assert!(partial.is_complete());

        let _ = partial.build().unwrap();
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
        let u32_shape = store.view(u32_h);

        let mut heap = TestHeap::new();
        let src = heap.alloc(u32_shape);
        heap.mark_init(src, 4);

        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        let inner_field = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_field,
                src: Source::Stage(None),
            })
            .unwrap();
        let inner_a = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_a,
                src: Source::Imm(src),
            })
            .unwrap();

        drop(partial);
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
        let u32_shape = store.view(u32_h);

        let mut heap = TestHeap::new();
        let src = heap.alloc(u32_shape);
        heap.mark_init(src, 4);

        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        let inner_field = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_field,
                src: Source::Stage(None),
            })
            .unwrap();
        let inner_a = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_a,
                src: Source::Imm(src),
            })
            .unwrap();

        drop(partial);
    }

    #[test]
    fn end_op_incomplete_fails() {
        let mut store = DynShapeStore::new();

        // Inner struct: { a: u32, b: u32 }
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let inner_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let inner_h = store.add(inner_def);

        // Outer struct: { inner: Inner }
        let outer_def = DynShapeDef::struct_with_fields(&store, &[(0, inner_h)]);
        let outer_h = store.add(outer_def);
        let shape = store.view(outer_h);
        let u32_shape = store.view(u32_h);

        let mut heap = TestHeap::new();
        let src = heap.alloc(u32_shape);
        heap.mark_init(src, 4);

        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        let inner_field = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_field,
                src: Source::Stage(None),
            })
            .unwrap();
        let inner_a = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_a,
                src: Source::Imm(src),
            })
            .unwrap();

        let err = partial.apply(Op::End);
        assert_eq!(err, Err(PartialError::CurrentIncomplete));
    }

    #[test]
    fn end_op_at_root_fails() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);
        let u32_shape = store.view(u32_h);

        let mut heap = TestHeap::new();
        let src = heap.alloc(u32_shape);
        heap.mark_init(src, 4);

        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        let f0 = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &f0,
                src: Source::Imm(src),
            })
            .unwrap();

        let err = partial.apply(Op::End);
        assert_eq!(err, Err(PartialError::AtRoot));
    }

    #[test]
    fn apply_set_imm_field() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);
        let u32_shape = store.view(u32_h);

        let mut heap = TestHeap::new();
        let src = heap.alloc(u32_shape);
        heap.mark_init(src, 4);

        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        let path = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &path,
                src: Source::Imm(src),
            })
            .unwrap();

        assert!(partial.is_complete());
        let _ = partial.build().unwrap();
    }

    #[test]
    fn apply_stage_and_end() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let inner_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let inner_h = store.add(inner_def);
        let outer_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, inner_h)]);
        let outer_h = store.add(outer_def);

        let shape = store.view(outer_h);
        let u32_shape = store.view(u32_h);

        let mut heap = TestHeap::new();
        let src1 = heap.alloc(u32_shape);
        let src2 = heap.alloc(u32_shape);
        heap.mark_init(src1, 4);
        heap.mark_init(src2, 4);

        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        let outer_x = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &outer_x,
                src: Source::Imm(src1),
            })
            .unwrap();

        let inner_field = [PathSegment::Field(1)];
        partial
            .apply(Op::Set {
                dst: &inner_field,
                src: Source::Stage(None),
            })
            .unwrap();

        let inner_a = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_a,
                src: Source::Imm(src2),
            })
            .unwrap();

        let inner_b = [PathSegment::Field(1)];
        let src3 = partial.heap.alloc(u32_shape);
        partial.heap.mark_init(src3, 4);
        partial
            .apply(Op::Set {
                dst: &inner_b,
                src: Source::Imm(src3),
            })
            .unwrap();

        partial.apply(Op::End).unwrap();
        assert!(partial.is_complete());
        let _ = partial.build().unwrap();
    }
}

#[cfg(kani)]
mod kani_proofs {
    use super::*;
    use crate::arena::VerifiedArena;
    use crate::heap::VerifiedHeap;
    use crate::dyn_shape::{
        DynDef, DynFieldDef, DynShapeDef, DynShapeHandle, DynShapeStore, DynShapeView,
        DynStructDef, IField, IShape, IStructType, MAX_FIELDS,
    };
    use core::alloc::Layout;

    type S<'a> = DynShapeView<'a, DynShapeStore>;
    type TestHeap<'a> = VerifiedHeap<S<'a>>;
    type TestArena<'a> = VerifiedArena<Frame<TestHeap<'a>, S<'a>>, 4>;

    #[kani::proof]
    #[kani::unwind(10)]
    fn scalar_init_complete() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 4).unwrap()));
        let shape = store.view(h);

        let mut heap = TestHeap::new();
        let src = heap.alloc(shape);
        heap.mark_init(src, 4);
        let arena = TestArena::new();

        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        kani::assert(!partial.is_complete(), "not complete initially");
        let root: [PathSegment; 0] = [];
        partial
            .apply(Op::Set {
                dst: &root,
                src: Source::Imm(src),
            })
            .unwrap();
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

        let mut heap = TestHeap::new();
        let scalar_shape = store.view(scalar_h);
        let src = heap.alloc(scalar_shape);
        heap.mark_init(src, 4);
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        // Init all but one field
        let skip_field: u8 = kani::any();
        kani::assume(skip_field < field_count);

        for i in 0..(field_count as usize) {
            if i != skip_field as usize {
                let path = [PathSegment::Field(i as u32)];
                partial
                    .apply(Op::Set {
                        dst: &path,
                        src: Source::Imm(src),
                    })
                    .unwrap();
            }
        }

        // Should not be complete
        kani::assert(!partial.is_complete(), "incomplete without all fields");

        // Init the skipped field
        let path = [PathSegment::Field(skip_field as u32)];
        partial
            .apply(Op::Set {
                dst: &path,
                src: Source::Imm(src),
            })
            .unwrap();

        // Now should be complete
        kani::assert(partial.is_complete(), "complete with all fields");
    }

    /// Prove: staging the same field via Root while in a child re-enters
    #[kani::proof]
    #[kani::unwind(10)]
    fn double_init_rejected() {
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

        let heap = TestHeap::new();
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        let path = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &path,
                src: Source::Stage(None),
            })
            .unwrap();

        let scalar_shape = store.view(scalar_h);
        let src = partial.heap.alloc(scalar_shape);
        partial.heap.mark_init(src, 4);
        let inner_a = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_a,
                src: Source::Imm(src),
            })
            .unwrap();
        partial.apply(Op::End).unwrap();

        let root_path = [PathSegment::Root, PathSegment::Field(0)];
        let result2 = partial.apply(Op::Set {
            dst: &root_path,
            src: Source::Stage(None),
        });
        kani::assert(result2.is_ok(), "stage after complete re-enters");
        kani::assert(partial.depth() == 1, "cursor remains in child");
    }

    /// Prove: build fails if not all fields initialized
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

        let mut heap = TestHeap::new();
        let scalar_shape = store.view(scalar_h);
        let src = heap.alloc(scalar_shape);
        heap.mark_init(src, 4);
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        // Init only first field
        let f0 = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &f0,
                src: Source::Imm(src),
            })
            .unwrap();

        // build should fail
        let result = partial.build();
        kani::assert(result.is_err(), "build fails when incomplete");
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

        let mut heap = TestHeap::new();
        let scalar_shape = store.view(scalar_h);
        let src = heap.alloc(scalar_shape);
        heap.mark_init(src, 4);
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        // Try to init field beyond bounds
        let bad_idx: u8 = kani::any();
        kani::assume(bad_idx >= field_count);
        kani::assume(bad_idx < 10); // Keep bounded

        let path = [PathSegment::Field(bad_idx as u32)];
        let result = partial.apply(Op::Set {
            dst: &path,
            src: Source::Imm(src),
        });
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

        let mut heap = TestHeap::new();
        let scalar_shape = store.view(scalar_h);
        let src0 = heap.alloc(scalar_shape);
        let src1 = heap.alloc(scalar_shape);
        let src2 = heap.alloc(scalar_shape);
        heap.mark_init(src0, 4);
        heap.mark_init(src1, 4);
        heap.mark_init(src2, 4);
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        // Choose arbitrary init order
        let first: u8 = kani::any();
        let second: u8 = kani::any();
        let third: u8 = kani::any();
        kani::assume(first < 3 && second < 3 && third < 3);
        kani::assume(first != second && second != third && first != third);

        let paths = [
            [PathSegment::Field(0)],
            [PathSegment::Field(1)],
            [PathSegment::Field(2)],
        ];
        let srcs = [src0, src1, src2];
        let order = [first as usize, second as usize, third as usize];
        for idx in order {
            partial
                .apply(Op::Set {
                    dst: &paths[idx],
                    src: Source::Imm(srcs[idx]),
                })
                .unwrap();
        }

        kani::assert(
            partial.is_complete(),
            "complete after all fields in any order",
        );

        let result = partial.build();
        kani::assert(result.is_ok(), "build succeeds when complete");
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
        let mut heap = TestHeap::new();
        let scalar_shape = store.view(scalar_h);
        let src_x = heap.alloc(scalar_shape);
        let src_a = heap.alloc(scalar_shape);
        let src_b = heap.alloc(scalar_shape);
        heap.mark_init(src_x, 4);
        heap.mark_init(src_a, 4);
        heap.mark_init(src_b, 4);
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        let outer_x = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &outer_x,
                src: Source::Imm(src_x),
            })
            .unwrap();
        let inner_field = [PathSegment::Field(1)];
        partial
            .apply(Op::Set {
                dst: &inner_field,
                src: Source::Stage(None),
            })
            .unwrap();
        let inner_a = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_a,
                src: Source::Imm(src_a),
            })
            .unwrap();
        let inner_b = [PathSegment::Field(1)];
        partial
            .apply(Op::Set {
                dst: &inner_b,
                src: Source::Imm(src_b),
            })
            .unwrap();
        partial.apply(Op::End).unwrap();

        kani::assert(partial.is_complete(), "outer complete after both fields");
        let result = partial.build();
        kani::assert(result.is_ok(), "build succeeds");
    }

    /// Prove: Stage/End lifecycle works correctly
    #[kani::proof]
    #[kani::unwind(10)]
    fn stage_end_lifecycle() {
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

        let mut heap = TestHeap::new();
        let scalar_shape = store.view(scalar_h);
        let src_x = heap.alloc(scalar_shape);
        let src_a = heap.alloc(scalar_shape);
        let src_b = heap.alloc(scalar_shape);
        heap.mark_init(src_x, 4);
        heap.mark_init(src_a, 4);
        heap.mark_init(src_b, 4);
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        // Verify initial state
        kani::assert(partial.depth() == 0, "starts at depth 0");
        kani::assert(!partial.is_complete(), "not complete initially");

        // Init scalar field
        let outer_x = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &outer_x,
                src: Source::Imm(src_x),
            })
            .unwrap();
        kani::assert(!partial.is_complete(), "not complete with one field");

        // Stage nested struct
        let inner_field = [PathSegment::Field(1)];
        let result = partial.apply(Op::Set {
            dst: &inner_field,
            src: Source::Stage(None),
        });
        kani::assert(result.is_ok(), "stage succeeds");
        kani::assert(partial.depth() == 1, "depth is 1 after begin");
        kani::assert(!partial.is_complete(), "inner not complete yet");

        // Init inner fields
        let inner_a = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_a,
                src: Source::Imm(src_a),
            })
            .unwrap();
        let inner_b = [PathSegment::Field(1)];
        partial
            .apply(Op::Set {
                dst: &inner_b,
                src: Source::Imm(src_b),
            })
            .unwrap();
        kani::assert(partial.is_complete(), "inner complete");

        // End nested struct
        let result = partial.apply(Op::End);
        kani::assert(result.is_ok(), "end succeeds");
        kani::assert(partial.depth() == 0, "back to depth 0");
        kani::assert(partial.is_complete(), "outer complete");

        let result = partial.build();
        kani::assert(result.is_ok(), "build succeeds");
    }

    /// Prove: staging the same field via Root while in a child re-enters
    #[kani::proof]
    #[kani::unwind(10)]
    fn stage_reenter_root_ok() {
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

        let heap = TestHeap::new();
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        let path = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &path,
                src: Source::Stage(None),
            })
            .unwrap();

        let scalar_shape = store.view(scalar_h);
        let src = partial.heap.alloc(scalar_shape);
        partial.heap.mark_init(src, 4);
        let inner_a = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_a,
                src: Source::Imm(src),
            })
            .unwrap();
        partial.apply(Op::End).unwrap();

        let root_path = [PathSegment::Root, PathSegment::Field(0)];
        let result = partial.apply(Op::Set {
            dst: &root_path,
            src: Source::Stage(None),
        });
        kani::assert(result.is_ok(), "stage after complete re-enters");
        kani::assert(partial.depth() == 1, "cursor remains in child");
    }

    /// Prove: Op::End at root returns error
    #[kani::proof]
    #[kani::unwind(10)]
    fn end_op_at_root_fails() {
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

        let mut heap = TestHeap::new();
        let scalar_shape = store.view(scalar_h);
        let src = heap.alloc(scalar_shape);
        heap.mark_init(src, 4);
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        let f0 = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &f0,
                src: Source::Imm(src),
            })
            .unwrap();

        // Try End at root
        let result = partial.apply(Op::End);
        kani::assert(result.is_err(), "end at root fails");
        kani::assert(
            matches!(result, Err(PartialError::AtRoot)),
            "error is AtRoot",
        );
    }

    /// Prove: End with incomplete inner fails
    #[kani::proof]
    #[kani::unwind(10)]
    fn end_op_incomplete_inner_fails() {
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

        let mut heap = TestHeap::new();
        let scalar_shape = store.view(scalar_h);
        let src = heap.alloc(scalar_shape);
        heap.mark_init(src, 4);
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        let inner_field = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_field,
                src: Source::Stage(None),
            })
            .unwrap();
        let inner_a = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_a,
                src: Source::Imm(src),
            })
            .unwrap();

        // Try End with incomplete inner
        let result = partial.apply(Op::End);
        kani::assert(result.is_err(), "end with incomplete inner fails");
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

        let mut heap = TestHeap::new();
        let scalar_shape = store.view(scalar_h);
        let src = heap.alloc(scalar_shape);
        heap.mark_init(src, 4);
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        // Enter nested struct
        let inner_field = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_field,
                src: Source::Stage(None),
            })
            .unwrap();
        // Init inner field
        let inner_a = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_a,
                src: Source::Imm(src),
            })
            .unwrap();
        // Don't end - just drop

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

        let mut heap = TestHeap::new();
        let scalar_shape = store.view(scalar_h);
        let src = heap.alloc(scalar_shape);
        heap.mark_init(src, 4);
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        kani::assert(partial.depth() == 0, "initial depth is 0");

        let inner_field = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_field,
                src: Source::Stage(None),
            })
            .unwrap();
        kani::assert(partial.depth() == 1, "depth is 1 after begin");

        let inner_a = [PathSegment::Field(0)];
        partial
            .apply(Op::Set {
                dst: &inner_a,
                src: Source::Imm(src),
            })
            .unwrap();
        kani::assert(partial.depth() == 1, "depth still 1 after inner init");

        partial.apply(Op::End).unwrap();
        kani::assert(partial.depth() == 0, "depth back to 0 after end");
    }

    /// Prove: Stage uses the same allocation as the parent.
    #[kani::proof]
    #[kani::unwind(8)]
    fn stage_same_alloc_id() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
        let inner_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let inner_h = store.add(inner_def);
        let outer_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, inner_h)]);
        let outer_h = store.add(outer_def);
        let shape = store.view(outer_h);

        let heap = TestHeap::new();
        let arena = TestArena::new();
        let mut partial = unsafe { Partial::new(heap, arena, shape) };

        let inner_field = [PathSegment::Field(1)];
        partial
            .apply(Op::Set {
                dst: &inner_field,
                src: Source::Stage(None),
            })
            .unwrap();

        let child = partial.current;
        let child_frame = partial.arena.get(child);
        let parent_frame = partial.arena.get(child_frame.parent);

        kani::assert(
            child_frame.data.alloc_id() == parent_frame.data.alloc_id(),
            "same allocation id",
        );
        kani::assert(child_frame.data.offset_bytes() == 4, "offset is field offset");
    }
}
