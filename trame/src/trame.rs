//! Partial value construction.
//!
//! `Trame` manages incremental construction of a value, tracking which
//! fields have been initialized and ensuring proper cleanup on failure.

mod errors;
pub use errors::TrameError;

use crate::{
    Op, Path, PathSegment, Source,
    node::{FieldSlot, MAX_NODE_FIELDS, Node, NodeKind, NodeState},
    runtime::{IArena, IField, IHeap, IPtr, IRuntime, IShape, IStructType, Idx},
};

type Heap<R> = <R as IRuntime>::Heap;
type Shape<R> = <R as IRuntime>::Shape;
type Arena<R> = <R as IRuntime>::Arena;
type Ptr<R> = <Heap<R> as IHeap<Shape<R>>>::Ptr;

// ============================================================================
// Trame - the main builder
// ============================================================================

/// Manages incremental construction of a value.
pub struct Trame<R>
where
    R: IRuntime,
{
    /// The heap for memory operations.
    heap: R::Heap,

    /// Arena holding Nodes.
    arena: R::Arena,

    /// Root Node index.
    root: Idx<Node<R::Heap, R::Shape>>,

    /// Current Node we're building.
    current: Idx<Node<R::Heap, R::Shape>>,

    /// Whether we've been poisoned (error occurred).
    poisoned: bool,
}

#[derive(Debug, Clone, Copy)]
struct FieldMeta<S: IShape> {
    shape: S,
    offset: usize,
    layout: core::alloc::Layout,
}

impl<R> Trame<R>
where
    R: IRuntime,
{
    /// Create a new Trame for the given shape.
    ///
    /// # Safety
    /// The caller must ensure the shape is valid and sized.
    pub unsafe fn new(mut heap: R::Heap, shape: Shape<R>) -> Self {
        let mut arena = R::arena();
        let data = unsafe { heap.alloc(shape) };
        let node = Node::new_root(data, shape);
        let root = arena.alloc(node);

        Self {
            heap,
            arena,
            root,
            current: root,
            poisoned: false,
        }
    }

    /// Check if poisoned, return error if so.
    fn check_poisoned(&self) -> Result<(), TrameError> {
        if self.poisoned {
            Err(TrameError::Poisoned)
        } else {
            Ok(())
        }
    }

    fn struct_type(shape: Shape<R>) -> Result<<Shape<R> as IShape>::StructType, TrameError> {
        shape.as_struct().ok_or(TrameError::NotAStruct)
    }

    fn field_meta(shape: Shape<R>, field_idx: usize) -> Result<FieldMeta<Shape<R>>, TrameError> {
        let st = Self::struct_type(shape)?;
        let field_count = st.field_count();
        let field = st.field(field_idx).ok_or(TrameError::FieldOutOfBounds {
            index: field_idx,
            count: field_count,
        })?;
        let field_shape = field.shape();
        let offset = field.offset();
        let layout = field_shape.layout().expect("IShape requires sized types");
        Ok(FieldMeta {
            shape: field_shape,
            offset,
            layout,
        })
    }

    fn field_ptr(
        node: &Node<Heap<R>, Shape<R>>,
        field_idx: usize,
    ) -> Result<(Shape<R>, Ptr<R>, usize), TrameError> {
        let meta = Self::field_meta(node.shape, field_idx)?;
        let node_size = node
            .shape
            .layout()
            .expect("IShape requires sized types")
            .size();
        assert!(
            meta.offset + meta.layout.size() <= node_size,
            "field out of bounds: {} + {} > {}",
            meta.offset,
            meta.layout.size(),
            node_size
        );
        Ok((
            meta.shape,
            node.data.byte_add(meta.offset),
            meta.layout.size(),
        ))
    }

    fn resolve_path(
        &mut self,
        path: Path<'_>,
    ) -> Result<(Idx<Node<Heap<R>, Shape<R>>>, Option<usize>), TrameError> {
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
            return Err(TrameError::UnsupportedPath { segment: segs[0] });
        }

        match segs[0] {
            PathSegment::Field(n) => Ok((idx, Some(n as usize))),
            PathSegment::Append => Err(TrameError::UnsupportedPath {
                segment: PathSegment::Append,
            }),
            PathSegment::Root => Err(TrameError::UnsupportedPath {
                segment: PathSegment::Root,
            }),
        }
    }

    fn ascend_to_root(&mut self) -> Result<Idx<Node<Heap<R>, Shape<R>>>, TrameError> {
        while self.current != self.root {
            self.end_current_node()?;
        }
        Ok(self.current)
    }

    fn apply_set(
        &mut self,
        target_idx: Idx<Node<Heap<R>, Shape<R>>>,
        field_idx: Option<usize>,
        src: Source<Ptr<R>>,
    ) -> Result<(), TrameError> {
        let node = self.arena.get(target_idx);

        match field_idx {
            None => match node.kind {
                NodeKind::Scalar { .. } => {
                    let size = node
                        .shape
                        .layout()
                        .expect("IShape requires sized types")
                        .size();
                    let dst = node.data;
                    let already_init = matches!(node.kind, NodeKind::Scalar { initialized: true });

                    if already_init {
                        unsafe { self.heap.drop_in_place(dst, node.shape) };
                    }

                    match src {
                        Source::Imm(src_ptr) => {
                            unsafe { self.heap.memcpy(dst, src_ptr, size) };
                        }
                        Source::Default => {
                            let ok = unsafe { self.heap.default_in_place(dst, node.shape) };
                            if !ok {
                                return Err(TrameError::DefaultUnavailable);
                            }
                        }
                        Source::Stage(_) => return Err(TrameError::UnsupportedSource),
                    }

                    let node = self.arena.get_mut(target_idx);
                    if let NodeKind::Scalar { initialized } = &mut node.kind {
                        *initialized = true;
                    }
                    Ok(())
                }
                NodeKind::Struct { .. } => Err(TrameError::NotAStruct),
            },
            Some(field_idx) => {
                let (mut child_idx, mut already_init) = match &node.kind {
                    NodeKind::Struct { fields } => {
                        if field_idx >= fields.count as usize {
                            return Err(TrameError::FieldOutOfBounds {
                                index: field_idx,
                                count: fields.count as usize,
                            });
                        }
                        (fields.get_child(field_idx), fields.is_init(field_idx))
                    }
                    NodeKind::Scalar { .. } => return Err(TrameError::NotAStruct),
                };

                let (field_shape, dst, size) = Self::field_ptr(node, field_idx)?;

                if let Some(child) = child_idx {
                    if matches!(src, Source::Imm(_) | Source::Default) {
                        // We are overwriting a staged field: drop any initialized subfields.
                        self.cleanup_node(child);
                        if self.current_in_subtree(child) {
                            self.current = target_idx;
                        }
                        if let NodeKind::Struct { fields } =
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
                    if let NodeKind::Struct { fields } = &mut self.arena.get_mut(target_idx).kind {
                        fields.mark_not_started(field_idx);
                    }
                }

                match src {
                    Source::Imm(src_ptr) => {
                        unsafe { self.heap.memcpy(dst, src_ptr, size) };
                        if let NodeKind::Struct { fields } =
                            &mut self.arena.get_mut(target_idx).kind
                        {
                            fields.mark_complete(field_idx);
                        }
                        Ok(())
                    }
                    Source::Default => {
                        let ok = unsafe { self.heap.default_in_place(dst, field_shape) };
                        if !ok {
                            return Err(TrameError::DefaultUnavailable);
                        }
                        if let NodeKind::Struct { fields } =
                            &mut self.arena.get_mut(target_idx).kind
                        {
                            fields.mark_complete(field_idx);
                        }
                        Ok(())
                    }
                    Source::Stage(_cap) => {
                        if !field_shape.is_struct() {
                            return Err(TrameError::NotAStruct);
                        }

                        if let Some(child) = child_idx {
                            let state = self.arena.get(child).state;
                            if state == NodeState::Staged {
                                // Re-enter an existing staged child Node.
                                self.current = child;
                                return Ok(());
                            }
                            // Child is complete: clear it and restart staging.
                            self.cleanup_node(child);
                            {
                                let child_node = self.arena.get_mut(child);
                                child_node.kind =
                                    Node::<Heap<R>, Shape<R>>::kind_for_shape(child_node.shape);
                                child_node.state = NodeState::Staged;
                            }
                            self.current = child;
                            return Ok(());
                        }

                        let child_node = Node {
                            data: dst,
                            shape: field_shape,
                            kind: Node::<Heap<R>, Shape<R>>::kind_for_shape(field_shape),
                            state: NodeState::Staged,
                            parent: target_idx,
                            field_in_parent: field_idx as u32,
                        };

                        let child_idx = self.arena.alloc(child_node);
                        if let NodeKind::Struct { fields } =
                            &mut self.arena.get_mut(target_idx).kind
                        {
                            fields.set_child(field_idx, child_idx);
                        }
                        // Move the cursor to the child Node.
                        self.current = child_idx;
                        Ok(())
                    }
                }
            }
        }
    }

    fn current_in_subtree(&self, ancestor: Idx<Node<Heap<R>, Shape<R>>>) -> bool {
        let mut idx = self.current;
        while idx.is_valid() {
            if idx == ancestor {
                return true;
            }
            let node = self.arena.get(idx);
            idx = node.parent;
        }
        false
    }

    fn node_is_complete(&self, idx: Idx<Node<Heap<R>, Shape<R>>>) -> bool {
        let node = self.arena.get(idx);
        match &node.kind {
            NodeKind::Scalar { initialized } => *initialized,
            NodeKind::Struct { fields } => {
                for i in 0..(fields.count as usize) {
                    match fields.slot(i) {
                        FieldSlot::Untracked => return false,
                        FieldSlot::Complete => {}
                        FieldSlot::Child(child) => {
                            let child_node = self.arena.get(child);
                            if child_node.state != NodeState::Sealed {
                                return false;
                            }
                        }
                    }
                }
                true
            }
        }
    }

    pub fn apply(&mut self, op: Op<'_, Ptr<R>>) -> Result<(), TrameError> {
        self.check_poisoned()?;

        match op {
            Op::End => self.end_current_node(),
            Op::Set { dst, src } => {
                let (target, field_idx) = self.resolve_path(dst)?;
                self.apply_set(target, field_idx, src)
            }
        }
    }

    /// End constructing the current Node and move the cursor to the parent.
    ///
    /// This marks the field as initialized in the parent Node and pops back.
    /// The current Node must be complete.
    fn end_current_node(&mut self) -> Result<(), TrameError> {
        self.check_poisoned()?;

        let node = self.arena.get(self.current);

        // Check current Node is complete
        if !self.node_is_complete(self.current) {
            return Err(TrameError::CurrentIncomplete);
        }

        // Check we're not at root
        let parent_idx = node.parent;
        if !parent_idx.is_valid() {
            return Err(TrameError::AtRoot);
        }

        // Mark this Node as complete.
        let node = self.arena.get_mut(self.current);
        node.state = NodeState::Sealed;

        // Move cursor back to parent
        self.current = parent_idx;

        Ok(())
    }

    /// Get the current nesting depth (0 = root).
    pub fn depth(&self) -> usize {
        let mut depth = 0;
        let mut idx = self.current;
        while idx.is_valid() {
            let node = self.arena.get(idx);
            if !node.parent.is_valid() {
                break;
            }
            depth += 1;
            idx = node.parent;
        }
        depth
    }

    /// Check if the current Node is complete.
    pub fn is_complete(&self) -> bool {
        if self.poisoned {
            return false;
        }
        self.node_is_complete(self.current)
    }

    /// Build the value, returning ownership of heap and arena.
    ///
    /// Returns error if not all fields are initialized.
    pub fn build(self) -> Result<(Heap<R>, Arena<R>, Ptr<R>), TrameError> {
        self.check_poisoned()?;

        if !self.node_is_complete(self.current) {
            return Err(TrameError::Incomplete);
        }

        let node = self.arena.get(self.root);
        let data = node.data;

        // Don't run Drop - we're transferring ownership
        let heap = unsafe { core::ptr::read(&self.heap) };
        let arena = unsafe { core::ptr::read(&self.arena) };
        core::mem::forget(self);

        Ok((heap, arena, data))
    }

    /// Poison the Trame, cleaning up all initialized fields.
    fn poison(&mut self) {
        if self.poisoned {
            return;
        }
        self.poisoned = true;

        // Clean up from root, depth-first (leaves before parents)
        self.cleanup_node(self.root);
    }

    /// Recursively clean up a Node and all its children (depth-first).
    fn cleanup_node(&mut self, idx: Idx<Node<Heap<R>, Shape<R>>>) {
        if !idx.is_valid() {
            return;
        }

        let (node_state, node_kind, node_data, node_shape, node_parent) = {
            let node = self.arena.get(idx);
            (node.state, node.kind, node.data, node.shape, node.parent)
        };

        if node_state == NodeState::Sealed {
            unsafe { self.heap.drop_in_place(node_data, node_shape) };
        } else {
            // Collect children first to avoid borrow issues
            let mut children = [Idx::NOT_STARTED; MAX_NODE_FIELDS];
            let mut child_count = 0;

            if let NodeKind::Struct { fields } = &node_kind {
                for i in 0..(fields.count as usize) {
                    if let Some(child_idx) = fields.get_child(i) {
                        children[child_count] = child_idx;
                        child_count += 1;
                    }
                }
            }

            // Recursively clean up children first (depth-first)
            for i in 0..child_count {
                self.cleanup_node(children[i]);
            }

            // Now clean up this Node's initialized slots
            match &node_kind {
                NodeKind::Scalar { initialized: true } => {
                    unsafe { self.heap.drop_in_place(node_data, node_shape) };
                }
                NodeKind::Struct { fields } => {
                    let node = self.arena.get(idx);
                    for i in 0..(fields.count as usize) {
                        if matches!(fields.slot(i), FieldSlot::Complete) {
                            let (field_shape, ptr, _size) = Self::field_ptr(node, i)
                                .expect("field metadata should be valid during cleanup");
                            unsafe { self.heap.drop_in_place(ptr, field_shape) };
                        }
                    }
                }
                _ => {}
            }
        }

        // Only root owns the allocation.
        // FIXME: ^ this isn't true. arbitrary Nodes can own their allocations
        if !node_parent.is_valid() {
            unsafe { self.heap.dealloc(node_data, node_shape) };
        }
    }
}

impl<R> Drop for Trame<R>
where
    R: IRuntime,
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
    use crate::runtime::verified::*;
    use core::alloc::Layout;

    #[test]
    fn scalar_lifecycle() {
        let h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
        let shape = vshape_view(h);
        let mut heap = VRuntime::heap();
        let src = unsafe { heap.alloc(shape) };
        unsafe { heap.default_in_place(src, shape) };
        let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

        assert!(!trame.is_complete());
        let root: [PathSegment; 0] = [];
        trame
            .apply(Op::Set {
                dst: &root,
                src: Source::Imm(src),
            })
            .unwrap();
        assert!(trame.is_complete());

        let _ = trame.build().unwrap();
    }

    //     #[test]
    //     fn struct_lifecycle() {
    //         let mut store = VShapeStore::new();
    //         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    //         let struct_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
    //         let struct_h = store.add(struct_def);
    //         let shape = store.view(struct_h);
    //         let u32_shape = store.view(u32_h);

    //         let mut heap = TestHeap::new();
    //         let src0 = unsafe { heap.alloc(u32_shape) };
    //         let src1 = unsafe { heap.alloc(u32_shape) };
    //         unsafe { heap.default_in_place(src0, u32_shape) };
    //         unsafe { heap.default_in_place(src1, u32_shape) };

    //         let arena = TestArena::new();
    //         let mut trame = unsafe { Trame::new(shape) };

    //         assert!(!trame.is_complete());

    //         let f0 = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &f0,
    //                 src: Source::Imm(src0),
    //             })
    //             .unwrap();
    //         assert!(!trame.is_complete());

    //         let f1 = [PathSegment::Field(1)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &f1,
    //                 src: Source::Imm(src1),
    //             })
    //             .unwrap();
    //         assert!(trame.is_complete());

    //         let _ = trame.build().unwrap();
    //     }

    //     #[test]
    //     fn struct_any_order() {
    //         let mut store = VShapeStore::new();
    //         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    //         let struct_def =
    //             VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h), (8, u32_h)]);
    //         let struct_h = store.add(struct_def);
    //         let shape = store.view(struct_h);
    //         let u32_shape = store.view(u32_h);

    //         let mut heap = TestHeap::new();
    //         let src0 = unsafe { heap.alloc(u32_shape) };
    //         let src1 = unsafe { heap.alloc(u32_shape) };
    //         let src2 = unsafe { heap.alloc(u32_shape) };
    //         unsafe { heap.default_in_place(src0, u32_shape) };
    //         unsafe { heap.default_in_place(src1, u32_shape) };
    //         unsafe { heap.default_in_place(src2, u32_shape) };

    //         let arena = TestArena::new();
    //         let mut trame = unsafe { Trame::new(shape) };

    //         // Init in reverse order
    //         let f2 = [PathSegment::Field(2)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &f2,
    //                 src: Source::Imm(src2),
    //             })
    //             .unwrap();
    //         let f0 = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &f0,
    //                 src: Source::Imm(src0),
    //             })
    //             .unwrap();
    //         let f1 = [PathSegment::Field(1)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &f1,
    //                 src: Source::Imm(src1),
    //             })
    //             .unwrap();

    //         assert!(trame.is_complete());
    //         let _ = trame.build().unwrap();
    //     }

    //     #[test]
    //     fn stage_field_twice_reenters() {
    //         let mut store = VShapeStore::new();
    //         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    //         let inner_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h)]);
    //         let inner_h = store.add(inner_def);
    //         let outer_def = VShapeDef::struct_with_fields(&store, &[(0, inner_h)]);
    //         let outer_h = store.add(outer_def);
    //         let shape = store.view(outer_h);

    //         let heap = TestHeap::new();
    //         let arena = TestArena::new();

    //         let mut trame = unsafe { Trame::new(shape) };

    //         let field0 = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &field0,
    //                 src: Source::Stage(None),
    //             })
    //             .unwrap();

    //         let inner_a = [PathSegment::Field(0)];
    //         let u32_shape = store.view(u32_h);
    //         let src = unsafe { trame.heap.alloc(u32_shape) };
    //         unsafe { trame.heap.default_in_place(src, u32_shape) };
    //         trame
    //             .apply(Op::Set {
    //                 dst: &inner_a,
    //                 src: Source::Imm(src),
    //             })
    //             .unwrap();
    //         trame.apply(Op::End).unwrap();

    //         let root_field0 = [PathSegment::Root, PathSegment::Field(0)];
    //         let result = trame.apply(Op::Set {
    //             dst: &root_field0,
    //             src: Source::Stage(None),
    //         });
    //         assert!(result.is_ok());
    //         assert_eq!(trame.depth(), 1);
    //     }

    //     #[test]
    //     fn incomplete_build_fails() {
    //         let mut store = VShapeStore::new();
    //         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    //         let struct_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
    //         let struct_h = store.add(struct_def);
    //         let shape = store.view(struct_h);
    //         let u32_shape = store.view(u32_h);

    //         let mut heap = TestHeap::new();
    //         let src = unsafe { heap.alloc(u32_shape) };
    //         unsafe { heap.default_in_place(src, u32_shape) };

    //         let arena = TestArena::new();
    //         let mut trame = unsafe { Trame::new(shape) };

    //         let f0 = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &f0,
    //                 src: Source::Imm(src),
    //             })
    //             .unwrap();

    //         let err = trame.build();
    //         assert!(matches!(err, Err(TrameError::Incomplete)));
    //     }

    //     #[test]
    //     fn drop_cleans_up() {
    //         let mut store = VShapeStore::new();
    //         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    //         let struct_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
    //         let struct_h = store.add(struct_def);
    //         let shape = store.view(struct_h);
    //         let u32_shape = store.view(u32_h);

    //         let mut heap = TestHeap::new();
    //         let src = unsafe { heap.alloc(u32_shape) };
    //         unsafe { heap.default_in_place(src, u32_shape) };

    //         let arena = TestArena::new();
    //         let mut trame = unsafe { Trame::new(shape) };

    //         let f0 = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &f0,
    //                 src: Source::Imm(src),
    //             })
    //             .unwrap();

    //         drop(trame);
    //     }

    //     // --- Nested struct tests ---

    //     #[test]
    //     fn nested_struct_begin_end() {
    //         let mut store = VShapeStore::new();

    //         // Inner struct: { a: u32, b: u32 }
    //         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    //         let inner_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
    //         let inner_h = store.add(inner_def);

    //         // Outer struct: { x: u32, inner: Inner }
    //         let outer_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, inner_h)]);
    //         let outer_h = store.add(outer_def);
    //         let shape = store.view(outer_h);
    //         let u32_shape = store.view(u32_h);

    //         let mut heap = TestHeap::new();
    //         let src_x = unsafe { heap.alloc(u32_shape) };
    //         let src_a = unsafe { heap.alloc(u32_shape) };
    //         let src_b = unsafe { heap.alloc(u32_shape) };
    //         unsafe { heap.default_in_place(src_x, u32_shape) };
    //         unsafe { heap.default_in_place(src_a, u32_shape) };
    //         unsafe { heap.default_in_place(src_b, u32_shape) };

    //         let arena = TestArena::new();
    //         let mut trame = unsafe { Trame::new(shape) };

    //         assert_eq!(trame.depth(), 0);
    //         assert!(!trame.is_complete());

    //         let outer_x = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &outer_x,
    //                 src: Source::Imm(src_x),
    //             })
    //             .unwrap();
    //         assert!(!trame.is_complete());

    //         let inner_field = [PathSegment::Field(1)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &inner_field,
    //                 src: Source::Stage(None),
    //             })
    //             .unwrap();
    //         assert_eq!(trame.depth(), 1);

    //         let inner_a = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &inner_a,
    //                 src: Source::Imm(src_a),
    //             })
    //             .unwrap();
    //         let inner_b = [PathSegment::Field(1)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &inner_b,
    //                 src: Source::Imm(src_b),
    //             })
    //             .unwrap();
    //         assert!(trame.is_complete());

    //         trame.apply(Op::End).unwrap();
    //         assert_eq!(trame.depth(), 0);
    //         assert!(trame.is_complete());

    //         let _ = trame.build().unwrap();
    //     }

    //     #[test]
    //     fn nested_struct_drop_cleans_up() {
    //         let mut store = VShapeStore::new();

    //         // Inner struct: { a: u32 }
    //         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    //         let inner_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h)]);
    //         let inner_h = store.add(inner_def);

    //         // Outer struct: { inner: Inner }
    //         let outer_def = VShapeDef::struct_with_fields(&store, &[(0, inner_h)]);
    //         let outer_h = store.add(outer_def);
    //         let shape = store.view(outer_h);
    //         let u32_shape = store.view(u32_h);

    //         let mut heap = TestHeap::new();
    //         let src = unsafe { heap.alloc(u32_shape) };
    //         unsafe { heap.default_in_place(src, u32_shape) };

    //         let arena = TestArena::new();
    //         let mut trame = unsafe { Trame::new(shape) };

    //         let inner_field = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &inner_field,
    //                 src: Source::Stage(None),
    //             })
    //             .unwrap();
    //         let inner_a = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &inner_a,
    //                 src: Source::Imm(src),
    //             })
    //             .unwrap();

    //         drop(trame);
    //     }

    //     #[test]
    //     fn nested_struct_trame_inner_cleanup() {
    //         let mut store = VShapeStore::new();

    //         // Inner struct: { a: u32, b: u32 }
    //         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    //         let inner_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
    //         let inner_h = store.add(inner_def);

    //         // Outer struct: { inner: Inner }
    //         let outer_def = VShapeDef::struct_with_fields(&store, &[(0, inner_h)]);
    //         let outer_h = store.add(outer_def);
    //         let shape = store.view(outer_h);
    //         let u32_shape = store.view(u32_h);

    //         let mut heap = TestHeap::new();
    //         let src = unsafe { heap.alloc(u32_shape) };
    //         unsafe { heap.default_in_place(src, u32_shape) };

    //         let arena = TestArena::new();
    //         let mut trame = unsafe { Trame::new(shape) };

    //         let inner_field = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &inner_field,
    //                 src: Source::Stage(None),
    //             })
    //             .unwrap();
    //         let inner_a = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &inner_a,
    //                 src: Source::Imm(src),
    //             })
    //             .unwrap();

    //         drop(trame);
    //     }

    //     #[test]
    //     fn end_op_incomplete_fails() {
    //         let mut store = VShapeStore::new();

    //         // Inner struct: { a: u32, b: u32 }
    //         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    //         let inner_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
    //         let inner_h = store.add(inner_def);

    //         // Outer struct: { inner: Inner }
    //         let outer_def = VShapeDef::struct_with_fields(&store, &[(0, inner_h)]);
    //         let outer_h = store.add(outer_def);
    //         let shape = store.view(outer_h);
    //         let u32_shape = store.view(u32_h);

    //         let mut heap = TestHeap::new();
    //         let src = unsafe { heap.alloc(u32_shape) };
    //         unsafe { heap.default_in_place(src, u32_shape) };

    //         let arena = TestArena::new();
    //         let mut trame = unsafe { Trame::new(shape) };

    //         let inner_field = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &inner_field,
    //                 src: Source::Stage(None),
    //             })
    //             .unwrap();
    //         let inner_a = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &inner_a,
    //                 src: Source::Imm(src),
    //             })
    //             .unwrap();

    //         let err = trame.apply(Op::End);
    //         assert_eq!(err, Err(TrameError::CurrentIncomplete));
    //     }

    //     #[test]
    //     fn end_op_at_root_fails() {
    //         let mut store = VShapeStore::new();
    //         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    //         let struct_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h)]);
    //         let struct_h = store.add(struct_def);
    //         let shape = store.view(struct_h);
    //         let u32_shape = store.view(u32_h);

    //         let mut heap = TestHeap::new();
    //         let src = unsafe { heap.alloc(u32_shape) };
    //         unsafe { heap.default_in_place(src, u32_shape) };

    //         let arena = TestArena::new();
    //         let mut trame = unsafe { Trame::new(shape) };

    //         let f0 = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &f0,
    //                 src: Source::Imm(src),
    //             })
    //             .unwrap();

    //         let err = trame.apply(Op::End);
    //         assert_eq!(err, Err(TrameError::AtRoot));
    //     }

    //     #[test]
    //     fn apply_set_imm_field() {
    //         let mut store = VShapeStore::new();
    //         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    //         let struct_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h)]);
    //         let struct_h = store.add(struct_def);
    //         let shape = store.view(struct_h);
    //         let u32_shape = store.view(u32_h);

    //         let mut heap = TestHeap::new();
    //         let src = unsafe { heap.alloc(u32_shape) };
    //         unsafe { heap.default_in_place(src, u32_shape) };

    //         let arena = TestArena::new();
    //         let mut trame = unsafe { Trame::new(shape) };

    //         let path = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &path,
    //                 src: Source::Imm(src),
    //             })
    //             .unwrap();

    //         assert!(trame.is_complete());
    //         let _ = trame.build().unwrap();
    //     }

    //     #[test]
    //     fn apply_stage_and_end() {
    //         let mut store = VShapeStore::new();
    //         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    //         let inner_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
    //         let inner_h = store.add(inner_def);
    //         let outer_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, inner_h)]);
    //         let outer_h = store.add(outer_def);

    //         let shape = store.view(outer_h);
    //         let u32_shape = store.view(u32_h);

    //         let mut heap = TestHeap::new();
    //         let src1 = unsafe { heap.alloc(u32_shape) };
    //         let src2 = unsafe { heap.alloc(u32_shape) };
    //         unsafe { heap.default_in_place(src1, u32_shape) };
    //         unsafe { heap.default_in_place(src2, u32_shape) };

    //         let arena = TestArena::new();
    //         let mut trame = unsafe { Trame::new(shape) };

    //         let outer_x = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &outer_x,
    //                 src: Source::Imm(src1),
    //             })
    //             .unwrap();

    //         let inner_field = [PathSegment::Field(1)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &inner_field,
    //                 src: Source::Stage(None),
    //             })
    //             .unwrap();

    //         let inner_a = [PathSegment::Field(0)];
    //         trame
    //             .apply(Op::Set {
    //                 dst: &inner_a,
    //                 src: Source::Imm(src2),
    //             })
    //             .unwrap();

    //         let inner_b = [PathSegment::Field(1)];
    //         let src3 = unsafe { trame.heap.alloc(u32_shape) };
    //         unsafe { trame.heap.default_in_place(src3, u32_shape) };
    //         trame
    //             .apply(Op::Set {
    //                 dst: &inner_b,
    //                 src: Source::Imm(src3),
    //             })
    //             .unwrap();

    //         trame.apply(Op::End).unwrap();
    //         assert!(trame.is_complete());
    //         let _ = trame.build().unwrap();
    //     }
}

// #[cfg(kani)]
// mod kani_proofs {
//     use super::*;
//     use crate::runtime::verified::VArena;
//     use crate::runtime::verified::VHeap;
//     use crate::runtime::verified::{
//         IField, IShape, IStructType, MAX_FIELDS_PER_STRUCT, VDef, VFieldDef, VShapeDef,
//         VShapeHandle, VShapeStore, VShapeView, VStructDef,
//     };
//     use core::alloc::Layout;

//     type S<'a> = VShapeView<'a, VShapeStore>;
//     type TestHeap<'a> = VHeap<S<'a>>;
//     type TestArena<'a> = VArena<Node<TestHeap<'a>, S<'a>>, 4>;

//     #[kani::proof]
//     #[kani::unwind(10)]
//     fn scalar_init_complete() {
//         let mut store = VShapeStore::new();
//         let h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 4).unwrap()));
//         let shape = store.view(h);

//         let mut heap = TestHeap::new();
//         let src = unsafe { heap.alloc(shape) };
//         heap.mark_init(src, 4);
//         let arena = TestArena::new();

//         let mut trame = unsafe { Trame::new(shape) };

//         kani::assert(!trame.is_complete(), "not complete initially");
//         let root: [PathSegment; 0] = [];
//         trame
//             .apply(Op::Set {
//                 dst: &root,
//                 src: Source::Imm(src),
//             })
//             .unwrap();
//         kani::assert(trame.is_complete(), "complete after init");
//     }

//     #[kani::proof]
//     #[kani::unwind(10)]
//     fn struct_all_fields_required() {
//         let field_count: u8 = kani::any();
//         kani::assume(field_count > 0 && field_count <= 3);

//         let mut store = VShapeStore::new();
//         let scalar_h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

//         let mut fields_arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//         for i in 0..(field_count as usize) {
//             fields_arr[i] = VFieldDef::new(i * 4, scalar_h);
//         }

//         let struct_def = VShapeDef {
//             layout: Layout::from_size_align((field_count as usize) * 4, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count,
//                 fields: fields_arr,
//             }),
//         };
//         let struct_h = store.add(struct_def);
//         let shape = store.view(struct_h);

//         let mut heap = TestHeap::new();
//         let scalar_shape = store.view(scalar_h);
//         let src = unsafe { heap.alloc(scalar_shape) };
//         heap.mark_init(src, 4);
//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         // Init all but one field
//         let skip_field: u8 = kani::any();
//         kani::assume(skip_field < field_count);

//         for i in 0..(field_count as usize) {
//             if i != skip_field as usize {
//                 let path = [PathSegment::Field(i as u32)];
//                 trame
//                     .apply(Op::Set {
//                         dst: &path,
//                         src: Source::Imm(src),
//                     })
//                     .unwrap();
//             }
//         }

//         // Should not be complete
//         kani::assert(!trame.is_complete(), "incomplete without all fields");

//         // Init the skipped field
//         let path = [PathSegment::Field(skip_field as u32)];
//         trame
//             .apply(Op::Set {
//                 dst: &path,
//                 src: Source::Imm(src),
//             })
//             .unwrap();

//         // Now should be complete
//         kani::assert(trame.is_complete(), "complete with all fields");
//     }

//     /// Prove: staging the same field via Root while in a child re-enters
//     #[kani::proof]
//     #[kani::unwind(10)]
//     fn double_init_rejected() {
//         let mut store = VShapeStore::new();
//         let scalar_h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

//         let inner_def = VShapeDef {
//             layout: Layout::from_size_align(4, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 1,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, scalar_h);
//                     arr
//                 },
//             }),
//         };
//         let inner_h = store.add(inner_def);
//         let outer_def = VShapeDef {
//             layout: Layout::from_size_align(4, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 1,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, inner_h);
//                     arr
//                 },
//             }),
//         };
//         let outer_h = store.add(outer_def);
//         let shape = store.view(outer_h);

//         let heap = TestHeap::new();
//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         let path = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &path,
//                 src: Source::Stage(None),
//             })
//             .unwrap();

//         let scalar_shape = store.view(scalar_h);
//         let src = unsafe { trame.heap.alloc(scalar_shape) };
//         trame.heap.mark_init(src, 4);
//         let inner_a = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_a,
//                 src: Source::Imm(src),
//             })
//             .unwrap();
//         trame.apply(Op::End).unwrap();

//         let root_path = [PathSegment::Root, PathSegment::Field(0)];
//         let result2 = trame.apply(Op::Set {
//             dst: &root_path,
//             src: Source::Stage(None),
//         });
//         kani::assert(result2.is_ok(), "stage after complete re-enters");
//         kani::assert(trame.depth() == 1, "cursor remains in child");
//     }

//     /// Prove: build fails if not all fields initialized
//     #[kani::proof]
//     #[kani::unwind(10)]
//     fn incomplete_finish_fails() {
//         let field_count: u8 = kani::any();
//         kani::assume(field_count >= 2 && field_count <= 3);

//         let mut store = VShapeStore::new();
//         let scalar_h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

//         let mut fields_arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//         for i in 0..(field_count as usize) {
//             fields_arr[i] = VFieldDef::new(i * 4, scalar_h);
//         }

//         let struct_def = VShapeDef {
//             layout: Layout::from_size_align((field_count as usize) * 4, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count,
//                 fields: fields_arr,
//             }),
//         };
//         let struct_h = store.add(struct_def);
//         let shape = store.view(struct_h);

//         let mut heap = TestHeap::new();
//         let scalar_shape = store.view(scalar_h);
//         let src = unsafe { heap.alloc(scalar_shape) };
//         heap.mark_init(src, 4);
//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         // Init only first field
//         let f0 = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &f0,
//                 src: Source::Imm(src),
//             })
//             .unwrap();

//         // build should fail
//         let result = trame.build();
//         kani::assert(result.is_err(), "build fails when incomplete");
//         kani::assert(
//             matches!(result, Err(trame::Incomplete)),
//             "error is Incomplete",
//         );
//     }

//     /// Prove: field index out of bounds returns error
//     #[kani::proof]
//     #[kani::unwind(10)]
//     fn out_of_bounds_rejected() {
//         let field_count: u8 = kani::any();
//         kani::assume(field_count > 0 && field_count <= 3);

//         let mut store = VShapeStore::new();
//         let scalar_h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

//         let mut fields_arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//         for i in 0..(field_count as usize) {
//             fields_arr[i] = VFieldDef::new(i * 4, scalar_h);
//         }

//         let struct_def = VShapeDef {
//             layout: Layout::from_size_align((field_count as usize) * 4, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count,
//                 fields: fields_arr,
//             }),
//         };
//         let struct_h = store.add(struct_def);
//         let shape = store.view(struct_h);

//         let mut heap = TestHeap::new();
//         let scalar_shape = store.view(scalar_h);
//         let src = unsafe { heap.alloc(scalar_shape) };
//         heap.mark_init(src, 4);
//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         // Try to init field beyond bounds
//         let bad_idx: u8 = kani::any();
//         kani::assume(bad_idx >= field_count);
//         kani::assume(bad_idx < 10); // Keep bounded

//         let path = [PathSegment::Field(bad_idx as u32)];
//         let result = trame.apply(Op::Set {
//             dst: &path,
//             src: Source::Imm(src),
//         });
//         kani::assert(result.is_err(), "out of bounds fails");
//         kani::assert(
//             matches!(result, Err(PartialError::FieldOutOfBounds { .. })),
//             "error is FieldOutOfBounds",
//         );
//     }

//     /// Prove: any init order works for completion
//     #[kani::proof]
//     #[kani::unwind(10)]
//     fn any_init_order_completes() {
//         let mut store = VShapeStore::new();
//         let scalar_h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

//         let struct_def = VShapeDef {
//             layout: Layout::from_size_align(12, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 3,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, scalar_h);
//                     arr[1] = VFieldDef::new(4, scalar_h);
//                     arr[2] = VFieldDef::new(8, scalar_h);
//                     arr
//                 },
//             }),
//         };
//         let struct_h = store.add(struct_def);
//         let shape = store.view(struct_h);

//         let mut heap = TestHeap::new();
//         let scalar_shape = store.view(scalar_h);
//         let src0 = unsafe { heap.alloc(scalar_shape) };
//         let src1 = unsafe { heap.alloc(scalar_shape) };
//         let src2 = unsafe { heap.alloc(scalar_shape) };
//         heap.mark_init(src0, 4);
//         heap.mark_init(src1, 4);
//         heap.mark_init(src2, 4);
//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         // Choose arbitrary init order
//         let first: u8 = kani::any();
//         let second: u8 = kani::any();
//         let third: u8 = kani::any();
//         kani::assume(first < 3 && second < 3 && third < 3);
//         kani::assume(first != second && second != third && first != third);

//         let paths = [
//             [PathSegment::Field(0)],
//             [PathSegment::Field(1)],
//             [PathSegment::Field(2)],
//         ];
//         let srcs = [src0, src1, src2];
//         let order = [first as usize, second as usize, third as usize];
//         for idx in order {
//             trame
//                 .apply(Op::Set {
//                     dst: &paths[idx],
//                     src: Source::Imm(srcs[idx]),
//                 })
//                 .unwrap();
//         }

//         kani::assert(
//             trame.is_complete(),
//             "complete after all fields in any order",
//         );

//         let result = trame.build();
//         kani::assert(result.is_ok(), "build succeeds when complete");
//     }

//     /// Prove: nested struct fields are properly tracked
//     /// A struct containing another struct as a field should work correctly.
//     #[kani::proof]
//     #[kani::unwind(10)]
//     fn nested_struct_field_tracking() {
//         let mut store = VShapeStore::new();

//         // Inner struct: { a: u32, b: u32 }
//         let scalar_h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
//         let inner_def = VShapeDef {
//             layout: Layout::from_size_align(8, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 2,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, scalar_h);
//                     arr[1] = VFieldDef::new(4, scalar_h);
//                     arr
//                 },
//             }),
//         };
//         let inner_h = store.add(inner_def);

//         // Outer struct: { x: u32, inner: Inner }
//         let outer_def = VShapeDef {
//             layout: Layout::from_size_align(12, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 2,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, scalar_h); // x: u32
//                     arr[1] = VFieldDef::new(4, inner_h); // inner: Inner (nested struct!)
//                     arr
//                 },
//             }),
//         };
//         let outer_h = store.add(outer_def);
//         let shape = store.view(outer_h);

//         // Verify shape structure via IShape trait
//         kani::assert(shape.is_struct(), "outer is struct");
//         let outer_st = shape.as_struct().unwrap();
//         kani::assert(outer_st.field_count() == 2, "outer has 2 fields");

//         // Field 1 should be the nested struct
//         let field1 = outer_st.field(1).unwrap();
//         let field1_shape = field1.shape();
//         kani::assert(field1_shape.is_struct(), "field 1 is nested struct");

//         // Construct the outer struct
//         let mut heap = TestHeap::new();
//         let scalar_shape = store.view(scalar_h);
//         let src_x = unsafe { heap.alloc(scalar_shape) };
//         let src_a = unsafe { heap.alloc(scalar_shape) };
//         let src_b = unsafe { heap.alloc(scalar_shape) };
//         heap.mark_init(src_x, 4);
//         heap.mark_init(src_a, 4);
//         heap.mark_init(src_b, 4);
//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         let outer_x = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &outer_x,
//                 src: Source::Imm(src_x),
//             })
//             .unwrap();
//         let inner_field = [PathSegment::Field(1)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_field,
//                 src: Source::Stage(None),
//             })
//             .unwrap();
//         let inner_a = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_a,
//                 src: Source::Imm(src_a),
//             })
//             .unwrap();
//         let inner_b = [PathSegment::Field(1)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_b,
//                 src: Source::Imm(src_b),
//             })
//             .unwrap();
//         trame.apply(Op::End).unwrap();

//         kani::assert(trame.is_complete(), "outer complete after both fields");
//         let result = trame.build();
//         kani::assert(result.is_ok(), "build succeeds");
//     }

//     /// Prove: Stage/End lifecycle works correctly
//     #[kani::proof]
//     #[kani::unwind(10)]
//     fn stage_end_lifecycle() {
//         let mut store = VShapeStore::new();

//         // Inner struct: { a: u32, b: u32 }
//         let scalar_h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
//         let inner_def = VShapeDef {
//             layout: Layout::from_size_align(8, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 2,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, scalar_h);
//                     arr[1] = VFieldDef::new(4, scalar_h);
//                     arr
//                 },
//             }),
//         };
//         let inner_h = store.add(inner_def);

//         // Outer struct: { x: u32, inner: Inner }
//         let outer_def = VShapeDef {
//             layout: Layout::from_size_align(12, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 2,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, scalar_h);
//                     arr[1] = VFieldDef::new(4, inner_h);
//                     arr
//                 },
//             }),
//         };
//         let outer_h = store.add(outer_def);
//         let shape = store.view(outer_h);

//         let mut heap = TestHeap::new();
//         let scalar_shape = store.view(scalar_h);
//         let src_x = unsafe { heap.alloc(scalar_shape) };
//         let src_a = unsafe { heap.alloc(scalar_shape) };
//         let src_b = unsafe { heap.alloc(scalar_shape) };
//         heap.mark_init(src_x, 4);
//         heap.mark_init(src_a, 4);
//         heap.mark_init(src_b, 4);
//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         // Verify initial state
//         kani::assert(trame.depth() == 0, "starts at depth 0");
//         kani::assert(!trame.is_complete(), "not complete initially");

//         // Init scalar field
//         let outer_x = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &outer_x,
//                 src: Source::Imm(src_x),
//             })
//             .unwrap();
//         kani::assert(!trame.is_complete(), "not complete with one field");

//         // Stage nested struct
//         let inner_field = [PathSegment::Field(1)];
//         let result = trame.apply(Op::Set {
//             dst: &inner_field,
//             src: Source::Stage(None),
//         });
//         kani::assert(result.is_ok(), "stage succeeds");
//         kani::assert(trame.depth() == 1, "depth is 1 after begin");
//         kani::assert(!trame.is_complete(), "inner not complete yet");

//         // Init inner fields
//         let inner_a = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_a,
//                 src: Source::Imm(src_a),
//             })
//             .unwrap();
//         let inner_b = [PathSegment::Field(1)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_b,
//                 src: Source::Imm(src_b),
//             })
//             .unwrap();
//         kani::assert(trame.is_complete(), "inner complete");

//         // End nested struct
//         let result = trame.apply(Op::End);
//         kani::assert(result.is_ok(), "end succeeds");
//         kani::assert(trame.depth() == 0, "back to depth 0");
//         kani::assert(trame.is_complete(), "outer complete");

//         let result = trame.build();
//         kani::assert(result.is_ok(), "build succeeds");
//     }

//     /// Prove: staging the same field via Root while in a child re-enters
//     #[kani::proof]
//     #[kani::unwind(10)]
//     fn stage_reenter_root_ok() {
//         let mut store = VShapeStore::new();
//         let scalar_h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
//         let inner_def = VShapeDef {
//             layout: Layout::from_size_align(4, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 1,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, scalar_h);
//                     arr
//                 },
//             }),
//         };
//         let inner_h = store.add(inner_def);

//         let outer_def = VShapeDef {
//             layout: Layout::from_size_align(4, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 1,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, inner_h);
//                     arr
//                 },
//             }),
//         };
//         let outer_h = store.add(outer_def);
//         let shape = store.view(outer_h);

//         let heap = TestHeap::new();
//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         let path = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &path,
//                 src: Source::Stage(None),
//             })
//             .unwrap();

//         let scalar_shape = store.view(scalar_h);
//         let src = unsafe { trame.heap.alloc(scalar_shape) };
//         trame.heap.mark_init(src, 4);
//         let inner_a = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_a,
//                 src: Source::Imm(src),
//             })
//             .unwrap();
//         trame.apply(Op::End).unwrap();

//         let root_path = [PathSegment::Root, PathSegment::Field(0)];
//         let result = trame.apply(Op::Set {
//             dst: &root_path,
//             src: Source::Stage(None),
//         });
//         kani::assert(result.is_ok(), "stage after complete re-enters");
//         kani::assert(trame.depth() == 1, "cursor remains in child");
//     }

//     /// Prove: Op::End at root returns error
//     #[kani::proof]
//     #[kani::unwind(10)]
//     fn end_op_at_root_fails() {
//         let mut store = VShapeStore::new();
//         let scalar_h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
//         let struct_def = VShapeDef {
//             layout: Layout::from_size_align(4, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 1,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, scalar_h);
//                     arr
//                 },
//             }),
//         };
//         let struct_h = store.add(struct_def);
//         let shape = store.view(struct_h);

//         let mut heap = TestHeap::new();
//         let scalar_shape = store.view(scalar_h);
//         let src = unsafe { heap.alloc(scalar_shape) };
//         heap.mark_init(src, 4);
//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         let f0 = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &f0,
//                 src: Source::Imm(src),
//             })
//             .unwrap();

//         // Try End at root
//         let result = trame.apply(Op::End);
//         kani::assert(result.is_err(), "end at root fails");
//         kani::assert(
//             matches!(result, Err(PartialError::AtRoot)),
//             "error is AtRoot",
//         );
//     }

//     /// Prove: End with incomplete inner fails
//     #[kani::proof]
//     #[kani::unwind(10)]
//     fn end_op_incomplete_inner_fails() {
//         let mut store = VShapeStore::new();
//         let scalar_h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
//         let inner_def = VShapeDef {
//             layout: Layout::from_size_align(8, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 2,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, scalar_h);
//                     arr[1] = VFieldDef::new(4, scalar_h);
//                     arr
//                 },
//             }),
//         };
//         let inner_h = store.add(inner_def);

//         let outer_def = VShapeDef {
//             layout: Layout::from_size_align(8, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 1,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, inner_h);
//                     arr
//                 },
//             }),
//         };
//         let outer_h = store.add(outer_def);
//         let shape = store.view(outer_h);

//         let mut heap = TestHeap::new();
//         let scalar_shape = store.view(scalar_h);
//         let src = unsafe { heap.alloc(scalar_shape) };
//         heap.mark_init(src, 4);
//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         let inner_field = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_field,
//                 src: Source::Stage(None),
//             })
//             .unwrap();
//         let inner_a = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_a,
//                 src: Source::Imm(src),
//             })
//             .unwrap();

//         // Try End with incomplete inner
//         let result = trame.apply(Op::End);
//         kani::assert(result.is_err(), "end with incomplete inner fails");
//         kani::assert(
//             matches!(result, Err(PartialError::CurrentIncomplete)),
//             "error is CurrentIncomplete",
//         );
//     }

//     /// Prove: drop properly cleans up nested Nodes (depth-first)
//     #[kani::proof]
//     #[kani::unwind(12)]
//     fn nested_drop_cleanup() {
//         let mut store = VShapeStore::new();
//         let scalar_h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
//         let inner_def = VShapeDef {
//             layout: Layout::from_size_align(4, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 1,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, scalar_h);
//                     arr
//                 },
//             }),
//         };
//         let inner_h = store.add(inner_def);

//         let outer_def = VShapeDef {
//             layout: Layout::from_size_align(4, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 1,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, inner_h);
//                     arr
//                 },
//             }),
//         };
//         let outer_h = store.add(outer_def);
//         let shape = store.view(outer_h);

//         let mut heap = TestHeap::new();
//         let scalar_shape = store.view(scalar_h);
//         let src = unsafe { heap.alloc(scalar_shape) };
//         heap.mark_init(src, 4);
//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         // Enter nested struct
//         let inner_field = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_field,
//                 src: Source::Stage(None),
//             })
//             .unwrap();
//         // Init inner field
//         let inner_a = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_a,
//                 src: Source::Imm(src),
//             })
//             .unwrap();
//         // Don't end - just drop

//         // Drop should clean up child Node before parent
//         drop(trame);
//         // No panic = cleanup order is correct
//     }

//     /// Prove: depth tracking is correct through nested begin/end
//     #[kani::proof]
//     #[kani::unwind(10)]
//     fn depth_tracking_correct() {
//         let mut store = VShapeStore::new();
//         let scalar_h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
//         let inner_def = VShapeDef {
//             layout: Layout::from_size_align(4, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 1,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, scalar_h);
//                     arr
//                 },
//             }),
//         };
//         let inner_h = store.add(inner_def);

//         let outer_def = VShapeDef {
//             layout: Layout::from_size_align(4, 1).unwrap(),
//             def: VDef::Struct(VStructDef {
//                 field_count: 1,
//                 fields: {
//                     let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
//                     arr[0] = VFieldDef::new(0, inner_h);
//                     arr
//                 },
//             }),
//         };
//         let outer_h = store.add(outer_def);
//         let shape = store.view(outer_h);

//         let mut heap = TestHeap::new();
//         let scalar_shape = store.view(scalar_h);
//         let src = unsafe { heap.alloc(scalar_shape) };
//         heap.mark_init(src, 4);
//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         kani::assert(trame.depth() == 0, "initial depth is 0");

//         let inner_field = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_field,
//                 src: Source::Stage(None),
//             })
//             .unwrap();
//         kani::assert(trame.depth() == 1, "depth is 1 after begin");

//         let inner_a = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_a,
//                 src: Source::Imm(src),
//             })
//             .unwrap();
//         kani::assert(trame.depth() == 1, "depth still 1 after inner init");

//         trame.apply(Op::End).unwrap();
//         kani::assert(trame.depth() == 0, "depth back to 0 after end");
//     }

//     /// Prove: Stage uses the same allocation as the parent.
//     #[kani::proof]
//     #[kani::unwind(8)]
//     fn stage_same_alloc_id() {
//         let mut store = VShapeStore::new();
//         let u32_h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
//         let inner_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
//         let inner_h = store.add(inner_def);
//         let outer_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, inner_h)]);
//         let outer_h = store.add(outer_def);
//         let shape = store.view(outer_h);

//         let heap = TestHeap::new();
//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         let inner_field = [PathSegment::Field(1)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_field,
//                 src: Source::Stage(None),
//             })
//             .unwrap();

//         let child = trame.current;
//         let child_Node = trame.arena.get(child);
//         let parent_Node = trame.arena.get(child_Node.parent);

//         kani::assert(
//             child_Node.data.alloc_id() == parent_Node.data.alloc_id(),
//             "same allocation id",
//         );
//         kani::assert(
//             child_Node.data.offset_bytes() == 4,
//             "offset is field offset",
//         );
//     }
// }
