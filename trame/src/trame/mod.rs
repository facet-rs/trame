//! Partial value construction.
//!
//! `Trame` manages incremental construction of a value, tracking which
//! fields have been initialized and ensuring proper cleanup on failure.

mod errors;
mod heap_value;
pub use errors::TrameError;
pub use heap_value::HeapValue;

#[cfg(creusot)]
use crate::node::FieldStates;
#[cfg(not(creusot))]
use crate::runtime::LiveRuntime;
use crate::{
    Op, PathSegment, Source,
    node::{FieldSlot, Node, NodeKind, NodeState},
    ops::SourceKind,
    runtime::{IArena, IField, IHeap, IPtr, IRuntime, IShape, IStructType, Idx},
};
use core::marker::PhantomData;

#[cfg(creusot)]
use crate::runtime::IShapeExtra as _;
#[cfg(creusot)]
use creusot_std::{
    invariant::Invariant,
    macros::{ensures, logic, pearlite, proof_assert, requires, snapshot, trusted},
    snapshot::Snapshot,
};

#[cfg(creusot)]
#[trusted]
#[ensures(*b)]
fn assume(b: Snapshot<bool>) {}

type Heap<R> = <R as IRuntime>::Heap;
type Shape<R> = <R as IRuntime>::Shape;
type NodeIdx<R> = Idx<Node<Heap<R>, Shape<R>>>;
type Ptr<R> = <Heap<R> as IHeap<Shape<R>>>::Ptr;

#[cfg(creusot)]
#[ensures(result == Ok(()) ==> field_idx@ < fields.len_logic())]
fn prove_field_idx_in_bounds<F>(
    fields: &FieldStates<F>,
    field_idx: usize,
) -> Result<(), TrameError> {
    let field_count = fields.len();
    if field_idx < field_count {
        fields.prove_idx_in_bounds(field_idx, field_count);
        Ok(())
    } else {
        Err(TrameError::FieldOutOfBounds {
            index: field_idx,
            count: field_count,
        })
    }
}

// ============================================================================
// Trame - the main builder
// ============================================================================

/// Manages incremental construction of a value.
pub struct Trame<'facet, R>
where
    R: IRuntime,
{
    /// The heap for memory operations.
    heap: R::Heap,

    /// Arena holding Nodes.
    arena: R::Arena<Node<R::Heap, R::Shape>>,

    /// Root Node index.
    root: Idx<Node<R::Heap, R::Shape>>,

    /// Current Node we're building.
    current: Idx<Node<R::Heap, R::Shape>>,

    /// Whether we've been poisoned (error occurred).
    poisoned: bool,

    _marker: PhantomData<&'facet ()>,
}

#[cfg(creusot)]
impl<'facet, R> Invariant for Trame<'facet, R>
where
    R: IRuntime,
{
    #[logic]
    fn invariant(self) -> bool {
        pearlite! {
            self.arena.contains(self.root) &&
            self.arena.contains(self.current) &&
            forall<i> self.arena.contains(i) ==> {
                let node = self.arena.get_logic(i);
                (i == self.root || self.arena.contains(node.parent)) &&
                match node.kind {
                    NodeKind::Scalar { initialized } =>
                        initialized == self.heap.range_init(node.data, node.shape.size_logic())
                        && initialized == self.heap.can_drop(node.data, node.shape),
                    NodeKind::Struct { .. } => true,
                }
            } && forall<j> i != j && self.arena.contains(j) ==> {
                let nodei = self.arena.get_logic(i);
                let nodej = self.arena.get_logic(j);
                nodei.data != nodej.data
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct FieldMeta<S: IShape> {
    shape: S,
    offset: usize,
    layout: trame_runtime::VLayout,
}

#[cfg(creusot)]
fn layout_size(layout: trame_runtime::VLayout) -> usize {
    layout.size
}

#[cfg(not(creusot))]
fn layout_size(layout: trame_runtime::VLayout) -> usize {
    layout.size()
}

#[cfg(creusot)]
#[trusted]
fn vlayout_from_layout(layout: std::alloc::Layout) -> trame_runtime::VLayout {
    trame_runtime::VLayout::from_size_align(layout.size(), layout.align()).expect("valid layout")
}

#[cfg(not(creusot))]
fn vlayout_from_layout(layout: std::alloc::Layout) -> trame_runtime::VLayout {
    trame_runtime::VLayout::from_size_align(layout.size(), layout.align()).expect("valid layout")
}

#[cfg(creusot)]
#[trusted]
#[ensures(layout == Some(result))]
fn layout_expect(layout: Option<std::alloc::Layout>) -> std::alloc::Layout {
    layout.expect("IShape requires sized types")
}

#[cfg(not(creusot))]
fn layout_expect(layout: Option<std::alloc::Layout>) -> std::alloc::Layout {
    layout.expect("IShape requires sized types")
}


#[cfg(creusot)]
#[trusted]
#[ensures(result == heap.range_init(ptr, len))]
fn heap_range_init<H, S>(heap: &H, ptr: <H as IHeap<S>>::Ptr, len: usize) -> bool
where
    H: IHeap<S>,
    S: IShape,
{
    false
}

#[cfg(creusot)]
#[trusted]
#[ensures(result == heap.can_drop(ptr, shape))]
fn heap_can_drop<H, S>(heap: &H, ptr: <H as IHeap<S>>::Ptr, shape: S) -> bool
where
    H: IHeap<S>,
    S: IShape,
{
    false
}

#[cfg(creusot)]
#[trusted]
#[ensures(meta.offset + meta.layout.size <= node_size)]
fn assume_field_ptr_in_bounds<S: IShape>(meta: &FieldMeta<S>, node_size: usize) {}

impl<'facet, R> Trame<'facet, R>
where
    R: IRuntime,
{
    /// Create a new Trame for the given shape.
    ///
    /// # Safety
    /// The caller must ensure the shape is valid and sized.
    #[cfg_attr(creusot, trusted)]
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
            _marker: PhantomData,
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
        let layout = layout_expect(field_shape.layout());
        let layout = vlayout_from_layout(layout);
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
        let node_size = layout_size(vlayout_from_layout(layout_expect(node.shape.layout())));
        #[cfg(creusot)]
        assume_field_ptr_in_bounds(&meta, node_size);
        #[cfg(not(creusot))]
        assert!(
            meta.offset + layout_size(meta.layout) <= node_size,
            "field out of bounds: {} + {} > {}",
            meta.offset,
            layout_size(meta.layout),
            node_size
        );
        Ok((
            meta.shape,
            unsafe { node.data.byte_add(meta.offset) },
            layout_size(meta.layout),
        ))
    }

    #[cfg_attr(creusot, trusted)]
    #[cfg_attr(creusot, ensures(match result {
        Ok((node, _)) => (^self).arena.contains(node),
        Err(_) => true,
    }))]
    fn resolve_path(
        &mut self,
        path: &[PathSegment],
    ) -> Result<(NodeIdx<R>, Option<usize>), TrameError> {
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

    #[cfg_attr(creusot, trusted)]
    fn ascend_to_root(&mut self) -> Result<NodeIdx<R>, TrameError> {
        while !self.current.same(self.root) {
            self.end_current_node()?;
        }
        Ok(self.current)
    }

    #[cfg_attr(creusot, requires(self.arena.contains(target_idx)))]
    #[cfg_attr(creusot, ensures(match result {
        Ok(()) => {
            let node = self.arena.get_logic(target_idx);
            match node.kind {
                NodeKind::Scalar { .. } => match src.kind {
                    SourceKind::Stage(_) => false,
                    _ => true,
                },
                _ => true,
            }
        },
        Err(_) => true,
    }))]
    fn apply_set(
        &mut self,
        target_idx: NodeIdx<R>,
        field_idx: Option<usize>,
        src: Source<Ptr<R>, Shape<R>>,
    ) -> Result<(), TrameError>
    where
        Shape<R>: IShape + PartialEq,
    {
        let node = self.arena.get(target_idx);

        match field_idx {
            None => match &node.kind {
                NodeKind::Scalar { .. } => {
                    let shape = node.shape;
                    let size = layout_expect(shape.layout()).size();
                    let dst = node.data;
                    let already_init = matches!(&node.kind, NodeKind::Scalar { initialized: true });

                    if already_init {
                        unsafe { self.heap.drop_in_place(dst, shape) };
                        // Mark as uninitialized immediately after drop - if the subsequent
                        // write fails, we must not leave the node claiming to be initialized
                        let node = self.arena.get_mut(target_idx);
                        if let NodeKind::Scalar { initialized } = &mut node.kind {
                            *initialized = false;
                        }
                    }

                    match src.kind {
                        SourceKind::Imm(imm) => {
                            let src_ptr = imm.ptr;
                            let src_shape = imm.shape;
                            if src_shape != shape {
                                return Err(TrameError::ShapeMismatch);
                            }
                            #[cfg(creusot)]
                            assume(snapshot! { self.heap.range_init(src_ptr, size) });
                            unsafe { self.heap.memcpy(dst, src_ptr, size) };
                        }
                        SourceKind::Default => {
                            let ok = unsafe { self.heap.default_in_place(dst, shape) };
                            if !ok {
                                return Err(TrameError::DefaultUnavailable);
                            }
                        }
                        SourceKind::Stage(_) => return Err(TrameError::UnsupportedSource),
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
                        let field_count = fields.len();
                        if field_idx < field_count {
                            #[cfg(creusot)]
                            {
                                prove_field_idx_in_bounds(fields, field_idx)?;
                            }
                            (fields.get_child(field_idx), fields.is_init(field_idx))
                        } else {
                            return Err(TrameError::FieldOutOfBounds {
                                index: field_idx,
                                count: field_count,
                            });
                        }
                    }
                    NodeKind::Scalar { .. } => return Err(TrameError::NotAStruct),
                };

                #[cfg(creusot)]
                assume(snapshot! { false });
                let (field_shape, dst, size) = Self::field_ptr(node, field_idx)?;

                if let Some(child) = child_idx {
                    if matches!(src.kind, SourceKind::Imm { .. } | SourceKind::Default) {
                        // We are overwriting a staged field: drop any initialized subfields.
                        self.cleanup_node(child);
                        if self.current_in_subtree(child) {
                            self.current = target_idx;
                        }
                        if let NodeKind::Struct { fields } =
                            &mut self.arena.get_mut(target_idx).kind
                        {
                            #[cfg(creusot)]
                            {
                                prove_field_idx_in_bounds(fields, field_idx)?;
                            }
                            fields.mark_not_started(field_idx);
                        }
                        child_idx = None;
                        already_init = false;
                    }
                }

                if already_init {
                    unsafe { self.heap.drop_in_place(dst, field_shape) };
                    if let NodeKind::Struct { fields } = &mut self.arena.get_mut(target_idx).kind {
                        #[cfg(creusot)]
                        {
                            prove_field_idx_in_bounds(fields, field_idx)?;
                        }
                        fields.mark_not_started(field_idx);
                    }
                }

                match src.kind {
                    SourceKind::Imm(imm) => {
                        let src_ptr = imm.ptr;
                        let src_shape = imm.shape;
                        if src_shape != field_shape {
                            return Err(TrameError::ShapeMismatch);
                        }
                        unsafe { self.heap.memcpy(dst, src_ptr, size) };
                        if let NodeKind::Struct { fields } =
                            &mut self.arena.get_mut(target_idx).kind
                        {
                            #[cfg(creusot)]
                            {
                                prove_field_idx_in_bounds(fields, field_idx)?;
                            }
                            fields.mark_complete(field_idx);
                        }
                        Ok(())
                    }
                    SourceKind::Default => {
                        let ok = unsafe { self.heap.default_in_place(dst, field_shape) };
                        if !ok {
                            return Err(TrameError::DefaultUnavailable);
                        }
                        if let NodeKind::Struct { fields } =
                            &mut self.arena.get_mut(target_idx).kind
                        {
                            #[cfg(creusot)]
                            {
                                prove_field_idx_in_bounds(fields, field_idx)?;
                            }
                            fields.mark_complete(field_idx);
                        }
                        Ok(())
                    }
                    SourceKind::Stage(_cap) => {
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
                        };

                        let child_idx = self.arena.alloc(child_node);
                        if let NodeKind::Struct { fields } =
                            &mut self.arena.get_mut(target_idx).kind
                        {
                            #[cfg(creusot)]
                            {
                                prove_field_idx_in_bounds(fields, field_idx)?;
                            }
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

    #[cfg_attr(creusot, trusted)]
    fn current_in_subtree(&self, ancestor: NodeIdx<R>) -> bool {
        let mut idx = self.current;
        while idx.is_valid() {
            if idx.same(ancestor) {
                return true;
            }
            let node = self.arena.get(idx);
            idx = node.parent;
        }
        false
    }

    #[cfg_attr(creusot, trusted)]
    fn node_is_complete(&self, idx: NodeIdx<R>) -> bool {
        let node = self.arena.get(idx);
        match &node.kind {
            NodeKind::Scalar { initialized } => *initialized,
            NodeKind::Struct { fields } => {
                let field_count = fields.len();
                let mut i = 0;
                while i < field_count {
                    #[cfg(creusot)]
                    {
                        fields.prove_idx_in_bounds(i, field_count);
                    }
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
                    i += 1;
                }
                true
            }
        }
    }

    pub fn apply(&mut self, op: Op<'_, Ptr<R>, Shape<R>>) -> Result<(), TrameError>
    where
        Shape<R>: IShape + PartialEq,
    {
        self.check_poisoned()?;

        match op {
            Op::End => self.end_current_node(),
            Op::Set { dst, src } => {
                let (target, field_idx) = self.resolve_path(dst.segments())?;
                self.apply_set(target, field_idx, src)
            }
        }
    }

    /// End constructing the current Node and move the cursor to the parent.
    ///
    /// This marks the field as initialized in the parent Node and pops back.
    /// The current Node must be complete.
    #[cfg_attr(creusot, trusted)]
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
    #[cfg_attr(creusot, trusted)]
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

    /// Build the value, returning a HeapValue wrapper.
    ///
    /// Returns error if not all fields are initialized.
    #[cfg_attr(creusot, trusted)]
    pub fn build(self) -> Result<HeapValue<'facet, R>, TrameError> {
        self.check_poisoned()?;

        if !self.node_is_complete(self.current) {
            return Err(TrameError::Incomplete);
        }

        let mut this = self;
        let (data, shape) = {
            let node = this.arena.get(this.root);
            (node.data, node.shape)
        };
        let heap = core::mem::replace(&mut this.heap, R::heap());
        let arena = core::mem::replace(&mut this.arena, R::arena());
        drop(arena);
        this.poisoned = true;

        Ok(HeapValue::new(heap, data, shape))
    }
}

impl<'facet, R> Trame<'facet, R>
where
    R: IRuntime,
{
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
    #[cfg_attr(creusot, trusted)]
    fn cleanup_node(&mut self, idx: NodeIdx<R>) {
        if !idx.is_valid() {
            return;
        }

        let (node_state, node_kind, node_data, node_shape, node_parent) = {
            let node = self.arena.get(idx);
            (
                node.state,
                node.kind.clone(),
                node.data,
                node.shape,
                node.parent,
            )
        };

        if node_state == NodeState::Sealed {
            #[cfg(creusot)]
            if heap_can_drop::<Heap<R>, Shape<R>>(&self.heap, node_data, node_shape) {
                unsafe { self.heap.drop_in_place(node_data, node_shape) };
            }
            #[cfg(not(creusot))]
            unsafe {
                self.heap.drop_in_place(node_data, node_shape);
            }
        } else {
            // Collect children first to avoid borrow issues
            let mut children = Vec::new();

            if let NodeKind::Struct { fields } = &node_kind {
                let field_count = fields.len();
                let mut i = 0;
                while i < field_count {
                    #[cfg(creusot)]
                    {
                        fields.prove_idx_in_bounds(i, field_count);
                    }
                    if let Some(child_idx) = fields.get_child(i) {
                        children.push(child_idx);
                    }
                    i += 1;
                }
            }

            // Recursively clean up children first (depth-first)
            for child in children.iter() {
                self.cleanup_node(*child);
            }

            // Now clean up this Node's initialized slots
            match &node_kind {
                NodeKind::Scalar { initialized: true } => {
                    #[cfg(creusot)]
                    if heap_can_drop::<Heap<R>, Shape<R>>(&self.heap, node_data, node_shape) {
                        unsafe { self.heap.drop_in_place(node_data, node_shape) };
                    }
                    #[cfg(not(creusot))]
                    unsafe {
                        self.heap.drop_in_place(node_data, node_shape);
                    }
                }
                NodeKind::Struct { fields } => {
                    let node = self.arena.get(idx);
                    let field_count = fields.len();
                    let mut i = 0;
                    while i < field_count {
                        #[cfg(creusot)]
                        {
                            fields.prove_idx_in_bounds(i, field_count);
                        }
                        if matches!(fields.slot(i), FieldSlot::Complete) {
                            if let Ok((field_shape, ptr, _size)) = Self::field_ptr(node, i) {
                                #[cfg(creusot)]
                                if heap_can_drop::<Heap<R>, Shape<R>>(&self.heap, ptr, field_shape)
                                {
                                    unsafe { self.heap.drop_in_place(ptr, field_shape) };
                                }
                                #[cfg(not(creusot))]
                                unsafe {
                                    self.heap.drop_in_place(ptr, field_shape);
                                }
                            }
                        }
                        i += 1;
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

#[cfg(all(creusot, feature = "creusot-canary"))]
mod creusot_canary {
    use crate::runtime::{IHeap, IRuntime};
    use crate::trame::Trame;

    // Canary: should fail if cross-crate requires are enforced.
    #[allow(dead_code)]
    pub fn drop_requires_are_enforced<R: IRuntime>(
        mut trame: Trame<'_, R>,
        ptr: <R::Heap as IHeap<R::Shape>>::Ptr,
        shape: R::Shape,
    ) {
        // Intentionally violate the precondition.
        unsafe { trame.heap.drop_in_place(ptr, shape) };
    }
}

#[cfg(not(creusot))]
impl<'facet, R> Trame<'facet, R>
where
    R: LiveRuntime,
    Shape<R>: IShape + PartialEq,
{
    /// Allocate a Trame using a concrete Facet type.
    pub fn alloc<T: facet_core::Facet<'facet>>() -> Result<Self, TrameError> {
        let shape = T::SHAPE;
        Ok(unsafe { Trame::new(R::heap(), shape) })
    }

    /// Allocate a Trame using a shape directly.
    ///
    /// # Safety
    /// The caller must ensure `shape` matches the type that will be materialized.
    pub unsafe fn alloc_shape(shape: &'static facet_core::Shape) -> Result<Self, TrameError> {
        Ok(unsafe { Trame::new(R::heap(), shape) })
    }
}

impl<'facet, R> Drop for Trame<'facet, R>
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
mod tests;

#[cfg(kani)]
mod proofs;
