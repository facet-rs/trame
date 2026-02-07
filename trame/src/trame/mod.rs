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
    node::{FieldSlot, Node, NodeFlags, NodeKind, NodeState},
    ops::SourceKind,
    runtime::{
        CopyDesc, IArena, IField, IHeap, IPointerType, IPtr, IRuntime, IShape, IStructType, Idx,
    },
};
use core::marker::PhantomData;

#[cfg(creusot)]
use crate::runtime::IShapeExtra as _;
#[cfg(creusot)]
use creusot_std::{
    invariant::Invariant,
    macros::{ensures, logic, pearlite, proof_assert, requires, snapshot, trusted},
    snapshot::{self, Snapshot},
};

#[cfg(creusot)]
#[trusted]
#[ensures(*b)]
pub fn assume(b: Snapshot<bool>) {}

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
                    NodeKind::Pointer { initialized, .. } =>
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

    // todo(creusot): specification inference should work on standard functions
    // so that we can drop all these specs here. this is absurd
    #[cfg_attr(creusot, requires(self.arena.contains(target_idx)))]
    #[cfg_attr(creusot, requires(
        match self.arena.get_logic(target_idx).kind {
            NodeKind::Scalar { .. } => {
                let node = self.arena.get_logic(target_idx);
                initialized == self.heap.range_init(node.data, node.shape.size_logic())
                && initialized == self.heap.can_drop(node.data, node.shape)
            },
            _ => true,
        }
    ))]
    #[cfg_attr(
        creusot,
        ensures(match (^self).arena.get_logic(target_idx).kind {
            NodeKind::Scalar { initialized: state } => state == initialized,
            _ => true,
        })
    )]
    fn set_scalar_initialized(&mut self, target_idx: NodeIdx<R>, initialized: bool) {
        let node = self.arena.get_mut(target_idx);
        if let NodeKind::Scalar {
            initialized: state, ..
        } = &mut node.kind
        {
            *state = initialized;
        }
    }

    #[cfg_attr(creusot, requires(self.arena.contains(target_idx)))]
    #[cfg_attr(creusot, requires(
      match self.arena.get_logic(target_idx).kind {
          NodeKind::Pointer { .. } => {
              let node = self.arena.get_logic(target_idx);
              initialized == self.heap.range_init(node.data, node.shape.size_logic())
              && initialized == self.heap.can_drop(node.data, node.shape)
          },
          _ => true,
      }
    ))]
    #[cfg_attr(
        creusot,
        ensures(match (^self).arena.get_logic(target_idx).kind {
            NodeKind::Pointer { initialized: state, .. } => state == initialized,
            _ => true,
        })
    )]
    fn set_pointer_initialized(&mut self, target_idx: NodeIdx<R>, initialized: bool) {
        let node = self.arena.get_mut(target_idx);
        if let NodeKind::Pointer {
            initialized: state, ..
        } = &mut node.kind
        {
            *state = initialized;
        }
    }

    #[cfg_attr(creusot, requires(self.arena.contains(target_idx)))]
    #[cfg_attr(
        creusot,
        ensures(match (^self).arena.get_logic(target_idx).kind {
            NodeKind::Pointer { child, .. } => child == None,
            _ => true,
        })
    )]
    fn clear_pointer_child(&mut self, target_idx: NodeIdx<R>) {
        if let NodeKind::Pointer { child, .. } = &mut self.arena.get_mut(target_idx).kind {
            *child = None;
        }
    }

    #[cfg_attr(creusot, requires(self.arena.contains(target_idx)))]
    #[cfg_attr(
        creusot,
        requires(match self.arena.get_logic(target_idx).kind {
            NodeKind::Struct { fields } => field_idx@ < fields.len_logic(),
            _ => true,
        })
    )]
    #[cfg_attr(
        creusot,
        requires(match self.arena.get_logic(target_idx).kind {
            NodeKind::Pointer { .. } =>
                !self.heap.range_init(
                    self.arena.get_logic(target_idx).data,
                    self.arena.get_logic(target_idx).shape.size_logic())
                && !self.heap.can_drop(
                    self.arena.get_logic(target_idx).data,
                    self.arena.get_logic(target_idx).shape),
            _ => true,
        })
    )]
    #[cfg_attr(
        creusot,
        ensures(match (^self).arena.get_logic(target_idx).kind {
            NodeKind::Struct { fields } => match fields.slots@[field_idx@] {
                FieldSlot::Complete => false,
                _ => true,
            },
            NodeKind::Pointer { initialized, .. } => !initialized,
            _ => true,
        })
    )]
    fn mark_field_not_started(&mut self, target_idx: NodeIdx<R>, field_idx: usize) {
        match &mut self.arena.get_mut(target_idx).kind {
            NodeKind::Struct { fields } => fields.mark_not_started(field_idx),
            NodeKind::Pointer { initialized, .. } => *initialized = false,
            _ => {}
        }
    }

    #[cfg_attr(creusot, requires(self.arena.contains(target_idx)))]
    #[cfg_attr(
        creusot,
        requires(match self.arena.get_logic(target_idx).kind {
            NodeKind::Struct { fields } => field_idx@ < fields.len_logic(),
            _ => true,
        })
    )]
    #[cfg_attr(
        creusot,
        requires(match self.arena.get_logic(target_idx).kind {
            NodeKind::Pointer { .. } =>
                self.heap.range_init(
                    self.arena.get_logic(target_idx).data,
                    self.arena.get_logic(target_idx).shape.size_logic())
                && self.heap.can_drop(
                    self.arena.get_logic(target_idx).data,
                    self.arena.get_logic(target_idx).shape),
            _ => true,
        })
    )]
    #[cfg_attr(
        creusot,
        ensures(match (^self).arena.get_logic(target_idx).kind {
            NodeKind::Struct { fields } => match fields.slots@[field_idx@] {
                FieldSlot::Complete => true,
                _ => false,
            },
            NodeKind::Pointer { initialized, .. } => initialized,
            _ => true,
        })
    )]
    fn mark_field_complete(&mut self, target_idx: NodeIdx<R>, field_idx: usize) {
        match &mut self.arena.get_mut(target_idx).kind {
            NodeKind::Struct { fields } => fields.mark_complete(field_idx),
            NodeKind::Pointer { initialized, .. } => *initialized = true,
            _ => {}
        }
    }

    #[cfg_attr(creusot, requires(self.arena.contains(target_idx)))]
    #[cfg_attr(creusot, ensures(match result {
        Ok(()) => {
            let node = self.arena.get_logic(target_idx);
            match node.kind {
                NodeKind::Scalar { .. } | NodeKind::Pointer { .. } => match src.kind {
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
        let (target_kind, target_shape, target_data) = {
            let node = self.arena.get(target_idx);
            (node.kind.clone(), node.shape, node.data)
        };

        #[cfg(creusot)]
        assume(snapshot! { false });

        match field_idx {
            None => match &target_kind {
                NodeKind::Scalar { initialized } => self.apply_set_direct_scalar(
                    target_idx,
                    target_shape,
                    target_data,
                    *initialized,
                    src,
                ),
                NodeKind::Pointer { initialized, child } => self.apply_set_direct_pointer(
                    target_idx,
                    target_shape,
                    target_data,
                    *initialized,
                    *child,
                    src,
                ),
                NodeKind::Struct { .. } => Err(TrameError::NotAStruct),
            },
            Some(field_idx) => self.apply_set_field(
                target_idx,
                target_kind,
                target_shape,
                target_data,
                field_idx,
                src,
            ),
        }
    }

    #[cfg_attr(creusot, requires(self.arena.contains(target_idx)))]
    #[cfg_attr(creusot, requires(self.arena.get_logic(target_idx).data == dst))]
    #[cfg_attr(creusot, requires(self.arena.get_logic(target_idx).shape == shape))]
    #[cfg_attr(creusot, requires(
        match self.arena.get_logic(target_idx).kind {
            NodeKind::Scalar { initialized } => initialized == already_init,
            _ => false,
        }
    ))]
    fn apply_set_direct_scalar(
        &mut self,
        target_idx: NodeIdx<R>,
        shape: Shape<R>,
        dst: Ptr<R>,
        already_init: bool,
        src: Source<Ptr<R>, Shape<R>>,
    ) -> Result<(), TrameError>
    where
        Shape<R>: IShape + PartialEq,
    {
        #[cfg(creusot)]
        let size = layout_size(vlayout_from_layout(layout_expect(shape.layout())));
        #[cfg(creusot)]
        assume(snapshot! { size == shape.size_logic() });

        if already_init {
            unsafe { self.heap.drop_in_place(dst, shape) };
            // Inline update: avoid calling set_scalar_initialized to prevent
            // type invariant check while heap and arena are temporarily inconsistent.
            if let NodeKind::Scalar {
                initialized: state, ..
            } = &mut self.arena.get_mut(target_idx).kind
            {
                *state = false;
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
                unsafe { self.heap.memcpy(dst, src_ptr, CopyDesc::value(shape)) };
            }
            SourceKind::Default => {
                let ok = unsafe { self.heap.default_in_place(dst, shape) };
                if !ok {
                    return Err(TrameError::DefaultUnavailable);
                }
            }
            SourceKind::Stage(_) => return Err(TrameError::UnsupportedSource),
        }

        // Inline update: set initialized = true after the value is written.
        if let NodeKind::Scalar {
            initialized: state, ..
        } = &mut self.arena.get_mut(target_idx).kind
        {
            *state = true;
        }
        Ok(())
    }

    fn apply_set_direct_pointer(
        &mut self,
        target_idx: NodeIdx<R>,
        shape: Shape<R>,
        dst: Ptr<R>,
        already_init: bool,
        existing_child: Option<NodeIdx<R>>,
        src: Source<Ptr<R>, Shape<R>>,
    ) -> Result<(), TrameError>
    where
        Shape<R>: IShape + PartialEq,
    {
        #[cfg(creusot)]
        assume(snapshot! { false });

        #[cfg(creusot)]
        let size = layout_size(vlayout_from_layout(layout_expect(shape.layout())));
        #[cfg(creusot)]
        assume(snapshot! { size == shape.size_logic() });

        if let Some(child) = existing_child {
            self.cleanup_node(child);
            if self.current_in_subtree(child) {
                self.current = target_idx;
            }
            self.clear_pointer_child(target_idx);
        }

        if already_init {
            unsafe { self.heap.drop_in_place(dst, shape) };
            self.set_pointer_initialized(target_idx, false);
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
                unsafe { self.heap.memcpy(dst, src_ptr, CopyDesc::value(shape)) };
            }
            SourceKind::Default => {
                let ok = unsafe { self.heap.default_in_place(dst, shape) };
                if !ok {
                    return Err(TrameError::DefaultUnavailable);
                }
            }
            SourceKind::Stage(_) => return Err(TrameError::UnsupportedSource),
        }

        self.set_pointer_initialized(target_idx, true);
        Ok(())
    }

    fn apply_set_field(
        &mut self,
        target_idx: NodeIdx<R>,
        target_kind: NodeKind<Node<Heap<R>, Shape<R>>>,
        target_shape: Shape<R>,
        target_data: Ptr<R>,
        field_idx: usize,
        src: Source<Ptr<R>, Shape<R>>,
    ) -> Result<(), TrameError>
    where
        Shape<R>: IShape + PartialEq,
    {
        #[cfg(creusot)]
        assume(snapshot! { false });

        let (mut child_idx, mut already_init, is_pointer_parent) = match &target_kind {
            NodeKind::Struct { fields } => {
                let field_count = fields.len();
                if field_idx < field_count {
                    #[cfg(creusot)]
                    {
                        prove_field_idx_in_bounds(fields, field_idx)?;
                    }
                    (
                        fields.get_child(field_idx),
                        fields.is_init(field_idx),
                        false,
                    )
                } else {
                    return Err(TrameError::FieldOutOfBounds {
                        index: field_idx,
                        count: field_count,
                    });
                }
            }
            NodeKind::Pointer {
                child, initialized, ..
            } => {
                if field_idx != 0 {
                    return Err(TrameError::FieldOutOfBounds {
                        index: field_idx,
                        count: 1,
                    });
                }
                (*child, *initialized, true)
            }
            NodeKind::Scalar { .. } => return Err(TrameError::NotAStruct),
        };

        if is_pointer_parent && !matches!(src.kind, SourceKind::Stage(_)) {
            return Err(TrameError::UnsupportedSource);
        }

        let (field_shape, dst) = if is_pointer_parent {
            let pointer = target_shape.as_pointer().ok_or(TrameError::NotAStruct)?;
            if !pointer.constructible_from_pointee() {
                return Err(TrameError::UnsupportedSource);
            }
            let pointee = pointer.pointee().ok_or(TrameError::NotAStruct)?;
            if pointee.layout().is_none() {
                return Err(TrameError::UnsupportedSource);
            }
            #[cfg(creusot)]
            {
                let layout = layout_expect(pointee.layout());
                let size = layout_size(vlayout_from_layout(layout));
                assume(snapshot! { size == pointee.size_logic() });
            }
            (pointee, target_data)
        } else {
            #[cfg(creusot)]
            assume(snapshot! { false });
            let node_ref = self.arena.get(target_idx);
            let (field_shape, dst, _size) = Self::field_ptr(node_ref, field_idx)?;
            (field_shape, dst)
        };

        if let Some(child) = child_idx {
            if matches!(src.kind, SourceKind::Imm { .. } | SourceKind::Default) {
                self.cleanup_node(child);
                if self.current_in_subtree(child) {
                    self.current = target_idx;
                }
                self.mark_field_not_started(target_idx, field_idx);
                self.clear_pointer_child(target_idx);
                child_idx = None;
                already_init = false;
            }
        }

        if already_init {
            if is_pointer_parent {
                unsafe { self.heap.drop_in_place(target_data, target_shape) };
            } else {
                unsafe { self.heap.drop_in_place(dst, field_shape) };
            }
            self.mark_field_not_started(target_idx, field_idx);
        }

        match src.kind {
            SourceKind::Imm(imm) => {
                let src_ptr = imm.ptr;
                let src_shape = imm.shape;
                if src_shape != field_shape {
                    return Err(TrameError::ShapeMismatch);
                }
                unsafe { self.heap.memcpy(dst, src_ptr, CopyDesc::value(field_shape)) };
                self.mark_field_complete(target_idx, field_idx);
                Ok(())
            }
            SourceKind::Default => {
                let ok = unsafe { self.heap.default_in_place(dst, field_shape) };
                if !ok {
                    return Err(TrameError::DefaultUnavailable);
                }
                self.mark_field_complete(target_idx, field_idx);
                Ok(())
            }
            SourceKind::Stage(_cap) => {
                if !is_pointer_parent
                    && !field_shape.is_struct()
                    && field_shape.as_pointer().is_none()
                {
                    return Err(TrameError::NotAStruct);
                }
                if let Some(child) = child_idx {
                    let state = self.arena.get(child).state;
                    if state == NodeState::Staged {
                        self.current = child;
                        return Ok(());
                    }
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

                let child_data = if is_pointer_parent {
                    unsafe { self.heap.alloc(field_shape) }
                } else {
                    dst
                };

                let child_node = Node {
                    data: child_data,
                    shape: field_shape,
                    kind: Node::<Heap<R>, Shape<R>>::kind_for_shape(field_shape),
                    state: NodeState::Staged,
                    parent: target_idx,
                    flags: if is_pointer_parent {
                        NodeFlags::empty().with_owns_allocation()
                    } else {
                        NodeFlags::empty()
                    },
                };

                let child_idx = self.arena.alloc(child_node);
                match &mut self.arena.get_mut(target_idx).kind {
                    NodeKind::Struct { fields } => {
                        #[cfg(creusot)]
                        {
                            prove_field_idx_in_bounds(fields, field_idx)?;
                        }
                        fields.set_child(field_idx, child_idx);
                    }
                    NodeKind::Pointer { child, .. } => {
                        *child = Some(child_idx);
                    }
                    _ => {}
                }
                self.current = child_idx;
                Ok(())
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
            NodeKind::Pointer { initialized, .. } => *initialized,
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

        let (node_data, node_shape, parent_idx, node_flags) = {
            let node = self.arena.get(self.current);
            (node.data, node.shape, node.parent, node.flags)
        };

        // Check current Node is complete
        if !self.node_is_complete(self.current) {
            return Err(TrameError::CurrentIncomplete);
        }

        // Check we're not at root
        if !parent_idx.is_valid() {
            return Err(TrameError::AtRoot);
        }

        let is_pointer_child = {
            let parent = self.arena.get(parent_idx);
            match parent.kind {
                NodeKind::Pointer { child, .. } => child.is_some_and(|c| c.same(self.current)),
                _ => false,
            }
        };

        if is_pointer_child {
            let parent = self.arena.get(parent_idx);
            let ok = unsafe {
                self.heap
                    .pointer_from_pointee(parent.data, parent.shape, node_data, node_shape)
            };
            if !ok {
                return Err(TrameError::UnsupportedSource);
            }

            if node_flags.owns_allocation() {
                unsafe { self.heap.dealloc_moved(node_data, node_shape) };
            }

            if let NodeKind::Pointer {
                child, initialized, ..
            } = &mut self.arena.get_mut(parent_idx).kind
            {
                *child = None;
                *initialized = true;
            }

            self.arena.free(self.current);
            self.current = parent_idx;
            return Ok(());
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

        let (node_state, node_kind, node_data, node_shape, node_flags) = {
            let node = self.arena.get(idx);
            (
                node.state,
                node.kind.clone(),
                node.data,
                node.shape,
                node.flags,
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

            match &node_kind {
                NodeKind::Struct { fields } => {
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
                NodeKind::Pointer {
                    child: Some(child_idx),
                    ..
                } => {
                    children.push(*child_idx);
                }
                NodeKind::Pointer { child: None, .. } => {}
                _ => {}
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
                NodeKind::Pointer {
                    initialized: true, ..
                } => {
                    #[cfg(creusot)]
                    if heap_can_drop::<Heap<R>, Shape<R>>(&self.heap, node_data, node_shape) {
                        unsafe { self.heap.drop_in_place(node_data, node_shape) };
                    }
                    #[cfg(not(creusot))]
                    unsafe {
                        self.heap.drop_in_place(node_data, node_shape);
                    }
                }
                _ => {}
            }
        }

        if node_flags.owns_allocation() {
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
