//! Partial value construction.
//!
//! `Trame` manages incremental construction of a value, tracking which
//! fields have been initialized and ensuring proper cleanup on failure.

mod errors;
mod heap_value;
pub use errors::TrameError;
pub use heap_value::HeapValue;

#[cfg(not(creusot))]
use crate::runtime::LiveRuntime;
use crate::{
    Op, PathSegment, Source,
    node::{FieldSlot, FieldStates, Node, NodeFlags, NodeKind, NodeState},
    ops::SourceKind,
    runtime::{
        CopyDesc, IArena, IEnumType, IField, IHeap, IListType, IPointerType, IPtr, IRuntime,
        IShape, IStructType, IVariantType, Idx,
    },
};
use core::marker::PhantomData;

#[cfg(creusot)]
use crate::runtime::IShapeExtra as _;
#[cfg(creusot)]
use creusot_std::{
    invariant::Invariant,
    macros::{ensures, logic, requires, snapshot, trusted},
    snapshot::Snapshot,
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
                        if initialized {
                            self.heap.is_init(node.data, node.shape)
                        } else {
                            self.heap.is_uninit(node.data, node.shape)
                        },
                    NodeKind::Pointer { initialized, .. } =>
                        if initialized {
                            self.heap.is_init(node.data, node.shape)
                        } else {
                            self.heap.is_uninit(node.data, node.shape)
                        },
                    NodeKind::Struct { .. } => true,
                    NodeKind::List { .. } => true,
                    NodeKind::Enum { .. } => true,
                    NodeKind::EnumVariant { .. } => true,
                    NodeKind::EnumPayload { .. } => true,
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

#[derive(Debug, Clone, Copy)]
enum PathTarget {
    Direct,
    Field(usize),
    Append,
}

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
fn assume_field_ptr_in_bounds<S: IShape>(meta: &FieldMeta<S>, node_size: usize) {}

#[cfg(creusot)]
#[trusted]
fn shape_eq<S: PartialEq>(a: S, b: S) -> bool {
    a == b
}

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

    fn enum_type(shape: Shape<R>) -> Result<<Shape<R> as IShape>::EnumType, TrameError> {
        shape.as_enum().ok_or(TrameError::NotAStruct)
    }

    fn enum_variant(
        shape: Shape<R>,
        variant_idx: usize,
    ) -> Result<<<Shape<R> as IShape>::EnumType as IEnumType>::Variant, TrameError> {
        let en = Self::enum_type(shape)?;
        let variant_count = en.variant_count();
        en.variant(variant_idx).ok_or(TrameError::FieldOutOfBounds {
            index: variant_idx,
            count: variant_count,
        })
    }

    fn enum_payload_meta(
        shape: Shape<R>,
        variant_idx: usize,
        field_idx: usize,
    ) -> Result<FieldMeta<Shape<R>>, TrameError> {
        let variant = Self::enum_variant(shape, variant_idx)?;
        let payload = variant.data();
        let field_count = payload.field_count();
        let field = payload
            .field(field_idx)
            .ok_or(TrameError::FieldOutOfBounds {
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

    fn enum_payload_ptr(
        node: &Node<Heap<R>, Shape<R>>,
        variant_idx: usize,
        field_idx: usize,
    ) -> Result<(Shape<R>, Ptr<R>, usize), TrameError> {
        let meta = Self::enum_payload_meta(node.shape, variant_idx, field_idx)?;
        let node_size = layout_size(vlayout_from_layout(layout_expect(node.shape.layout())));
        #[cfg(creusot)]
        assume_field_ptr_in_bounds(&meta, node_size);
        #[cfg(not(creusot))]
        assert!(
            meta.offset + layout_size(meta.layout) <= node_size,
            "enum payload field out of bounds: {} + {} > {}",
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

    #[cfg_attr(creusot,
        trusted,
        ensures(match result {
            Ok((node, _)) => (^self).arena.contains(node),
            Err(_) => true,
        })
    )]
    fn resolve_path(
        &mut self,
        path: &[PathSegment],
    ) -> Result<(NodeIdx<R>, PathTarget), TrameError> {
        if path.is_empty() {
            return Ok((self.current, PathTarget::Direct));
        }

        let mut idx = self.current;
        let mut segs = path;
        if let Some(PathSegment::Root) = segs.first() {
            // Move the cursor to the root by walking parent links.
            idx = self.ascend_to_root()?;
            segs = &segs[1..];
        }

        if segs.is_empty() {
            return Ok((idx, PathTarget::Direct));
        }

        if segs.len() != 1 {
            return Err(TrameError::UnsupportedPath { segment: segs[0] });
        }

        match segs[0] {
            PathSegment::Field(n) => Ok((idx, PathTarget::Field(n as usize))),
            PathSegment::Append => Ok((idx, PathTarget::Append)),
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

    #[cfg_attr(creusot,
        requires(self.arena.contains(target_idx)),
        requires(self.valid_src(src)),
        ensures(match result {
            Ok(()) => {
                let node = self.arena.get_logic(target_idx);
                match node.kind {
                    NodeKind::Scalar { .. } | NodeKind::Pointer { .. } => match src.kind {
                        SourceKind::Stage(_) | SourceKind::StageDeferred(_) => false,
                        _ => true,
                    },
                    _ => true,
                }
            },
            Err(_) => true,
        })
    )]
    fn apply_set(
        &mut self,
        target_idx: NodeIdx<R>,
        target: PathTarget,
        src: Source<Ptr<R>, Shape<R>>,
    ) -> Result<(), TrameError>
    where
        Shape<R>: IShape + PartialEq,
    {
        let (target_kind, target_shape, target_data) = {
            let node = self.arena.get(target_idx);
            (node.kind.clone(), node.shape, node.data)
        };
        self.mark_lineage_staged(target_idx);

        match target {
            PathTarget::Direct => match &target_kind {
                NodeKind::Scalar { initialized } => self.apply_set_direct_scalar(
                    target_idx,
                    target_shape,
                    target_data,
                    *initialized,
                    src,
                ),
                NodeKind::Pointer { initialized, child } => {
                    #[cfg(creusot)]
                    assume(snapshot! { false });
                    self.apply_set_direct_pointer(
                        target_idx,
                        target_shape,
                        target_data,
                        *initialized,
                        *child,
                        src,
                    )
                }
                NodeKind::Enum {
                    initialized,
                    variant_child,
                    ..
                } => self.apply_set_direct_enum(
                    target_idx,
                    target_shape,
                    target_data,
                    *initialized,
                    *variant_child,
                    src,
                ),
                NodeKind::EnumVariant { .. } | NodeKind::EnumPayload { .. } => {
                    Err(TrameError::NotAStruct)
                }
                NodeKind::Struct { .. } => Err(TrameError::NotAStruct),
                NodeKind::List {
                    initialized,
                    elements,
                    closed,
                } => self.apply_set_direct_list(
                    target_idx,
                    target_shape,
                    target_data,
                    *initialized,
                    elements.clone(),
                    *closed,
                    src,
                ),
            },
            PathTarget::Field(field_idx) => {
                #[cfg(creusot)]
                assume(snapshot! { false });
                self.apply_set_field(
                    target_idx,
                    target_kind,
                    target_shape,
                    target_data,
                    field_idx,
                    src,
                )
            }
            PathTarget::Append => {
                self.apply_set_append(target_idx, target_kind, target_shape, target_data, src)
            }
        }
    }

    #[cfg(creusot)]
    #[logic]
    fn valid_src(self, src: Source<Ptr<R>, Shape<R>>) -> bool {
        pearlite! {
            match src.kind {
                SourceKind::Imm(imm) => self.heap.is_init(imm.ptr, imm.shape) && forall<i> self.arena.contains(i) ==>
                    imm.ptr != self.arena.get_logic(i).data,
                SourceKind::Default => true,
                _ => false,
            }
        }
    }

    #[cfg_attr(creusot,
        requires(self.arena.contains(target_idx)),
        requires(self.arena.get_logic(target_idx).kind == NodeKind::Scalar { initialized: already_init }),
        requires(self.arena.get_logic(target_idx).shape == shape),
        requires(self.arena.get_logic(target_idx).data == dst),
        requires(if already_init { self.heap.is_init(dst, shape) } else { self.heap.is_uninit(dst, shape) }),
        requires(self.valid_src(src)),
        ensures(match result {
            Ok(()) => {
                let node = self.arena.get_logic(target_idx);
                {
                    match node.kind {
                        NodeKind::Scalar { .. } => match src.kind {
                            SourceKind::Stage(_) | SourceKind::StageDeferred(_) => false,
                            _ => true,
                        },
                        _ => false,
                    }
                }
            },
            Err(_) => true,
        }),
        ensures(forall<z, p, s> p != self.arena.get_logic(target_idx).data && self.heap.is(z, p, s) ==> (^self).heap.is(z, p, s)),
        ensures(forall<i> i != target_idx && self.arena.contains(i) ==> (^self).arena.contains(i) && (*self).arena.get_logic(i) == (^self).arena.get_logic(i))
    )]
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
        if already_init {
            unsafe { self.heap.drop_in_place(dst, shape) };
            self.arena.get_mut(target_idx).set_scalar_initialized(false);
        }

        match src.kind {
            SourceKind::Imm(imm) => {
                let src_ptr = imm.ptr;
                let src_shape = imm.shape;
                #[cfg(creusot)]
                let same_shape = shape_eq(src_shape, shape);
                #[cfg(not(creusot))]
                let same_shape = src_shape == shape;
                if !same_shape {
                    return Err(TrameError::ShapeMismatch);
                }
                unsafe { self.heap.memcpy(dst, src_ptr, CopyDesc::value(shape)) };
            }
            SourceKind::Default => {
                let ok = unsafe { self.heap.default_in_place(dst, shape) };
                if !ok {
                    return Err(TrameError::DefaultUnavailable);
                }
            }
            SourceKind::Stage(_) | SourceKind::StageDeferred(_) => {
                return Err(TrameError::UnsupportedSource);
            }
        }

        self.arena.get_mut(target_idx).set_scalar_initialized(true);
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

        if let Some(child) = existing_child {
            self.cleanup_node(child);
            if self.current_in_subtree(child) {
                self.current = target_idx;
            }
            self.arena.get_mut(target_idx).clear_pointer_child();
        }

        if already_init {
            unsafe { self.heap.drop_in_place(dst, shape) };
            self.arena
                .get_mut(target_idx)
                .set_pointer_initialized(false);
        }

        match src.kind {
            SourceKind::Imm(imm) => {
                let src_ptr = imm.ptr;
                let src_shape = imm.shape;
                #[cfg(creusot)]
                let same_shape = shape_eq(src_shape, shape);
                #[cfg(not(creusot))]
                let same_shape = src_shape == shape;
                if !same_shape {
                    return Err(TrameError::ShapeMismatch);
                }
                #[cfg(creusot)]
                assume(snapshot! { self.heap.is_init(src_ptr, src_shape) });
                unsafe { self.heap.memcpy(dst, src_ptr, CopyDesc::value(shape)) };
            }
            SourceKind::Default => {
                let ok = unsafe { self.heap.default_in_place(dst, shape) };
                if !ok {
                    return Err(TrameError::DefaultUnavailable);
                }
            }
            SourceKind::Stage(_) | SourceKind::StageDeferred(_) => {
                return Err(TrameError::UnsupportedSource);
            }
        }

        self.arena.get_mut(target_idx).set_pointer_initialized(true);
        Ok(())
    }

    fn apply_set_direct_enum(
        &mut self,
        target_idx: NodeIdx<R>,
        shape: Shape<R>,
        dst: Ptr<R>,
        already_init: bool,
        existing_variant_child: Option<NodeIdx<R>>,
        src: Source<Ptr<R>, Shape<R>>,
    ) -> Result<(), TrameError>
    where
        Shape<R>: IShape + PartialEq,
    {
        // t[impl state.machine.overwrite]
        #[cfg(creusot)]
        assume(snapshot! { false });

        if let Some(child) = existing_variant_child {
            if !already_init {
                self.cleanup_node(child);
            }
            if self.current_in_subtree(child) {
                self.current = target_idx;
            }
        }

        if already_init {
            unsafe { self.heap.drop_in_place(dst, shape) };
        }
        if let NodeKind::Enum {
            selected_variant,
            variant_child,
            initialized,
        } = &mut self.arena.get_mut(target_idx).kind
        {
            *selected_variant = None;
            *variant_child = None;
            *initialized = false;
        }

        match src.kind {
            SourceKind::Imm(imm) => {
                let src_ptr = imm.ptr;
                let src_shape = imm.shape;
                #[cfg(creusot)]
                let same_shape = shape_eq(src_shape, shape);
                #[cfg(not(creusot))]
                let same_shape = src_shape == shape;
                if !same_shape {
                    return Err(TrameError::ShapeMismatch);
                }
                unsafe { self.heap.memcpy(dst, src_ptr, CopyDesc::value(shape)) };
            }
            SourceKind::Default => {
                let ok = unsafe { self.heap.default_in_place(dst, shape) };
                if !ok {
                    return Err(TrameError::DefaultUnavailable);
                }
            }
            SourceKind::Stage(_) | SourceKind::StageDeferred(_) => {
                return Err(TrameError::UnsupportedSource);
            }
        }

        if let NodeKind::Enum {
            selected_variant,
            variant_child,
            initialized,
        } = &mut self.arena.get_mut(target_idx).kind
        {
            *selected_variant = None;
            *variant_child = None;
            *initialized = true;
        }

        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn apply_set_direct_list(
        &mut self,
        target_idx: NodeIdx<R>,
        shape: Shape<R>,
        dst: Ptr<R>,
        was_initialized: bool,
        elements: FieldStates<Node<Heap<R>, Shape<R>>>,
        _was_closed: bool,
        src: Source<Ptr<R>, Shape<R>>,
    ) -> Result<(), TrameError>
    where
        Shape<R>: IShape + PartialEq,
    {
        let element_count = elements.len();
        for i in 0..element_count {
            #[cfg(creusot)]
            {
                elements.prove_idx_in_bounds(i, element_count);
            }
            if let Some(child) = elements.get_child(i) {
                self.cleanup_node(child);
                if self.current_in_subtree(child) {
                    self.current = target_idx;
                }
            }
        }

        if was_initialized {
            unsafe { self.heap.drop_in_place(dst, shape) };
        }

        match src.kind {
            SourceKind::Imm(imm) => {
                let src_ptr = imm.ptr;
                let src_shape = imm.shape;
                #[cfg(creusot)]
                let same_shape = shape_eq(src_shape, shape);
                #[cfg(not(creusot))]
                let same_shape = src_shape == shape;
                if !same_shape {
                    return Err(TrameError::ShapeMismatch);
                }
                unsafe { self.heap.memcpy(dst, src_ptr, CopyDesc::value(shape)) };
            }
            SourceKind::Default => {
                let ok = unsafe { self.heap.default_in_place(dst, shape) };
                if !ok {
                    return Err(TrameError::DefaultUnavailable);
                }
            }
            SourceKind::Stage(_) | SourceKind::StageDeferred(_) => {
                return Err(TrameError::UnsupportedSource);
            }
        }

        if let NodeKind::List {
            initialized,
            elements,
            closed,
        } = &mut self.arena.get_mut(target_idx).kind
        {
            *initialized = true;
            *closed = true;
            *elements = FieldStates::new(0);
        }
        Ok(())
    }

    fn apply_set_append(
        &mut self,
        target_idx: NodeIdx<R>,
        target_kind: NodeKind<Node<Heap<R>, Shape<R>>>,
        target_shape: Shape<R>,
        target_data: Ptr<R>,
        src: Source<Ptr<R>, Shape<R>>,
    ) -> Result<(), TrameError>
    where
        Shape<R>: IShape + PartialEq,
    {
        let (mut elements, mut initialized, closed) = match &target_kind {
            NodeKind::List {
                elements,
                initialized,
                closed,
            } => (elements.clone(), *initialized, *closed),
            _ => {
                return Err(TrameError::UnsupportedPath {
                    segment: PathSegment::Append,
                });
            }
        };

        let is_deferred_stage = match src.kind {
            SourceKind::StageDeferred(_) => true,
            SourceKind::Stage(_) => false,
            _ => return Err(TrameError::UnsupportedSource),
        };
        let capacity = match src.kind {
            SourceKind::Stage(cap) | SourceKind::StageDeferred(cap) => cap.unwrap_or(0),
            _ => 0,
        };

        if closed {
            return Err(TrameError::UnsupportedSource);
        }

        if !initialized {
            let ok = unsafe {
                self.heap
                    .list_init_in_place_with_capacity(target_data, target_shape, capacity)
            };
            if !ok {
                return Err(TrameError::UnsupportedSource);
            }
            initialized = true;
        }

        let list_type = target_shape
            .as_list()
            .ok_or(TrameError::UnsupportedSource)?;
        let element_shape = list_type.element();
        let child_data = unsafe { self.heap.alloc(element_shape) };
        let child_node = Node {
            data: child_data,
            shape: element_shape,
            kind: Node::<Heap<R>, Shape<R>>::kind_for_shape(element_shape),
            state: NodeState::Staged,
            parent: target_idx,
            flags: NodeFlags::empty()
                .with_owns_allocation()
                .with_deferred_root_if(is_deferred_stage),
        };
        let child_idx = self.arena.alloc(child_node);

        elements.slots.push(FieldSlot::Child(child_idx));

        if let NodeKind::List {
            elements: stored_elements,
            initialized: stored_initialized,
            closed: stored_closed,
        } = &mut self.arena.get_mut(target_idx).kind
        {
            *stored_elements = elements;
            *stored_initialized = initialized;
            *stored_closed = false;
        }

        self.current = child_idx;
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

        match &target_kind {
            NodeKind::Enum {
                selected_variant,
                variant_child,
                initialized,
            } => {
                return self.apply_set_enum_variant(
                    target_idx,
                    target_shape,
                    target_data,
                    field_idx,
                    *selected_variant,
                    *variant_child,
                    *initialized,
                    src,
                );
            }
            NodeKind::EnumVariant {
                variant_idx,
                payload_child,
                initialized,
            } => {
                return self.apply_set_enum_payload_entry(
                    target_idx,
                    target_shape,
                    target_data,
                    *variant_idx,
                    *payload_child,
                    *initialized,
                    field_idx,
                    src,
                );
            }
            NodeKind::EnumPayload {
                variant_idx,
                fields,
            } => {
                return self.apply_set_enum_payload_field(
                    target_idx,
                    target_shape,
                    target_data,
                    *variant_idx,
                    fields.clone(),
                    field_idx,
                    src,
                );
            }
            _ => {}
        }

        let (mut child_idx, mut already_init, is_pointer_parent, is_list_parent) =
            match &target_kind {
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
                    (*child, *initialized, true, false)
                }
                NodeKind::List {
                    elements,
                    initialized: _,
                    closed,
                } => {
                    let element_count = elements.len();
                    if field_idx >= element_count {
                        return Err(TrameError::FieldOutOfBounds {
                            index: field_idx,
                            count: element_count,
                        });
                    }
                    if *closed {
                        return Err(TrameError::UnsupportedSource);
                    }
                    (
                        elements.get_child(field_idx),
                        elements.is_init(field_idx),
                        false,
                        true,
                    )
                }
                NodeKind::EnumPayload { fields, .. } => {
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
                            false,
                        )
                    } else {
                        return Err(TrameError::FieldOutOfBounds {
                            index: field_idx,
                            count: field_count,
                        });
                    }
                }
                NodeKind::Scalar { .. } => return Err(TrameError::NotAStruct),
                NodeKind::Enum { .. } | NodeKind::EnumVariant { .. } => {
                    return Err(TrameError::NotAStruct);
                }
            };

        if is_pointer_parent
            && !matches!(
                &src.kind,
                SourceKind::Stage(_) | SourceKind::StageDeferred(_)
            )
        {
            return Err(TrameError::UnsupportedSource);
        }
        if is_list_parent
            && !matches!(
                &src.kind,
                SourceKind::Stage(_) | SourceKind::StageDeferred(_)
            )
        {
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
        } else if is_list_parent {
            let list = target_shape.as_list().ok_or(TrameError::NotAStruct)?;
            (list.element(), target_data)
        } else {
            #[cfg(creusot)]
            assume(snapshot! { false });
            let node_ref = self.arena.get(target_idx);
            let (field_shape, dst, _size) = Self::field_ptr(node_ref, field_idx)?;
            (field_shape, dst)
        };

        if let Some(child) = child_idx {
            if matches!(&src.kind, SourceKind::Imm { .. } | SourceKind::Default) {
                self.cleanup_node(child);
                if self.current_in_subtree(child) {
                    self.current = target_idx;
                }
                self.arena
                    .get_mut(target_idx)
                    .mark_field_not_started(field_idx);
                self.arena.get_mut(target_idx).clear_pointer_child();
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
            self.arena
                .get_mut(target_idx)
                .mark_field_not_started(field_idx);
        }

        match src.kind {
            SourceKind::Imm(imm) => {
                let src_ptr = imm.ptr;
                let src_shape = imm.shape;
                #[cfg(creusot)]
                let same_shape = shape_eq(src_shape, field_shape);
                #[cfg(not(creusot))]
                let same_shape = src_shape == field_shape;
                if !same_shape {
                    return Err(TrameError::ShapeMismatch);
                }
                unsafe { self.heap.memcpy(dst, src_ptr, CopyDesc::value(field_shape)) };
                self.arena
                    .get_mut(target_idx)
                    .mark_field_complete(field_idx);
                Ok(())
            }
            SourceKind::Default => {
                let ok = unsafe { self.heap.default_in_place(dst, field_shape) };
                if !ok {
                    return Err(TrameError::DefaultUnavailable);
                }
                self.arena
                    .get_mut(target_idx)
                    .mark_field_complete(field_idx);
                Ok(())
            }
            SourceKind::Stage(_cap) => self.apply_set_stage_field(
                target_idx,
                field_idx,
                field_shape,
                dst,
                is_pointer_parent,
                is_list_parent,
                child_idx,
                false,
            ),
            SourceKind::StageDeferred(_cap) => self.apply_set_stage_field(
                target_idx,
                field_idx,
                field_shape,
                dst,
                is_pointer_parent,
                is_list_parent,
                child_idx,
                true,
            ),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn apply_set_enum_variant(
        &mut self,
        target_idx: NodeIdx<R>,
        target_shape: Shape<R>,
        target_data: Ptr<R>,
        field_idx: usize,
        selected_variant: Option<usize>,
        variant_child: Option<NodeIdx<R>>,
        already_init: bool,
        src: Source<Ptr<R>, Shape<R>>,
    ) -> Result<(), TrameError>
    where
        Shape<R>: IShape + PartialEq,
    {
        // t[impl state.machine.enum-select-writes-discriminant]
        // t[impl state.machine.enum-switch-drops-previous]
        // t[impl state.machine.enum-direct-then-switch]
        let enum_type = Self::enum_type(target_shape)?;
        let variant_count = enum_type.variant_count();
        if field_idx >= variant_count {
            return Err(TrameError::FieldOutOfBounds {
                index: field_idx,
                count: variant_count,
            });
        }

        let is_deferred_stage = matches!(src.kind, SourceKind::StageDeferred(_));
        if !matches!(
            src.kind,
            SourceKind::Stage(_) | SourceKind::StageDeferred(_)
        ) {
            return Err(TrameError::UnsupportedSource);
        }

        let variant = enum_type
            .variant(field_idx)
            .expect("validated enum variant index");
        let is_unit_variant = variant.data().field_count() == 0;
        let same_variant = selected_variant == Some(field_idx);

        if !same_variant {
            if let Some(child) = variant_child {
                self.cleanup_node(child);
                if self.current_in_subtree(child) {
                    self.current = target_idx;
                }
            }
            if already_init && variant_child.is_none() {
                unsafe { self.heap.drop_in_place(target_data, target_shape) };
            }
            if let NodeKind::Enum {
                selected_variant,
                variant_child,
                initialized,
            } = &mut self.arena.get_mut(target_idx).kind
            {
                *selected_variant = None;
                *variant_child = None;
                *initialized = false;
            }
        }

        let selected_ok = unsafe {
            self.heap
                .select_enum_variant(target_data, target_shape, field_idx)
        };
        if !selected_ok {
            return Err(TrameError::UnsupportedSource);
        }

        let existing_variant_child = match &self.arena.get(target_idx).kind {
            NodeKind::Enum { variant_child, .. } => *variant_child,
            _ => None,
        };

        if let Some(child) = existing_variant_child {
            if is_deferred_stage {
                let flags = self.arena.get(child).flags.with_deferred_root();
                self.arena.get_mut(child).flags = flags;
            }
            if let NodeKind::Enum {
                selected_variant,
                initialized,
                ..
            } = &mut self.arena.get_mut(target_idx).kind
            {
                *selected_variant = Some(field_idx);
                *initialized = is_unit_variant;
            }
            self.current = child;
            return Ok(());
        }

        let variant_node = Node {
            data: target_data,
            shape: target_shape,
            kind: NodeKind::EnumVariant {
                variant_idx: field_idx,
                payload_child: None,
                initialized: is_unit_variant,
            },
            state: NodeState::Staged,
            parent: target_idx,
            flags: NodeFlags::empty().with_deferred_root_if(is_deferred_stage),
        };
        let variant_child = self.arena.alloc(variant_node);
        if let NodeKind::Enum {
            selected_variant,
            variant_child: stored_child,
            initialized,
        } = &mut self.arena.get_mut(target_idx).kind
        {
            *selected_variant = Some(field_idx);
            *stored_child = Some(variant_child);
            *initialized = is_unit_variant;
        }
        self.current = variant_child;
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn apply_set_enum_payload_entry(
        &mut self,
        target_idx: NodeIdx<R>,
        target_shape: Shape<R>,
        target_data: Ptr<R>,
        variant_idx: usize,
        payload_child: Option<NodeIdx<R>>,
        _variant_initialized: bool,
        field_idx: usize,
        src: Source<Ptr<R>, Shape<R>>,
    ) -> Result<(), TrameError>
    where
        Shape<R>: IShape + PartialEq,
    {
        if !matches!(
            src.kind,
            SourceKind::Stage(_) | SourceKind::StageDeferred(_)
        ) {
            return Err(TrameError::UnsupportedSource);
        }

        let variant = Self::enum_variant(target_shape, variant_idx)?;
        let payload = variant.data();
        let payload_field_count = payload.field_count();

        if payload_field_count == 0 {
            return Err(TrameError::FieldOutOfBounds {
                index: field_idx,
                count: 0,
            });
        }
        if field_idx != 0 {
            return Err(TrameError::FieldOutOfBounds {
                index: field_idx,
                count: 1,
            });
        }

        let is_deferred_stage = matches!(src.kind, SourceKind::StageDeferred(_));

        if let Some(child) = payload_child {
            if is_deferred_stage {
                let flags = self.arena.get(child).flags.with_deferred_root();
                self.arena.get_mut(child).flags = flags;
            }
            if let NodeKind::EnumVariant { initialized, .. } =
                &mut self.arena.get_mut(target_idx).kind
            {
                *initialized = false;
            }
            let parent_idx = self.arena.get(target_idx).parent;
            if parent_idx.is_valid() {
                if let NodeKind::Enum { initialized, .. } = &mut self.arena.get_mut(parent_idx).kind
                {
                    *initialized = false;
                }
            }
            self.current = child;
            return Ok(());
        }

        let payload_node = Node {
            data: target_data,
            shape: target_shape,
            kind: NodeKind::EnumPayload {
                variant_idx,
                fields: FieldStates::new(payload_field_count),
            },
            state: NodeState::Staged,
            parent: target_idx,
            flags: NodeFlags::empty().with_deferred_root_if(is_deferred_stage),
        };
        let payload_child = self.arena.alloc(payload_node);

        if let NodeKind::EnumVariant {
            payload_child: stored_child,
            initialized,
            ..
        } = &mut self.arena.get_mut(target_idx).kind
        {
            *stored_child = Some(payload_child);
            *initialized = false;
        }
        let parent_idx = self.arena.get(target_idx).parent;
        if parent_idx.is_valid() {
            if let NodeKind::Enum {
                selected_variant,
                initialized,
                ..
            } = &mut self.arena.get_mut(parent_idx).kind
            {
                *selected_variant = Some(variant_idx);
                *initialized = false;
            }
        }
        self.current = payload_child;
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn apply_set_enum_payload_field(
        &mut self,
        target_idx: NodeIdx<R>,
        _target_shape: Shape<R>,
        _target_data: Ptr<R>,
        variant_idx: usize,
        fields: FieldStates<Node<Heap<R>, Shape<R>>>,
        field_idx: usize,
        src: Source<Ptr<R>, Shape<R>>,
    ) -> Result<(), TrameError>
    where
        Shape<R>: IShape + PartialEq,
    {
        // t[impl state.machine.overwrite]
        let field_count = fields.len();
        if field_idx >= field_count {
            return Err(TrameError::FieldOutOfBounds {
                index: field_idx,
                count: field_count,
            });
        }
        #[cfg(creusot)]
        {
            prove_field_idx_in_bounds(&fields, field_idx)?;
        }

        let mut child_idx = fields.get_child(field_idx);
        let mut already_init = fields.is_init(field_idx);

        let node_ref = self.arena.get(target_idx);
        let (field_shape, dst, _size) = Self::enum_payload_ptr(node_ref, variant_idx, field_idx)?;

        if let Some(child) = child_idx {
            if matches!(&src.kind, SourceKind::Imm { .. } | SourceKind::Default) {
                self.cleanup_node(child);
                if self.current_in_subtree(child) {
                    self.current = target_idx;
                }
                self.arena
                    .get_mut(target_idx)
                    .mark_field_not_started(field_idx);
                self.mark_enum_lineage_uninitialized(target_idx);
                child_idx = None;
                already_init = false;
            }
        }

        if already_init {
            unsafe { self.heap.drop_in_place(dst, field_shape) };
            self.arena
                .get_mut(target_idx)
                .mark_field_not_started(field_idx);
            self.mark_enum_lineage_uninitialized(target_idx);
        }

        match src.kind {
            SourceKind::Imm(imm) => {
                let src_ptr = imm.ptr;
                let src_shape = imm.shape;
                #[cfg(creusot)]
                let same_shape = shape_eq(src_shape, field_shape);
                #[cfg(not(creusot))]
                let same_shape = src_shape == field_shape;
                if !same_shape {
                    return Err(TrameError::ShapeMismatch);
                }
                unsafe { self.heap.memcpy(dst, src_ptr, CopyDesc::value(field_shape)) };
                self.arena
                    .get_mut(target_idx)
                    .mark_field_complete(field_idx);
                Ok(())
            }
            SourceKind::Default => {
                let ok = unsafe { self.heap.default_in_place(dst, field_shape) };
                if !ok {
                    return Err(TrameError::DefaultUnavailable);
                }
                self.arena
                    .get_mut(target_idx)
                    .mark_field_complete(field_idx);
                Ok(())
            }
            SourceKind::Stage(_cap) => self.apply_set_stage_field(
                target_idx,
                field_idx,
                field_shape,
                dst,
                false,
                false,
                child_idx,
                false,
            ),
            SourceKind::StageDeferred(_cap) => self.apply_set_stage_field(
                target_idx,
                field_idx,
                field_shape,
                dst,
                false,
                false,
                child_idx,
                true,
            ),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn apply_set_stage_field(
        &mut self,
        target_idx: NodeIdx<R>,
        field_idx: usize,
        field_shape: Shape<R>,
        dst: Ptr<R>,
        is_pointer_parent: bool,
        is_list_parent: bool,
        child_idx: Option<NodeIdx<R>>,
        is_deferred_stage: bool,
    ) -> Result<(), TrameError> {
        if !is_pointer_parent
            && !is_list_parent
            && !field_shape.is_struct()
            && field_shape.as_pointer().is_none()
        {
            return Err(TrameError::NotAStruct);
        }
        if let Some(child) = child_idx {
            if is_deferred_stage {
                let flags = self.arena.get(child).flags.with_deferred_root();
                self.arena.get_mut(child).flags = flags;
            }
            self.mark_parent_incomplete_for_stage_reentry(target_idx, field_idx, is_pointer_parent);

            let state = self.arena.get(child).state;
            if state == NodeState::Staged || self.arena.get(child).flags.deferred_root() {
                self.current = child;
                return Ok(());
            }

            self.cleanup_node(child);
            self.recycle_child_for_stage_reuse(child);
            self.current = child;
            return Ok(());
        }

        let child_data = if is_pointer_parent || is_list_parent {
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
            flags: if is_pointer_parent || is_list_parent {
                NodeFlags::empty().with_owns_allocation()
            } else {
                NodeFlags::empty()
            }
            .with_deferred_root_if(is_deferred_stage),
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
            NodeKind::EnumPayload { fields, .. } => {
                #[cfg(creusot)]
                {
                    prove_field_idx_in_bounds(fields, field_idx)?;
                }
                fields.set_child(field_idx, child_idx);
            }
            NodeKind::List { elements, .. } => {
                #[cfg(creusot)]
                {
                    prove_field_idx_in_bounds(elements, field_idx)?;
                }
                elements.set_child(field_idx, child_idx);
            }
            NodeKind::Pointer { child, .. } => {
                *child = Some(child_idx);
            }
            _ => {}
        }
        self.current = child_idx;
        Ok(())
    }

    /// Stage re-entry must make all parent-level completion state incomplete.
    fn mark_parent_incomplete_for_stage_reentry(
        &mut self,
        target_idx: NodeIdx<R>,
        field_idx: usize,
        is_pointer_parent: bool,
    ) {
        if is_pointer_parent {
            self.arena
                .get_mut(target_idx)
                .mark_field_not_started(field_idx);
        }

        if matches!(
            self.arena.get(target_idx).kind,
            NodeKind::EnumPayload { .. }
        ) {
            self.mark_enum_lineage_uninitialized(target_idx);
        }
    }

    /// Reinitialize a cleaned child node so it can be staged again.
    ///
    /// Precondition: `cleanup_node(child_idx)` has already been called.
    fn recycle_child_for_stage_reuse(&mut self, child_idx: NodeIdx<R>) {
        let (child_shape, child_flags) = {
            let child_node = self.arena.get(child_idx);
            (child_node.shape, child_node.flags)
        };
        let refreshed_data = if child_flags.owns_allocation() {
            Some(unsafe { self.heap.alloc(child_shape) })
        } else {
            None
        };
        let child_node = self.arena.get_mut(child_idx);
        if let Some(data) = refreshed_data {
            child_node.data = data;
        }
        child_node.kind = Node::<Heap<R>, Shape<R>>::kind_for_shape(child_node.shape);
        child_node.state = NodeState::Staged;
        child_node.flags = child_node.flags.without_cleaned();
    }

    /// Mark `idx` and all ancestors as staged.
    fn mark_lineage_staged(&mut self, mut idx: NodeIdx<R>) {
        while idx.is_valid() {
            let parent = {
                let node = self.arena.get_mut(idx);
                node.state = NodeState::Staged;
                node.parent
            };
            idx = parent;
        }
    }

    fn mark_enum_lineage_uninitialized(&mut self, payload_idx: NodeIdx<R>) {
        let variant_idx = self.arena.get(payload_idx).parent;
        if !variant_idx.is_valid() {
            return;
        }
        if let NodeKind::EnumVariant { initialized, .. } = &mut self.arena.get_mut(variant_idx).kind
        {
            *initialized = false;
        }
        let enum_idx = self.arena.get(variant_idx).parent;
        if !enum_idx.is_valid() {
            return;
        }
        if let NodeKind::Enum { initialized, .. } = &mut self.arena.get_mut(enum_idx).kind {
            *initialized = false;
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
    fn node_in_deferred_subtree(&self, mut idx: NodeIdx<R>) -> bool {
        while idx.is_valid() {
            let node = self.arena.get(idx);
            if node.flags.deferred_root() {
                return true;
            }
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
            NodeKind::List {
                elements,
                initialized,
                ..
            } => {
                if !*initialized {
                    return false;
                }
                let element_count = elements.len();
                let mut i = 0;
                while i < element_count {
                    #[cfg(creusot)]
                    {
                        elements.prove_idx_in_bounds(i, element_count);
                    }
                    match elements.slot(i) {
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
            NodeKind::Enum {
                initialized,
                variant_child,
                ..
            } => {
                if *initialized {
                    true
                } else {
                    variant_child
                        .is_some_and(|child| self.arena.get(child).state == NodeState::Sealed)
                }
            }
            NodeKind::EnumVariant {
                initialized,
                payload_child,
                ..
            } => {
                if *initialized {
                    true
                } else {
                    payload_child
                        .is_some_and(|child| self.arena.get(child).state == NodeState::Sealed)
                }
            }
            NodeKind::EnumPayload { fields, .. } => {
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
                let (target, path_target) = self.resolve_path(dst.segments())?;
                #[cfg(creusot)]
                assume(snapshot! { false });
                self.apply_set(target, path_target, src)
            }
        }
    }

    /// End constructing the current Node and move the cursor to the parent.
    ///
    /// In strict mode, current node must be complete.
    /// In deferred mode, incomplete nodes may be ended and resumed later.
    #[cfg_attr(creusot, trusted)]
    fn end_current_node(&mut self) -> Result<(), TrameError> {
        self.check_poisoned()?;

        let parent_idx = {
            let node = self.arena.get(self.current);
            node.parent
        };

        // Check we're not at root
        if !parent_idx.is_valid() {
            return Err(TrameError::AtRoot);
        }

        let node_complete = self.node_is_complete(self.current);
        let in_deferred = self.node_in_deferred_subtree(self.current);
        if !node_complete && !in_deferred {
            return Err(TrameError::CurrentIncomplete);
        }

        if in_deferred {
            let node = self.arena.get_mut(self.current);
            node.state = if node_complete {
                NodeState::Sealed
            } else {
                NodeState::Staged
            };
            self.current = parent_idx;
            return Ok(());
        }

        let is_pointer_child = {
            let parent = self.arena.get(parent_idx);
            match parent.kind {
                NodeKind::Pointer { child, .. } => child.is_some_and(|c| c.same(self.current)),
                _ => false,
            }
        };

        if is_pointer_child {
            let child_idx = self.current;
            self.fold_pointer_child(parent_idx, child_idx)?;
            return Ok(());
        }

        if let Some(slot_idx) = self.find_list_child_slot(parent_idx, self.current) {
            let child_idx = self.current;
            self.fold_list_child(parent_idx, child_idx, slot_idx)?;
            return Ok(());
        }

        // Mark this Node as complete.
        let node = self.arena.get_mut(self.current);
        node.state = NodeState::Sealed;

        // Move cursor back to parent
        self.current = parent_idx;

        Ok(())
    }

    fn fold_pointer_child(
        &mut self,
        parent_idx: NodeIdx<R>,
        child_idx: NodeIdx<R>,
    ) -> Result<(), TrameError> {
        let (parent_data, parent_shape, child_data, child_shape, child_flags) = {
            let parent = self.arena.get(parent_idx);
            let child = self.arena.get(child_idx);
            (
                parent.data,
                parent.shape,
                child.data,
                child.shape,
                child.flags,
            )
        };

        let ok = unsafe {
            self.heap
                .pointer_from_pointee(parent_data, parent_shape, child_data, child_shape)
        };
        if !ok {
            return Err(TrameError::UnsupportedSource);
        }

        if child_flags.owns_allocation() {
            unsafe { self.heap.dealloc_moved(child_data, child_shape) };
        }

        if let NodeKind::Pointer {
            child, initialized, ..
        } = &mut self.arena.get_mut(parent_idx).kind
        {
            *child = None;
            *initialized = true;
        }

        self.arena.free(child_idx);
        if self.current.same(child_idx) {
            self.current = parent_idx;
        }
        Ok(())
    }

    #[allow(clippy::manual_find)]
    fn find_list_child_slot(&self, parent_idx: NodeIdx<R>, child_idx: NodeIdx<R>) -> Option<usize> {
        let NodeKind::List { elements, .. } = &self.arena.get(parent_idx).kind else {
            return None;
        };
        let element_count = elements.len();
        for i in 0..element_count {
            #[cfg(creusot)]
            {
                elements.prove_idx_in_bounds(i, element_count);
            }
            if elements
                .get_child(i)
                .is_some_and(|slot_child| slot_child.same(child_idx))
            {
                return Some(i);
            }
        }
        None
    }

    fn fold_list_child(
        &mut self,
        parent_idx: NodeIdx<R>,
        child_idx: NodeIdx<R>,
        slot_idx: usize,
    ) -> Result<(), TrameError> {
        let (list_data, list_shape, child_data, child_shape, child_flags) = {
            let parent = self.arena.get(parent_idx);
            let child = self.arena.get(child_idx);
            (
                parent.data,
                parent.shape,
                child.data,
                child.shape,
                child.flags,
            )
        };

        let ok = unsafe {
            self.heap
                .list_push_element(list_data, list_shape, child_data, child_shape)
        };
        if !ok {
            return Err(TrameError::UnsupportedSource);
        }

        if child_flags.owns_allocation() {
            unsafe { self.heap.dealloc_moved(child_data, child_shape) };
        }

        if let NodeKind::List { elements, .. } = &mut self.arena.get_mut(parent_idx).kind {
            #[cfg(creusot)]
            {
                prove_field_idx_in_bounds(elements, slot_idx)?;
            }
            elements.mark_complete(slot_idx);
        }

        self.arena.free(child_idx);
        if self.current.same(child_idx) {
            self.current = parent_idx;
        }
        Ok(())
    }

    fn finalize_subtree(&mut self, idx: NodeIdx<R>) -> Result<(), TrameError> {
        // t[impl state.init.byte-tracking]
        // t[impl state.machine.strict-end]
        // t[impl state.machine.deferred-end]
        let kind = self.arena.get(idx).kind.clone();
        match kind {
            NodeKind::Scalar { initialized } => {
                if !initialized {
                    return Err(TrameError::Incomplete);
                }
                self.arena.get_mut(idx).state = NodeState::Sealed;
                Ok(())
            }
            NodeKind::Struct { fields } => {
                let field_count = fields.len();
                let mut i = 0;
                while i < field_count {
                    #[cfg(creusot)]
                    {
                        fields.prove_idx_in_bounds(i, field_count);
                    }
                    match fields.slot(i) {
                        FieldSlot::Untracked => return Err(TrameError::Incomplete),
                        FieldSlot::Complete => {}
                        FieldSlot::Child(child_idx) => {
                            self.finalize_subtree(child_idx)?;
                            if self.arena.get(child_idx).state != NodeState::Sealed {
                                return Err(TrameError::Incomplete);
                            }
                        }
                    }
                    i += 1;
                }
                self.arena.get_mut(idx).state = NodeState::Sealed;
                Ok(())
            }
            NodeKind::List {
                elements,
                initialized,
                ..
            } => {
                if !initialized {
                    return Err(TrameError::Incomplete);
                }
                let element_count = elements.len();
                let mut i = 0;
                while i < element_count {
                    #[cfg(creusot)]
                    {
                        elements.prove_idx_in_bounds(i, element_count);
                    }
                    match elements.slot(i) {
                        FieldSlot::Untracked => return Err(TrameError::Incomplete),
                        FieldSlot::Complete => {}
                        FieldSlot::Child(child_idx) => {
                            self.finalize_subtree(child_idx)?;
                            if self.arena.get(child_idx).state != NodeState::Sealed {
                                return Err(TrameError::Incomplete);
                            }
                            self.fold_list_child(idx, child_idx, i)?;
                        }
                    }
                    i += 1;
                }
                self.arena.get_mut(idx).state = NodeState::Sealed;
                Ok(())
            }
            NodeKind::Pointer { child, initialized } => {
                if initialized {
                    self.arena.get_mut(idx).state = NodeState::Sealed;
                    return Ok(());
                }

                let Some(child_idx) = child else {
                    return Err(TrameError::Incomplete);
                };
                self.finalize_subtree(child_idx)?;
                if self.arena.get(child_idx).state != NodeState::Sealed {
                    return Err(TrameError::Incomplete);
                }
                self.fold_pointer_child(idx, child_idx)?;
                self.arena.get_mut(idx).state = NodeState::Sealed;
                Ok(())
            }
            NodeKind::Enum {
                variant_child,
                initialized,
                ..
            } => {
                if initialized {
                    self.arena.get_mut(idx).state = NodeState::Sealed;
                    return Ok(());
                }

                let Some(child_idx) = variant_child else {
                    return Err(TrameError::Incomplete);
                };
                self.finalize_subtree(child_idx)?;
                if self.arena.get(child_idx).state != NodeState::Sealed {
                    return Err(TrameError::Incomplete);
                }
                if let NodeKind::Enum {
                    initialized,
                    selected_variant,
                    ..
                } = &mut self.arena.get_mut(idx).kind
                {
                    if selected_variant.is_none() {
                        return Err(TrameError::Incomplete);
                    }
                    *initialized = true;
                }
                self.arena.get_mut(idx).state = NodeState::Sealed;
                Ok(())
            }
            NodeKind::EnumVariant {
                payload_child,
                initialized,
                ..
            } => {
                if initialized {
                    self.arena.get_mut(idx).state = NodeState::Sealed;
                    return Ok(());
                }

                let Some(child_idx) = payload_child else {
                    return Err(TrameError::Incomplete);
                };
                self.finalize_subtree(child_idx)?;
                if self.arena.get(child_idx).state != NodeState::Sealed {
                    return Err(TrameError::Incomplete);
                }
                if let NodeKind::EnumVariant { initialized, .. } = &mut self.arena.get_mut(idx).kind
                {
                    *initialized = true;
                }
                self.arena.get_mut(idx).state = NodeState::Sealed;
                Ok(())
            }
            NodeKind::EnumPayload { fields, .. } => {
                let field_count = fields.len();
                let mut i = 0;
                while i < field_count {
                    #[cfg(creusot)]
                    {
                        fields.prove_idx_in_bounds(i, field_count);
                    }
                    match fields.slot(i) {
                        FieldSlot::Untracked => return Err(TrameError::Incomplete),
                        FieldSlot::Complete => {}
                        FieldSlot::Child(child_idx) => {
                            self.finalize_subtree(child_idx)?;
                            if self.arena.get(child_idx).state != NodeState::Sealed {
                                return Err(TrameError::Incomplete);
                            }
                        }
                    }
                    i += 1;
                }
                self.arena.get_mut(idx).state = NodeState::Sealed;
                Ok(())
            }
        }
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
        // t[impl state.machine.build-finalization]
        let mut this = self;
        this.check_poisoned()?;
        this.current = this.root;
        this.finalize_subtree(this.root)?;

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
        // t[impl state.machine.poison-cleanup]
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
        // t[impl state.machine.cleanup-no-double-free]
        if !idx.is_valid() {
            return;
        }

        {
            let flags = self.arena.get(idx).flags;
            if flags.cleaned() {
                return;
            }
            self.arena.get_mut(idx).flags = flags.with_cleaned();
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

        if node_state == NodeState::Sealed
            && !matches!(
                node_kind,
                NodeKind::EnumVariant { .. } | NodeKind::EnumPayload { .. } | NodeKind::List { .. }
            )
        {
            #[cfg(creusot)]
            assume(snapshot! {self.heap.is_init(node_data, node_shape)});
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
                NodeKind::List { elements, .. } => {
                    let element_count = elements.len();
                    let mut i = 0;
                    while i < element_count {
                        #[cfg(creusot)]
                        {
                            elements.prove_idx_in_bounds(i, element_count);
                        }
                        if let Some(child_idx) = elements.get_child(i) {
                            children.push(child_idx);
                        }
                        i += 1;
                    }
                }
                NodeKind::Pointer {
                    child: Some(child_idx),
                    initialized: false,
                    ..
                } => {
                    children.push(*child_idx);
                }
                NodeKind::Pointer {
                    initialized: true, ..
                } => {}
                NodeKind::Pointer { child: None, .. } => {}
                NodeKind::Enum {
                    variant_child: Some(child_idx),
                    initialized: false,
                    ..
                } => {
                    children.push(*child_idx);
                }
                NodeKind::Enum {
                    initialized: true, ..
                } => {}
                NodeKind::Enum {
                    variant_child: None,
                    ..
                } => {}
                NodeKind::EnumVariant {
                    payload_child: Some(child_idx),
                    ..
                } => {
                    children.push(*child_idx);
                }
                NodeKind::EnumVariant {
                    payload_child: None,
                    ..
                } => {}
                NodeKind::EnumPayload { fields, .. } => {
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
                    assume(snapshot! {self.heap.is_init(node_data, node_shape)});
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
                                assume(snapshot! {self.heap.is_init(node_data, node_shape)});
                                unsafe {
                                    self.heap.drop_in_place(ptr, field_shape);
                                }
                            }
                        }
                        i += 1;
                    }
                }
                NodeKind::List {
                    initialized: true, ..
                } => {
                    #[cfg(creusot)]
                    assume(snapshot! {self.heap.is_init(node_data, node_shape)});
                    unsafe {
                        self.heap.drop_in_place(node_data, node_shape);
                    }
                }
                NodeKind::Pointer {
                    initialized: true, ..
                } => {
                    #[cfg(creusot)]
                    assume(snapshot! {self.heap.is_init(node_data, node_shape)});
                    unsafe {
                        self.heap.drop_in_place(node_data, node_shape);
                    }
                }
                NodeKind::Enum {
                    initialized: true, ..
                } => {
                    #[cfg(creusot)]
                    assume(snapshot! {self.heap.is_init(node_data, node_shape)});
                    unsafe {
                        self.heap.drop_in_place(node_data, node_shape);
                    }
                }
                NodeKind::EnumPayload {
                    variant_idx,
                    fields,
                } => {
                    let node = self.arena.get(idx);
                    let field_count = fields.len();
                    let mut i = 0;
                    while i < field_count {
                        #[cfg(creusot)]
                        {
                            fields.prove_idx_in_bounds(i, field_count);
                        }
                        if matches!(fields.slot(i), FieldSlot::Complete) {
                            if let Ok((field_shape, ptr, _size)) =
                                Self::enum_payload_ptr(node, *variant_idx, i)
                            {
                                #[cfg(creusot)]
                                assume(snapshot! {self.heap.is_init(node_data, node_shape)});
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
