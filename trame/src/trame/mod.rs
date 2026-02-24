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
    Op, Path, PathSegment, Source,
    node::{FieldSlot, FieldStates, Node, NodeFlags, NodeKind, NodeState},
    ops::SourceKind,
    runtime::{
        CopyDesc, IArena, IEnumType, IField, IHeap, IListType, IMapType, IPointerType, IPtr,
        IRuntime, ISetType, IShape, IStructType, IVariantType, Idx,
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
const LIST_STAGE_CHUNK_DEFAULT_CAPACITY: usize = 8;
const MAP_STAGE_CHUNK_DEFAULT_CAPACITY: usize = 8;

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

    /// Per-list-node rope staging chunks for append-based construction.
    list_staging: Vec<Option<ListStage<Ptr<R>, Shape<R>>>>,
    /// Per-map-node rope staging chunks for append-based construction.
    map_staging: Vec<Option<MapStage<Ptr<R>, Shape<R>>>>,

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
                    NodeKind::PointerSlice { .. } => true,
                    NodeKind::Struct { .. } => true,
                    NodeKind::List { .. } => true,
                    NodeKind::Map { .. } => true,
                    NodeKind::MapEntry { .. } => true,
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

#[derive(Debug, Clone, Copy)]
struct ListStageChunk<P, S> {
    base: P,
    element_shape: S,
    capacity: usize,
    len: usize,
}

#[derive(Debug, Clone)]
struct ListStage<P, S> {
    chunks: Vec<ListStageChunk<P, S>>,
}

impl<P, S> ListStage<P, S> {
    fn new() -> Self {
        Self { chunks: Vec::new() }
    }
}

fn layout_size(layout: trame_runtime::VLayout) -> usize {
    layout.size()
}

#[derive(Debug, Clone, Copy)]
struct MapStageEntry<P, S> {
    key: P,
    key_shape: S,
    value: P,
    value_shape: S,
}

#[derive(Debug, Clone, Copy)]
struct MapStageChunk<P, S> {
    key_base: P,
    key_shape: S,
    value_base: P,
    value_shape: S,
    capacity: usize,
    len: usize,
}

#[derive(Debug, Clone)]
struct MapStage<P, S> {
    chunks: Vec<MapStageChunk<P, S>>,
    entries: Vec<MapStageEntry<P, S>>,
}

impl<P, S> MapStage<P, S> {
    fn new() -> Self {
        Self {
            chunks: Vec::new(),
            entries: Vec::new(),
        }
    }
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
            list_staging: Vec::new(),
            map_staging: Vec::new(),
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

    fn list_stage_slot_mut(
        &mut self,
        list_idx: NodeIdx<R>,
    ) -> &mut Option<ListStage<Ptr<R>, Shape<R>>> {
        let slot = list_idx.raw as usize;
        if self.list_staging.len() <= slot {
            self.list_staging.resize_with(slot + 1, || None);
        }
        &mut self.list_staging[slot]
    }

    fn list_stage_entry_mut(&mut self, list_idx: NodeIdx<R>) -> &mut ListStage<Ptr<R>, Shape<R>> {
        let slot = self.list_stage_slot_mut(list_idx);
        if slot.is_none() {
            *slot = Some(ListStage::new());
        }
        slot.as_mut().expect("list staging entry inserted")
    }

    fn take_list_stage(&mut self, list_idx: NodeIdx<R>) -> Option<ListStage<Ptr<R>, Shape<R>>> {
        let slot = list_idx.raw as usize;
        if slot >= self.list_staging.len() {
            None
        } else {
            self.list_staging[slot].take()
        }
    }

    fn alloc_list_stage_slot(
        &mut self,
        list_idx: NodeIdx<R>,
        element_shape: Shape<R>,
        hint: Option<usize>,
    ) -> Result<Ptr<R>, TrameError> {
        // t[impl state.machine.container-stable-staging]
        // t[impl state.machine.container-capacity-hint]
        let Some(elem_layout) = element_shape.layout() else {
            return Err(TrameError::UnsupportedSource);
        };
        let elem_size = elem_layout.size();

        let maybe_new_capacity = {
            let stage = self.list_stage_entry_mut(list_idx);
            if let Some(last) = stage.chunks.last() {
                if last.len < last.capacity {
                    None
                } else {
                    Some(last.capacity.saturating_mul(2).max(1))
                }
            } else {
                Some(hint.unwrap_or(LIST_STAGE_CHUNK_DEFAULT_CAPACITY).max(1))
            }
        };

        if let Some(capacity) = maybe_new_capacity {
            let base = unsafe { self.heap.alloc_repeat(element_shape, capacity) };
            let stage = self.list_stage_entry_mut(list_idx);
            stage.chunks.push(ListStageChunk {
                base,
                element_shape,
                capacity,
                len: 0,
            });
        }

        let stage = self.list_stage_entry_mut(list_idx);
        let chunk = stage
            .chunks
            .last_mut()
            .expect("list stage must have at least one chunk");
        let slot_offset = elem_size
            .checked_mul(chunk.len)
            .expect("list stage slot offset overflow");
        let ptr = if elem_size == 0 {
            chunk.base
        } else {
            unsafe { chunk.base.byte_add(slot_offset) }
        };
        chunk.len += 1;
        Ok(ptr)
    }

    fn release_list_stage_chunks_uninit(&mut self, list_idx: NodeIdx<R>) {
        if let Some(stage) = self.take_list_stage(list_idx) {
            for chunk in stage.chunks {
                unsafe {
                    self.heap
                        .dealloc_repeat(chunk.base, chunk.element_shape, chunk.capacity);
                }
            }
        }
    }

    fn release_list_stage_chunks_moved(&mut self, list_idx: NodeIdx<R>) {
        if let Some(stage) = self.take_list_stage(list_idx) {
            for chunk in stage.chunks {
                unsafe {
                    self.heap
                        .dealloc_repeat_moved(chunk.base, chunk.element_shape, chunk.capacity);
                }
            }
        }
    }

    #[cfg(test)]
    fn list_stage_chunk_capacities(&self, list_idx: NodeIdx<R>) -> Vec<usize> {
        let slot = list_idx.raw as usize;
        if slot >= self.list_staging.len() {
            return Vec::new();
        }
        let Some(stage) = &self.list_staging[slot] else {
            return Vec::new();
        };
        stage.chunks.iter().map(|chunk| chunk.capacity).collect()
    }

    fn map_stage_slot_mut(
        &mut self,
        map_idx: NodeIdx<R>,
    ) -> &mut Option<MapStage<Ptr<R>, Shape<R>>> {
        let slot = map_idx.raw as usize;
        if self.map_staging.len() <= slot {
            self.map_staging.resize_with(slot + 1, || None);
        }
        &mut self.map_staging[slot]
    }

    fn map_stage_entry_mut(&mut self, map_idx: NodeIdx<R>) -> &mut MapStage<Ptr<R>, Shape<R>> {
        let slot = self.map_stage_slot_mut(map_idx);
        if slot.is_none() {
            *slot = Some(MapStage::new());
        }
        slot.as_mut().expect("map staging entry inserted")
    }

    fn take_map_stage(&mut self, map_idx: NodeIdx<R>) -> Option<MapStage<Ptr<R>, Shape<R>>> {
        let slot = map_idx.raw as usize;
        if slot >= self.map_staging.len() {
            None
        } else {
            self.map_staging[slot].take()
        }
    }

    fn map_stage_entry(
        &self,
        map_idx: NodeIdx<R>,
        entry_idx: usize,
    ) -> Option<MapStageEntry<Ptr<R>, Shape<R>>> {
        let slot = map_idx.raw as usize;
        let stage = self.map_staging.get(slot)?.as_ref()?;
        stage.entries.get(entry_idx).copied()
    }

    fn alloc_map_stage_slot(
        &mut self,
        map_idx: NodeIdx<R>,
        key_shape: Shape<R>,
        value_shape: Shape<R>,
        hint: Option<usize>,
    ) -> Result<(usize, Ptr<R>, Ptr<R>), TrameError> {
        // t[impl state.machine.container-stable-staging]
        // t[impl state.machine.container-capacity-hint]
        let Some(key_layout) = key_shape.layout() else {
            return Err(TrameError::UnsupportedSource);
        };
        let Some(value_layout) = value_shape.layout() else {
            return Err(TrameError::UnsupportedSource);
        };
        let key_size = key_layout.size();
        let value_size = value_layout.size();

        let maybe_new_capacity = {
            let stage = self.map_stage_entry_mut(map_idx);
            if let Some(last) = stage.chunks.last() {
                if last.len < last.capacity {
                    None
                } else {
                    Some(last.capacity.saturating_mul(2).max(1))
                }
            } else {
                Some(hint.unwrap_or(MAP_STAGE_CHUNK_DEFAULT_CAPACITY).max(1))
            }
        };

        if let Some(capacity) = maybe_new_capacity {
            let key_base = unsafe { self.heap.alloc_repeat(key_shape, capacity) };
            let value_base = unsafe { self.heap.alloc_repeat(value_shape, capacity) };
            let stage = self.map_stage_entry_mut(map_idx);
            stage.chunks.push(MapStageChunk {
                key_base,
                key_shape,
                value_base,
                value_shape,
                capacity,
                len: 0,
            });
        }

        let stage = self.map_stage_entry_mut(map_idx);
        let chunk = stage
            .chunks
            .last_mut()
            .expect("map stage must have at least one chunk");

        let key_offset = key_size
            .checked_mul(chunk.len)
            .expect("map key stage slot offset overflow");
        let value_offset = value_size
            .checked_mul(chunk.len)
            .expect("map value stage slot offset overflow");

        let key_ptr = if key_size == 0 {
            chunk.key_base
        } else {
            unsafe { chunk.key_base.byte_add(key_offset) }
        };
        let value_ptr = if value_size == 0 {
            chunk.value_base
        } else {
            unsafe { chunk.value_base.byte_add(value_offset) }
        };
        chunk.len += 1;

        let entry_idx = stage.entries.len();
        stage.entries.push(MapStageEntry {
            key: key_ptr,
            key_shape,
            value: value_ptr,
            value_shape,
        });

        Ok((entry_idx, key_ptr, value_ptr))
    }

    fn release_map_stage_chunks_uninit(&mut self, map_idx: NodeIdx<R>) {
        if let Some(stage) = self.take_map_stage(map_idx) {
            for chunk in stage.chunks {
                unsafe {
                    self.heap
                        .dealloc_repeat(chunk.key_base, chunk.key_shape, chunk.capacity);
                    self.heap
                        .dealloc_repeat(chunk.value_base, chunk.value_shape, chunk.capacity);
                }
            }
        }
    }

    fn release_map_stage_chunks_moved(&mut self, map_idx: NodeIdx<R>) {
        if let Some(stage) = self.take_map_stage(map_idx) {
            for chunk in stage.chunks {
                unsafe {
                    self.heap
                        .dealloc_repeat_moved(chunk.key_base, chunk.key_shape, chunk.capacity);
                    self.heap.dealloc_repeat_moved(
                        chunk.value_base,
                        chunk.value_shape,
                        chunk.capacity,
                    );
                }
            }
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
                NodeKind::Struct { .. } | NodeKind::MapEntry { .. } => Err(TrameError::NotAStruct),
                NodeKind::PointerSlice { .. } => Err(TrameError::UnsupportedSource),
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
                NodeKind::Map {
                    initialized,
                    entries,
                    closed,
                } => self.apply_set_direct_map(
                    target_idx,
                    target_shape,
                    target_data,
                    *initialized,
                    entries.clone(),
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
        self.release_list_stage_chunks_uninit(target_idx);

        if was_initialized {
            unsafe { self.heap.drop_in_place(dst, shape) };
        }
        if let NodeKind::List {
            initialized,
            elements,
            closed,
        } = &mut self.arena.get_mut(target_idx).kind
        {
            *initialized = false;
            *closed = false;
            *elements = FieldStates::new(0);
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

    #[allow(clippy::too_many_arguments)]
    fn apply_set_direct_map(
        &mut self,
        target_idx: NodeIdx<R>,
        shape: Shape<R>,
        dst: Ptr<R>,
        was_initialized: bool,
        entries: FieldStates<Node<Heap<R>, Shape<R>>>,
        _was_closed: bool,
        src: Source<Ptr<R>, Shape<R>>,
    ) -> Result<(), TrameError>
    where
        Shape<R>: IShape + PartialEq,
    {
        let entry_count = entries.len();
        for i in 0..entry_count {
            #[cfg(creusot)]
            {
                entries.prove_idx_in_bounds(i, entry_count);
            }
            if let Some(child) = entries.get_child(i) {
                self.cleanup_node(child);
                if self.current_in_subtree(child) {
                    self.current = target_idx;
                }
            }
        }
        self.release_map_stage_chunks_uninit(target_idx);

        if was_initialized {
            unsafe { self.heap.drop_in_place(dst, shape) };
        }
        if let NodeKind::Map {
            initialized,
            entries,
            closed,
        } = &mut self.arena.get_mut(target_idx).kind
        {
            *initialized = false;
            *closed = false;
            *entries = FieldStates::new(0);
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

        if let NodeKind::Map {
            initialized,
            entries,
            closed,
        } = &mut self.arena.get_mut(target_idx).kind
        {
            *initialized = true;
            *closed = true;
            *entries = FieldStates::new(0);
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
        // t[impl state.machine.container-append-stage-only]
        let (is_deferred_stage, stage_hint) = match src.kind {
            SourceKind::StageDeferred(cap) => (true, cap),
            SourceKind::Stage(cap) => (false, cap),
            _ => return Err(TrameError::UnsupportedSource),
        };

        match &target_kind {
            NodeKind::PointerSlice { elements, closed } => {
                if *closed {
                    return Err(TrameError::UnsupportedSource);
                }

                let mut updated_elements = elements.clone();
                let element_shape = target_shape
                    .sequence_element()
                    .ok_or(TrameError::UnsupportedSource)?;
                let child_data =
                    self.alloc_list_stage_slot(target_idx, element_shape, stage_hint)?;
                let child_node = Node {
                    data: child_data,
                    shape: element_shape,
                    kind: Node::<Heap<R>, Shape<R>>::kind_for_shape(element_shape),
                    state: NodeState::Staged,
                    parent: target_idx,
                    flags: NodeFlags::empty().with_deferred_root_if(is_deferred_stage),
                };
                let child_idx = self.arena.alloc(child_node);

                updated_elements.slots.push(FieldSlot::Child(child_idx));

                if let NodeKind::PointerSlice {
                    elements: stored_elements,
                    closed: stored_closed,
                } = &mut self.arena.get_mut(target_idx).kind
                {
                    *stored_elements = updated_elements;
                    *stored_closed = false;
                }

                self.current = child_idx;
                Ok(())
            }
            NodeKind::List {
                elements, closed, ..
            } => {
                if *closed {
                    return Err(TrameError::UnsupportedSource);
                }

                let mut updated_elements = elements.clone();
                let element_shape = if let Some(list_type) = target_shape.as_list() {
                    list_type.element()
                } else if let Some(set_type) = target_shape.as_set() {
                    set_type.element()
                } else {
                    return Err(TrameError::UnsupportedSource);
                };
                let child_data =
                    self.alloc_list_stage_slot(target_idx, element_shape, stage_hint)?;
                let child_node = Node {
                    data: child_data,
                    shape: element_shape,
                    kind: Node::<Heap<R>, Shape<R>>::kind_for_shape(element_shape),
                    state: NodeState::Staged,
                    parent: target_idx,
                    flags: NodeFlags::empty().with_deferred_root_if(is_deferred_stage),
                };
                let child_idx = self.arena.alloc(child_node);

                updated_elements.slots.push(FieldSlot::Child(child_idx));

                if let NodeKind::List {
                    elements: stored_elements,
                    closed: stored_closed,
                    ..
                } = &mut self.arena.get_mut(target_idx).kind
                {
                    *stored_elements = updated_elements;
                    *stored_closed = false;
                }

                self.current = child_idx;
                Ok(())
            }
            NodeKind::Map {
                entries, closed, ..
            } => {
                if *closed {
                    return Err(TrameError::UnsupportedSource);
                }

                let mut updated_entries = entries.clone();
                let map_type = target_shape.as_map().ok_or(TrameError::UnsupportedSource)?;
                let key_shape = map_type.key();
                let value_shape = map_type.value();
                let (entry_idx, _key_data, _value_data) =
                    self.alloc_map_stage_slot(target_idx, key_shape, value_shape, stage_hint)?;
                let entry_node = Node {
                    data: target_data,
                    shape: target_shape,
                    kind: NodeKind::MapEntry {
                        fields: FieldStates::new(2),
                        entry_idx,
                    },
                    state: NodeState::Staged,
                    parent: target_idx,
                    flags: NodeFlags::empty().with_deferred_root_if(is_deferred_stage),
                };
                let entry_node_idx = self.arena.alloc(entry_node);

                updated_entries.slots.push(FieldSlot::Child(entry_node_idx));

                if let NodeKind::Map {
                    entries: stored_entries,
                    closed: stored_closed,
                    ..
                } = &mut self.arena.get_mut(target_idx).kind
                {
                    *stored_entries = updated_entries;
                    *stored_closed = false;
                }

                self.current = entry_node_idx;
                Ok(())
            }
            _ => Err(TrameError::UnsupportedPath {
                segment: PathSegment::Append,
            }),
        }
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
            NodeKind::MapEntry { fields, entry_idx } => {
                return self.apply_set_map_entry_field(
                    target_idx,
                    target_shape,
                    *entry_idx,
                    fields.clone(),
                    field_idx,
                    src,
                );
            }
            _ => {}
        }

        let (mut child_idx, mut already_init, is_pointer_parent, is_list_parent, is_map_parent) =
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
                    (*child, *initialized, true, false, false)
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
                        false,
                    )
                }
                NodeKind::PointerSlice { elements, closed } => {
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
                        false,
                    )
                }
                NodeKind::Map {
                    entries,
                    initialized: _,
                    closed,
                } => {
                    let entry_count = entries.len();
                    if field_idx >= entry_count {
                        return Err(TrameError::FieldOutOfBounds {
                            index: field_idx,
                            count: entry_count,
                        });
                    }
                    if *closed {
                        return Err(TrameError::UnsupportedSource);
                    }
                    (
                        entries.get_child(field_idx),
                        entries.is_init(field_idx),
                        false,
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
                NodeKind::Enum { .. }
                | NodeKind::EnumVariant { .. }
                | NodeKind::MapEntry { .. } => {
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
        if is_map_parent
            && !matches!(
                &src.kind,
                SourceKind::Stage(_) | SourceKind::StageDeferred(_)
            )
        {
            return Err(TrameError::UnsupportedSource);
        }

        let mut pointer_slice_builder_parent = false;
        let (field_shape, dst) = if is_pointer_parent {
            let pointer = target_shape.as_pointer().ok_or(TrameError::NotAStruct)?;
            if !pointer.constructible_from_pointee() {
                return Err(TrameError::UnsupportedSource);
            }
            let pointee = pointer.pointee().ok_or(TrameError::NotAStruct)?;
            if pointee.layout().is_none() {
                if !pointer.supports_slice_builder() {
                    return Err(TrameError::UnsupportedSource);
                }
                if pointee.sequence_element().is_none() {
                    return Err(TrameError::UnsupportedSource);
                }
                pointer_slice_builder_parent = true;
            } else {
                #[cfg(creusot)]
                {
                    let layout = layout_expect(pointee.layout());
                    let size = layout_size(vlayout_from_layout(layout));
                    assume(snapshot! { size == pointee.size_logic() });
                }
            }
            (pointee, target_data)
        } else if is_list_parent {
            let elem_shape = if let Some(elem) = target_shape.sequence_element() {
                elem
            } else if let Some(set) = target_shape.as_set() {
                set.element()
            } else {
                return Err(TrameError::NotAStruct);
            };
            (elem_shape, target_data)
        } else if is_map_parent {
            if child_idx.is_none() {
                return Err(TrameError::UnsupportedSource);
            }
            (target_shape, target_data)
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

        if already_init && !is_map_parent {
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
            SourceKind::Stage(cap) => self.apply_set_stage_field(
                target_idx,
                field_idx,
                field_shape,
                dst,
                target_shape,
                is_pointer_parent,
                pointer_slice_builder_parent,
                is_list_parent,
                is_map_parent,
                child_idx,
                cap,
                false,
            ),
            SourceKind::StageDeferred(cap) => self.apply_set_stage_field(
                target_idx,
                field_idx,
                field_shape,
                dst,
                target_shape,
                is_pointer_parent,
                pointer_slice_builder_parent,
                is_list_parent,
                is_map_parent,
                child_idx,
                cap,
                true,
            ),
        }
    }

    fn apply_set_map_entry_field(
        &mut self,
        target_idx: NodeIdx<R>,
        target_shape: Shape<R>,
        entry_idx: usize,
        fields: FieldStates<Node<Heap<R>, Shape<R>>>,
        field_idx: usize,
        src: Source<Ptr<R>, Shape<R>>,
    ) -> Result<(), TrameError>
    where
        Shape<R>: IShape + PartialEq,
    {
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

        let map_idx = self.arena.get(target_idx).parent;
        let staged_entry = self
            .map_stage_entry(map_idx, entry_idx)
            .ok_or(TrameError::UnsupportedSource)?;
        let map_type = target_shape.as_map().ok_or(TrameError::NotAStruct)?;
        let (field_shape, dst) = if field_idx == 0 {
            (map_type.key(), staged_entry.key)
        } else {
            (map_type.value(), staged_entry.value)
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
                child_idx = None;
                already_init = false;
            }
        }

        if already_init {
            unsafe { self.heap.drop_in_place(dst, field_shape) };
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
            SourceKind::Stage(cap) => self.apply_set_stage_field(
                target_idx,
                field_idx,
                field_shape,
                dst,
                target_shape,
                false,
                false,
                false,
                false,
                child_idx,
                cap,
                false,
            ),
            SourceKind::StageDeferred(cap) => self.apply_set_stage_field(
                target_idx,
                field_idx,
                field_shape,
                dst,
                target_shape,
                false,
                false,
                false,
                false,
                child_idx,
                cap,
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
        target_shape: Shape<R>,
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
            SourceKind::Stage(cap) => self.apply_set_stage_field(
                target_idx,
                field_idx,
                field_shape,
                dst,
                target_shape,
                false,
                false,
                false,
                false,
                child_idx,
                cap,
                false,
            ),
            SourceKind::StageDeferred(cap) => self.apply_set_stage_field(
                target_idx,
                field_idx,
                field_shape,
                dst,
                target_shape,
                false,
                false,
                false,
                false,
                child_idx,
                cap,
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
        target_shape: Shape<R>,
        is_pointer_parent: bool,
        is_pointer_slice_builder_parent: bool,
        is_list_parent: bool,
        is_map_parent: bool,
        child_idx: Option<NodeIdx<R>>,
        stage_hint: Option<usize>,
        is_deferred_stage: bool,
    ) -> Result<(), TrameError> {
        if !is_pointer_parent
            && !is_pointer_slice_builder_parent
            && !is_list_parent
            && !is_map_parent
            && !field_shape.is_struct()
            && !field_shape.is_list()
            && !field_shape.is_set()
            && !field_shape.is_map()
            && field_shape.as_enum().is_none()
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

        let child_data = if is_pointer_parent {
            if is_pointer_slice_builder_parent {
                unsafe { self.heap.pointer_slice_builder_new(target_shape) }
                    .ok_or(TrameError::UnsupportedSource)?
            } else {
                unsafe { self.heap.alloc(field_shape) }
            }
        } else if is_list_parent {
            self.alloc_list_stage_slot(target_idx, field_shape, stage_hint)?
        } else if is_map_parent {
            return Err(TrameError::UnsupportedSource);
        } else {
            dst
        };

        let child_kind = if is_pointer_parent && is_pointer_slice_builder_parent {
            NodeKind::PointerSlice {
                elements: FieldStates::new(0),
                closed: false,
            }
        } else {
            Node::<Heap<R>, Shape<R>>::kind_for_shape(field_shape)
        };

        let child_node = Node {
            data: child_data,
            shape: field_shape,
            kind: child_kind,
            state: NodeState::Staged,
            parent: target_idx,
            flags: if is_pointer_parent && !is_pointer_slice_builder_parent {
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
            NodeKind::PointerSlice { elements, .. } => {
                #[cfg(creusot)]
                {
                    prove_field_idx_in_bounds(elements, field_idx)?;
                }
                elements.set_child(field_idx, child_idx);
            }
            NodeKind::Map { entries, .. } => {
                #[cfg(creusot)]
                {
                    prove_field_idx_in_bounds(entries, field_idx)?;
                }
                entries.set_child(field_idx, child_idx);
            }
            NodeKind::MapEntry { fields, .. } => {
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
        let (child_shape, child_flags, map_entry_idx, pointer_parent_shape) = {
            let child_node = self.arena.get(child_idx);
            let map_entry_idx = match child_node.kind {
                NodeKind::MapEntry { entry_idx, .. } => Some(entry_idx),
                _ => None,
            };
            let pointer_parent_shape = match child_node.kind {
                NodeKind::PointerSlice { .. } => {
                    let parent = self.arena.get(child_node.parent);
                    Some(parent.shape)
                }
                _ => None,
            };
            (
                child_node.shape,
                child_node.flags,
                map_entry_idx,
                pointer_parent_shape,
            )
        };
        let refreshed_data = if child_flags.owns_allocation() {
            Some(unsafe { self.heap.alloc(child_shape) })
        } else if let Some(pointer_shape) = pointer_parent_shape {
            Some(
                unsafe { self.heap.pointer_slice_builder_new(pointer_shape) }
                    .expect("pointer slice builder should be available for staged pointer child"),
            )
        } else {
            None
        };
        let child_node = self.arena.get_mut(child_idx);
        if let Some(data) = refreshed_data {
            child_node.data = data;
        }
        child_node.kind = if let Some(entry_idx) = map_entry_idx {
            NodeKind::MapEntry {
                fields: FieldStates::new(2),
                entry_idx,
            }
        } else if pointer_parent_shape.is_some() {
            NodeKind::PointerSlice {
                elements: FieldStates::new(0),
                closed: false,
            }
        } else {
            Node::<Heap<R>, Shape<R>>::kind_for_shape(child_node.shape)
        };
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
            NodeKind::List { elements, .. } => {
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
            NodeKind::PointerSlice { elements, .. } => {
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
            NodeKind::Map { entries, .. } => {
                let entry_count = entries.len();
                let mut i = 0;
                while i < entry_count {
                    #[cfg(creusot)]
                    {
                        entries.prove_idx_in_bounds(i, entry_count);
                    }
                    match entries.slot(i) {
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
            NodeKind::MapEntry { fields, .. } => {
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
        if matches!(self.arena.get(self.current).kind, NodeKind::List { .. }) {
            self.materialize_list_node(self.current)?;
        }
        if matches!(self.arena.get(self.current).kind, NodeKind::Map { .. }) {
            self.materialize_map_node(self.current)?;
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
        let (parent_data, parent_shape, child_data, child_shape, child_flags, child_kind) = {
            let parent = self.arena.get(parent_idx);
            let child = self.arena.get(child_idx);
            (
                parent.data,
                parent.shape,
                child.data,
                child.shape,
                child.flags,
                child.kind.clone(),
            )
        };

        match child_kind {
            NodeKind::PointerSlice { elements, .. } => {
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
                        FieldSlot::Child(element_idx) => {
                            let element_node = self.arena.get(element_idx);
                            if element_node.state != NodeState::Sealed {
                                return Err(TrameError::Incomplete);
                            }
                            let ok = unsafe {
                                self.heap.pointer_slice_builder_push(
                                    child_data,
                                    parent_shape,
                                    element_node.data,
                                    element_node.shape,
                                )
                            };
                            if !ok {
                                return Err(TrameError::UnsupportedSource);
                            }
                            self.arena.free(element_idx);
                        }
                    }
                    i += 1;
                }

                self.release_list_stage_chunks_moved(child_idx);

                let ok = unsafe {
                    self.heap.pointer_slice_builder_convert_into(
                        parent_data,
                        parent_shape,
                        child_data,
                    )
                };
                if !ok {
                    return Err(TrameError::UnsupportedSource);
                }
            }
            _ => {
                let ok = unsafe {
                    self.heap.pointer_from_pointee(
                        parent_data,
                        parent_shape,
                        child_data,
                        child_shape,
                    )
                };
                if !ok {
                    return Err(TrameError::UnsupportedSource);
                }

                if child_flags.owns_allocation() {
                    unsafe { self.heap.dealloc_moved(child_data, child_shape) };
                }
            }
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

    fn materialize_list_node(&mut self, idx: NodeIdx<R>) -> Result<(), TrameError> {
        // t[impl state.machine.container-materialize-on-close]
        // t[impl state.machine.container-no-live-partial]
        let (list_data, list_shape, was_initialized, elements) = {
            let node = self.arena.get(idx);
            let NodeKind::List {
                elements,
                initialized,
                ..
            } = &node.kind
            else {
                return Ok(());
            };
            (node.data, node.shape, *initialized, elements.clone())
        };

        if !was_initialized {
            let ok = unsafe {
                if list_shape.is_list() {
                    self.heap.list_init_in_place_with_capacity(
                        list_data,
                        list_shape,
                        elements.len(),
                    )
                } else if list_shape.is_set() {
                    self.heap
                        .set_init_in_place_with_capacity(list_data, list_shape, elements.len())
                } else {
                    false
                }
            };
            if !ok {
                return Err(TrameError::UnsupportedSource);
            }
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
                    let child_node = self.arena.get(child_idx);
                    if child_node.state != NodeState::Sealed {
                        return Err(TrameError::Incomplete);
                    }

                    let child_data = child_node.data;
                    let child_shape = child_node.shape;
                    let child_flags = child_node.flags;
                    let ok = unsafe {
                        if list_shape.is_list() {
                            self.heap.list_push_element(
                                list_data,
                                list_shape,
                                child_data,
                                child_shape,
                            )
                        } else if list_shape.is_set() {
                            // t[impl state.machine.set-uniqueness]
                            self.heap.set_insert_element(
                                list_data,
                                list_shape,
                                child_data,
                                child_shape,
                            )
                        } else {
                            false
                        }
                    };
                    if !ok {
                        return Err(TrameError::UnsupportedSource);
                    }
                    if child_flags.owns_allocation() {
                        unsafe { self.heap.dealloc_moved(child_data, child_shape) };
                    }
                    self.arena.free(child_idx);
                }
            }
            i += 1;
        }
        self.release_list_stage_chunks_moved(idx);

        if let NodeKind::List {
            initialized,
            closed,
            elements,
        } = &mut self.arena.get_mut(idx).kind
        {
            *initialized = true;
            *closed = true;
            *elements = FieldStates::new(0);
        }
        Ok(())
    }

    fn materialize_map_node(&mut self, idx: NodeIdx<R>) -> Result<(), TrameError> {
        // t[impl state.machine.container-materialize-on-close]
        // t[impl state.machine.container-no-live-partial]
        let (map_data, map_shape, was_initialized, entries) = {
            let node = self.arena.get(idx);
            let NodeKind::Map {
                entries,
                initialized,
                ..
            } = &node.kind
            else {
                return Ok(());
            };
            (node.data, node.shape, *initialized, entries.clone())
        };

        if !was_initialized {
            let ok = unsafe {
                self.heap
                    .map_init_in_place_with_capacity(map_data, map_shape, entries.len())
            };
            if !ok {
                return Err(TrameError::UnsupportedSource);
            }
        }

        // t[impl state.machine.map-last-wins]
        let entry_count = entries.len();
        let mut i = 0;
        while i < entry_count {
            #[cfg(creusot)]
            {
                entries.prove_idx_in_bounds(i, entry_count);
            }
            match entries.slot(i) {
                FieldSlot::Untracked | FieldSlot::Complete => return Err(TrameError::Incomplete),
                FieldSlot::Child(entry_node_idx) => {
                    let (
                        key_data,
                        key_shape,
                        key_child,
                        key_child_flags,
                        value_data,
                        value_shape,
                        value_child,
                        value_child_flags,
                    ) = {
                        let entry_node = self.arena.get(entry_node_idx);
                        if entry_node.state != NodeState::Sealed {
                            return Err(TrameError::Incomplete);
                        }
                        let NodeKind::MapEntry { fields, entry_idx } = &entry_node.kind else {
                            return Err(TrameError::UnsupportedSource);
                        };
                        let staged_entry = self
                            .map_stage_entry(idx, *entry_idx)
                            .ok_or(TrameError::UnsupportedSource)?;

                        let (key_data, key_shape, key_child, key_child_flags) = match fields.slot(0)
                        {
                            FieldSlot::Untracked => return Err(TrameError::Incomplete),
                            FieldSlot::Complete => {
                                (staged_entry.key, staged_entry.key_shape, None, None)
                            }
                            FieldSlot::Child(child_idx) => {
                                let child_node = self.arena.get(child_idx);
                                if child_node.state != NodeState::Sealed {
                                    return Err(TrameError::Incomplete);
                                }
                                (
                                    child_node.data,
                                    child_node.shape,
                                    Some(child_idx),
                                    Some(child_node.flags),
                                )
                            }
                        };
                        let (value_data, value_shape, value_child, value_child_flags) =
                            match fields.slot(1) {
                                FieldSlot::Untracked => return Err(TrameError::Incomplete),
                                FieldSlot::Complete => {
                                    (staged_entry.value, staged_entry.value_shape, None, None)
                                }
                                FieldSlot::Child(child_idx) => {
                                    let child_node = self.arena.get(child_idx);
                                    if child_node.state != NodeState::Sealed {
                                        return Err(TrameError::Incomplete);
                                    }
                                    (
                                        child_node.data,
                                        child_node.shape,
                                        Some(child_idx),
                                        Some(child_node.flags),
                                    )
                                }
                            };

                        (
                            key_data,
                            key_shape,
                            key_child,
                            key_child_flags,
                            value_data,
                            value_shape,
                            value_child,
                            value_child_flags,
                        )
                    };

                    let ok = unsafe {
                        self.heap.map_insert_entry(
                            map_data,
                            map_shape,
                            key_data,
                            key_shape,
                            value_data,
                            value_shape,
                        )
                    };
                    if !ok {
                        return Err(TrameError::UnsupportedSource);
                    }

                    if let Some(child_idx) = key_child {
                        if key_child_flags.is_some_and(|flags| flags.owns_allocation()) {
                            unsafe { self.heap.dealloc_moved(key_data, key_shape) };
                        }
                        self.arena.free(child_idx);
                    }
                    if let Some(child_idx) = value_child {
                        if value_child_flags.is_some_and(|flags| flags.owns_allocation()) {
                            unsafe { self.heap.dealloc_moved(value_data, value_shape) };
                        }
                        self.arena.free(child_idx);
                    }
                    self.arena.free(entry_node_idx);
                }
            }
            i += 1;
        }
        self.release_map_stage_chunks_moved(idx);

        if let NodeKind::Map {
            initialized,
            closed,
            entries,
        } = &mut self.arena.get_mut(idx).kind
        {
            *initialized = true;
            *closed = true;
            *entries = FieldStates::new(0);
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
            NodeKind::List { elements, .. } => {
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
                        }
                    }
                    i += 1;
                }
                self.materialize_list_node(idx)?;
                self.arena.get_mut(idx).state = NodeState::Sealed;
                Ok(())
            }
            NodeKind::PointerSlice { elements, .. } => {
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
                        }
                    }
                    i += 1;
                }
                self.arena.get_mut(idx).state = NodeState::Sealed;
                Ok(())
            }
            NodeKind::Map { entries, .. } => {
                let entry_count = entries.len();
                let mut i = 0;
                while i < entry_count {
                    #[cfg(creusot)]
                    {
                        entries.prove_idx_in_bounds(i, entry_count);
                    }
                    match entries.slot(i) {
                        FieldSlot::Untracked => return Err(TrameError::Incomplete),
                        FieldSlot::Complete => return Err(TrameError::Incomplete),
                        FieldSlot::Child(child_idx) => {
                            self.finalize_subtree(child_idx)?;
                            if self.arena.get(child_idx).state != NodeState::Sealed {
                                return Err(TrameError::Incomplete);
                            }
                        }
                    }
                    i += 1;
                }
                self.materialize_map_node(idx)?;
                self.arena.get_mut(idx).state = NodeState::Sealed;
                Ok(())
            }
            NodeKind::MapEntry { fields, .. } => {
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
                NodeKind::EnumVariant { .. }
                    | NodeKind::EnumPayload { .. }
                    | NodeKind::PointerSlice { .. }
                    | NodeKind::List { .. }
                    | NodeKind::Map { .. }
                    | NodeKind::MapEntry { .. }
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
                NodeKind::PointerSlice { elements, .. } => {
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
                NodeKind::Map { entries, .. } => {
                    let entry_count = entries.len();
                    let mut i = 0;
                    while i < entry_count {
                        #[cfg(creusot)]
                        {
                            entries.prove_idx_in_bounds(i, entry_count);
                        }
                        if let Some(child_idx) = entries.get_child(i) {
                            children.push(child_idx);
                        }
                        i += 1;
                    }
                }
                NodeKind::MapEntry { fields, .. } => {
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

            if matches!(
                &node_kind,
                NodeKind::List { .. } | NodeKind::PointerSlice { .. }
            ) {
                self.release_list_stage_chunks_uninit(idx);
            }
            if matches!(&node_kind, NodeKind::Map { .. }) {
                self.release_map_stage_chunks_uninit(idx);
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
                NodeKind::Map {
                    initialized: true, ..
                } => {
                    #[cfg(creusot)]
                    assume(snapshot! {self.heap.is_init(node_data, node_shape)});
                    unsafe {
                        self.heap.drop_in_place(node_data, node_shape);
                    }
                }
                NodeKind::MapEntry { fields, entry_idx } => {
                    let map_idx = self.arena.get(idx).parent;
                    if let Some(staged_entry) = self.map_stage_entry(map_idx, *entry_idx) {
                        let map_type = node_shape.as_map();
                        let mut i = 0;
                        while i < fields.len() {
                            #[cfg(creusot)]
                            {
                                fields.prove_idx_in_bounds(i, fields.len());
                            }
                            if matches!(fields.slot(i), FieldSlot::Complete) {
                                let (field_shape, field_data) = if i == 0 {
                                    (
                                        map_type.map(|m| m.key()).unwrap_or(staged_entry.key_shape),
                                        staged_entry.key,
                                    )
                                } else {
                                    (
                                        map_type
                                            .map(|m| m.value())
                                            .unwrap_or(staged_entry.value_shape),
                                        staged_entry.value,
                                    )
                                };
                                #[cfg(creusot)]
                                assume(snapshot! {self.heap.is_init(node_data, node_shape)});
                                unsafe {
                                    self.heap.drop_in_place(field_data, field_shape);
                                }
                            }
                            i += 1;
                        }
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
                NodeKind::PointerSlice { .. } => {
                    let parent_shape = self.arena.get(idx).parent;
                    if parent_shape.is_valid() {
                        let pointer_shape = self.arena.get(parent_shape).shape;
                        unsafe {
                            self.heap
                                .pointer_slice_builder_free(pointer_shape, node_data);
                        }
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

    fn parse_target_shape(
        &self,
        target_idx: NodeIdx<R>,
        target: PathTarget,
    ) -> Result<&'static facet_core::Shape, TrameError> {
        let node = self.arena.get(target_idx);
        let target_shape = node.shape;

        match target {
            PathTarget::Direct => match &node.kind {
                NodeKind::Scalar { .. }
                | NodeKind::Pointer { .. }
                | NodeKind::Enum { .. }
                | NodeKind::List { .. }
                | NodeKind::Map { .. } => Ok(target_shape),
                NodeKind::PointerSlice { .. } => Err(TrameError::UnsupportedSource),
                NodeKind::Struct { .. }
                | NodeKind::MapEntry { .. }
                | NodeKind::EnumVariant { .. }
                | NodeKind::EnumPayload { .. } => Err(TrameError::NotAStruct),
            },
            PathTarget::Field(field_idx) => match &node.kind {
                NodeKind::Struct { .. } => Ok(Self::field_meta(target_shape, field_idx)?.shape),
                NodeKind::MapEntry { .. } => {
                    let map_idx = node.parent;
                    if !map_idx.is_valid() {
                        return Err(TrameError::NotAStruct);
                    }
                    let map_shape = self.arena.get(map_idx).shape;
                    let map_type = map_shape.as_map().ok_or(TrameError::NotAStruct)?;
                    match field_idx {
                        0 => Ok(map_type.key()),
                        1 => Ok(map_type.value()),
                        _ => Err(TrameError::FieldOutOfBounds {
                            index: field_idx,
                            count: 2,
                        }),
                    }
                }
                NodeKind::EnumPayload { variant_idx, .. } => {
                    Ok(Self::enum_payload_meta(target_shape, *variant_idx, field_idx)?.shape)
                }
                NodeKind::Pointer { .. } => {
                    if field_idx != 0 {
                        return Err(TrameError::FieldOutOfBounds {
                            index: field_idx,
                            count: 1,
                        });
                    }
                    Err(TrameError::UnsupportedSource)
                }
                NodeKind::List {
                    elements, closed, ..
                }
                | NodeKind::PointerSlice { elements, closed } => {
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
                    Err(TrameError::UnsupportedSource)
                }
                NodeKind::Map {
                    entries, closed, ..
                } => {
                    let entry_count = entries.len();
                    if field_idx >= entry_count {
                        return Err(TrameError::FieldOutOfBounds {
                            index: field_idx,
                            count: entry_count,
                        });
                    }
                    if *closed {
                        return Err(TrameError::UnsupportedSource);
                    }
                    Err(TrameError::UnsupportedSource)
                }
                NodeKind::Enum { .. } | NodeKind::EnumVariant { .. } => {
                    Err(TrameError::UnsupportedSource)
                }
                NodeKind::Scalar { .. } => Err(TrameError::NotAStruct),
            },
            PathTarget::Append => match &node.kind {
                NodeKind::List { .. } | NodeKind::PointerSlice { .. } => target_shape
                    .sequence_element()
                    .ok_or(TrameError::UnsupportedSource),
                NodeKind::Map { .. } => Err(TrameError::UnsupportedSource),
                _ => Err(TrameError::UnsupportedPath {
                    segment: PathSegment::Append,
                }),
            },
        }
    }

    /// Parse from string using shape parse hooks and assign into `dst`.
    pub fn parse_from_str(&mut self, dst: Path, input: &str) -> Result<(), TrameError> {
        let (target_idx, path_target) = self.resolve_path(dst.segments())?;
        let parse_shape = self.parse_target_shape(target_idx, path_target)?;

        let temp_ptr = unsafe { self.heap.alloc(parse_shape) };
        let parsed = unsafe { parse_shape.call_parse(input, facet_core::PtrUninit::new(temp_ptr)) };

        match parsed {
            Some(Ok(())) => {
                let set_result = self.apply(Op::Set {
                    dst,
                    src: unsafe { Source::from_ptr_shape(temp_ptr, parse_shape) },
                });
                match set_result {
                    Ok(()) => {
                        unsafe { self.heap.dealloc_moved(temp_ptr, parse_shape) };
                        Ok(())
                    }
                    Err(err) => {
                        unsafe {
                            self.heap.drop_in_place(temp_ptr, parse_shape);
                            self.heap.dealloc(temp_ptr, parse_shape);
                        }
                        Err(err)
                    }
                }
            }
            Some(Err(_)) => {
                unsafe { self.heap.dealloc(temp_ptr, parse_shape) };
                Err(TrameError::ParseFromStrFailed {
                    type_name: parse_shape.type_identifier,
                })
            }
            None => {
                unsafe { self.heap.dealloc(temp_ptr, parse_shape) };
                Err(TrameError::ParseFromStrUnsupported {
                    type_name: parse_shape.type_identifier,
                })
            }
        }
    }

    /// Parse from bytes using shape parse hooks and assign into `dst`.
    pub fn parse_from_bytes(&mut self, dst: Path, bytes: &[u8]) -> Result<(), TrameError> {
        let (target_idx, path_target) = self.resolve_path(dst.segments())?;
        let parse_shape = self.parse_target_shape(target_idx, path_target)?;

        let temp_ptr = unsafe { self.heap.alloc(parse_shape) };
        let parsed =
            unsafe { parse_shape.call_parse_bytes(bytes, facet_core::PtrUninit::new(temp_ptr)) };

        match parsed {
            Some(Ok(())) => {
                let set_result = self.apply(Op::Set {
                    dst,
                    src: unsafe { Source::from_ptr_shape(temp_ptr, parse_shape) },
                });
                match set_result {
                    Ok(()) => {
                        unsafe { self.heap.dealloc_moved(temp_ptr, parse_shape) };
                        Ok(())
                    }
                    Err(err) => {
                        unsafe {
                            self.heap.drop_in_place(temp_ptr, parse_shape);
                            self.heap.dealloc(temp_ptr, parse_shape);
                        }
                        Err(err)
                    }
                }
            }
            Some(Err(_)) => {
                unsafe { self.heap.dealloc(temp_ptr, parse_shape) };
                Err(TrameError::ParseFromBytesFailed {
                    type_name: parse_shape.type_identifier,
                })
            }
            None => {
                unsafe { self.heap.dealloc(temp_ptr, parse_shape) };
                Err(TrameError::ParseFromBytesUnsupported {
                    type_name: parse_shape.type_identifier,
                })
            }
        }
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
