use crate::runtime::{IHeap, IShape, IStructType, Idx};

#[cfg(creusot)]
use creusot_std::logic::Int;
#[cfg(creusot)]
use creusot_std::macros::{ensures, logic, requires, trusted};
#[cfg(creusot)]
use creusot_std::model::DeepModel;

// ============================================================================

/// A node tracking construction of a single value.
pub struct Node<H: IHeap<S>, S: IShape> {
    /// Pointer to this node's data.
    pub(crate) data: H::Ptr,

    /// Shape of the value being built.
    pub(crate) shape: S,

    /// What kind of value and its init state.
    pub(crate) kind: NodeKind<Self>,

    /// State of the node
    pub(crate) state: NodeState,

    /// Parent node index (NOT_STARTED if root).
    pub(crate) parent: Idx<Self>,

    /// Node behavior flags.
    pub(crate) flags: NodeFlags,
}

impl<H: IHeap<S>, S: IShape> Node<H, S> {
    #[cfg_attr(creusot,
        requires(match self.kind {
            NodeKind::Scalar { .. } => true,
            _ => false,
        }),
        ensures((^self) == Self { kind: NodeKind::Scalar { initialized: new }, .. *self })
    )]
    pub(crate) fn set_scalar_initialized(&mut self, new: bool) {
        if let NodeKind::Scalar { initialized } = &mut self.kind {
            *initialized = new
        }
    }

    #[cfg_attr(creusot,
        requires(
            match self.kind {
                NodeKind::Pointer { .. } => true,
                _ => false,
            }
        ),
        ensures({
            match self.kind {
                NodeKind::Pointer { child, initialized: _ } =>
                    (^self) == Self { kind: NodeKind::Pointer { initialized, child }, .. *self },
                _ => false
            }
        })
    )]
    pub(crate) fn set_pointer_initialized(&mut self, initialized: bool) {
        if let NodeKind::Pointer {
            initialized: state, ..
        } = &mut self.kind
        {
            *state = initialized;
        }
    }

    #[cfg_attr(creusot,
        ensures(
            match (^self).kind {
                NodeKind::Pointer { child, .. } => child == None, // TODO: other fields are unchanged
                _ => true,
            }
        )
    )]
    pub(crate) fn clear_pointer_child(&mut self) {
        if let NodeKind::Pointer { child, .. } = &mut self.kind {
            *child = None;
        }
    }

    #[cfg_attr(creusot,
        requires(match self.kind {
            NodeKind::Struct { fields } => field_idx@ < fields.len_logic(),
            NodeKind::Pointer { .. } => true,
            _ => false,
        }),
        ensures(match (^self).kind {
            NodeKind::Struct { fields } => fields.slots@[field_idx@] == FieldSlot::Untracked, // TODO: other fields are unchanged
            NodeKind::Pointer { initialized, .. } => !initialized,
            _ => true,
        })
    )]
    pub(crate) fn mark_field_not_started(&mut self, field_idx: usize) {
        match &mut self.kind {
            NodeKind::Struct { fields } => fields.mark_not_started(field_idx),
            NodeKind::Pointer { initialized, .. } => *initialized = false,
            _ => {}
        }
    }

    #[cfg_attr(creusot,
        requires(match self.kind {
            NodeKind::Struct { fields } => field_idx@ < fields.len_logic(),
            NodeKind::Pointer { .. } => true,
            _ => false,
        }),
        ensures(match (^self).kind {
            NodeKind::Struct { fields } => fields.slots@[field_idx@] == FieldSlot::Complete, // TODO: other fields are unchanged
            NodeKind::Pointer { initialized, .. } => initialized,
            _ => true,
        })
    )]
    pub(crate) fn mark_field_complete(&mut self, field_idx: usize) {
        match &mut self.kind {
            NodeKind::Struct { fields } => fields.mark_complete(field_idx),
            NodeKind::Pointer { initialized, .. } => *initialized = true,
            _ => {}
        }
    }
}

/// Node-level bit flags.
#[derive(Debug, Clone, Copy, Eq)]
pub(crate) struct NodeFlags(pub(crate) u8);

#[cfg(creusot)]
impl DeepModel for NodeFlags {
    type DeepModelTy = NodeFlags;

    #[logic(open, inline)]
    fn deep_model(self) -> Self::DeepModelTy {
        self
    }
}

impl PartialEq for NodeFlags {
    #[cfg_attr(
        creusot,
        ensures(result == (self.deep_model() == other.deep_model()))
    )]
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl NodeFlags {
    const OWNS_ALLOCATION: u8 = 1 << 0;
    const DEFERRED_ROOT: u8 = 1 << 1;

    #[inline]
    pub(crate) const fn empty() -> Self {
        Self(0)
    }

    #[inline]
    pub(crate) const fn with_owns_allocation(mut self) -> Self {
        self.0 |= Self::OWNS_ALLOCATION;
        self
    }

    #[inline]
    pub(crate) const fn owns_allocation(self) -> bool {
        (self.0 & Self::OWNS_ALLOCATION) != 0
    }

    #[inline]
    pub(crate) const fn with_deferred_root(mut self) -> Self {
        self.0 |= Self::DEFERRED_ROOT;
        self
    }

    #[inline]
    pub(crate) const fn deferred_root(self) -> bool {
        (self.0 & Self::DEFERRED_ROOT) != 0
    }

    #[inline]
    pub(crate) const fn with_deferred_root_if(self, enabled: bool) -> Self {
        if enabled {
            self.with_deferred_root()
        } else {
            self
        }
    }
}

/// Completion state for a node.
#[derive(Debug, Clone, Copy, Eq)]
pub(crate) enum NodeState {
    /// Node is currently partially-initialized ()
    Staged,

    /// Node has been finalized (complete).
    Sealed,
}

#[cfg(creusot)]
impl DeepModel for NodeState {
    type DeepModelTy = NodeState;

    #[logic(open, inline)]
    fn deep_model(self) -> Self::DeepModelTy {
        self
    }
}

impl PartialEq for NodeState {
    #[cfg_attr(
        creusot,
        ensures(result == (self.deep_model() == other.deep_model()))
    )]
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (Self::Staged, Self::Staged) | (Self::Sealed, Self::Sealed)
        )
    }
}

impl<H: IHeap<S>, S: IShape> Node<H, S> {
    pub(crate) fn kind_for_shape(shape: S) -> NodeKind<Self> {
        if let Some(st) = shape.as_struct() {
            NodeKind::Struct {
                fields: FieldStates::new(st.field_count()),
            }
        } else if shape.as_pointer().is_some() {
            NodeKind::Pointer {
                child: None,
                initialized: false,
            }
        } else {
            NodeKind::Scalar { initialized: false }
        }
    }

    /// Create a new root frame.
    pub(crate) fn new_root(data: H::Ptr, shape: S) -> Self {
        Self {
            data,
            shape,
            kind: Self::kind_for_shape(shape),
            state: NodeState::Staged,
            parent: Idx::not_started(),
            flags: NodeFlags::empty().with_owns_allocation(),
        }
    }
}

// ============================================================================

/// What kind of value a frame is building.
#[derive(Debug)]
pub(crate) enum NodeKind<F> {
    /// Scalar value (no internal structure).
    Scalar { initialized: bool },
    /// Struct with tracked fields.
    Struct { fields: FieldStates<F> },
    /// Smart pointer (`Box<T>`/etc) with one staged pointee child.
    Pointer {
        child: Option<Idx<F>>,
        initialized: bool,
    },
}

impl<F> Clone for NodeKind<F> {
    #[cfg_attr(creusot, ensures(result == *self))]
    fn clone(&self) -> Self {
        match self {
            Self::Scalar { initialized } => Self::Scalar {
                initialized: *initialized,
            },
            Self::Struct { fields } => Self::Struct {
                fields: fields.clone(),
            },
            Self::Pointer { child, initialized } => Self::Pointer {
                child: *child,
                initialized: *initialized,
            },
        }
    }
}

// ============================================================================
// FieldState - tracks which fields are initialized
// ============================================================================

/// Tracks per-field state within a struct frame.
#[cfg_attr(not(creusot), derive(PartialEq, Eq))]
#[derive(Debug)]
pub(crate) enum FieldSlot<F> {
    /// Field not touched.
    Untracked,
    /// Field fully initialized (no child frame).
    Complete,
    /// Field has a child frame (staged or complete).
    Child(Idx<F>),
}

impl<F> Clone for FieldSlot<F> {
    #[cfg_attr(creusot, ensures(result == *self))]
    fn clone(&self) -> Self {
        *self
    }
}

impl<F> Copy for FieldSlot<F> {}

/// Tracks initialization state of struct fields.
#[derive(Debug)]
pub(crate) struct FieldStates<F> {
    /// State of each field.
    pub slots: Vec<FieldSlot<F>>,
}

impl<F> Clone for FieldStates<F> {
    #[cfg_attr(creusot,
        trusted,
        ensures(result == *self)
    )] // TODO: not quite, because of Vec
    fn clone(&self) -> Self {
        Self {
            slots: self.slots.clone(),
        }
    }
}

impl<F> FieldStates<F> {
    #[cfg_attr(creusot, ensures(result@ == self.len_logic()))]
    pub(crate) fn len(&self) -> usize {
        self.slots.len()
    }

    #[cfg(creusot)]
    #[logic(open, inline)]
    pub(crate) fn len_logic(&self) -> Int {
        pearlite! { self.slots@.len() }
    }

    #[cfg(creusot)]
    #[requires(a < b)]
    #[ensures(a@ < b@)]
    fn usize_lt_to_int(a: usize, b: usize) {}

    /// Create field states for a struct with `count` fields.
    pub(crate) fn new(count: usize) -> Self {
        #[cfg(creusot)]
        let slots = creusot_std::std::vec::vec![FieldSlot::Untracked; count];
        #[cfg(not(creusot))]
        let slots = vec![FieldSlot::Untracked; count];

        Self { slots }
    }

    /// Mark a field as complete (initialized).
    #[cfg_attr(creusot, requires(idx@ < self.len_logic()))]
    #[cfg_attr(creusot, ensures(self.len_logic() == (^self).len_logic()))]
    #[cfg_attr(creusot, ensures((^self).slots@[idx@] == FieldSlot::Complete))]
    pub(crate) fn mark_complete(&mut self, idx: usize) {
        debug_assert!(idx < self.slots.len());
        self.slots[idx] = FieldSlot::Complete;
    }

    /// Mark a field as not started.
    #[cfg_attr(creusot, requires(idx@ < self.len_logic()))]
    #[cfg_attr(creusot, ensures(self.len_logic() == (^self).len_logic()))]
    #[cfg_attr(creusot, ensures((^self).slots@[idx@] == FieldSlot::Untracked))]
    pub(crate) fn mark_not_started(&mut self, idx: usize) {
        debug_assert!(idx < self.slots.len());
        self.slots[idx] = FieldSlot::Untracked;
    }

    /// Set a field's child frame index.
    #[cfg_attr(creusot, requires(idx@ < self.len_logic()))]
    #[cfg_attr(creusot, ensures(self.len_logic() == (^self).len_logic()))]
    pub(crate) fn set_child(&mut self, idx: usize, child: Idx<F>) {
        debug_assert!(idx < self.slots.len());
        self.slots[idx] = FieldSlot::Child(child);
    }

    /// Get a field's child frame index, if it has one.
    #[cfg_attr(creusot, requires(idx@ < self.len_logic()))]
    pub(crate) fn get_child(&self, idx: usize) -> Option<Idx<F>> {
        debug_assert!(idx < self.slots.len());
        match self.slots[idx] {
            FieldSlot::Child(child) => Some(child),
            _ => None,
        }
    }

    /// Prove a usize index is within bounds as a logic int.
    #[cfg(creusot)]
    #[requires(idx < len)]
    #[requires(len@ == self.len_logic())]
    #[ensures(idx@ < self.len_logic())]
    pub(crate) fn prove_idx_in_bounds(&self, idx: usize, len: usize) {
        Self::usize_lt_to_int(idx, len);
    }

    /// Check if a field is complete (initialized).
    #[cfg_attr(creusot, requires(idx@ < self.len_logic()))]
    pub(crate) fn is_init(&self, idx: usize) -> bool {
        debug_assert!(idx < self.slots.len());
        matches!(self.slots[idx], FieldSlot::Complete)
    }

    /// Get the raw slot for a field.
    #[cfg_attr(creusot, requires(idx@ < self.len_logic()))]
    pub(crate) fn slot(&self, idx: usize) -> FieldSlot<F> {
        debug_assert!(idx < self.slots.len());
        self.slots[idx]
    }
}
