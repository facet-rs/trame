use crate::runtime::{IHeap, IShape, IStructType, Idx};

#[cfg(creusot)]
use creusot_std::model::DeepModel;

/// Maximum fields tracked per node (for verification).
pub const MAX_NODE_FIELDS: usize = 8;

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

    #[creusot_std::macros::logic(open, inline)]
    fn deep_model(self) -> Self::DeepModelTy {
        self
    }
}

impl PartialEq for NodeState {
    #[cfg_attr(
        creusot,
        creusot_std::macros::ensures(result == (self.deep_model() == other.deep_model()))
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
            let count = st.field_count();
            assert!(count <= MAX_NODE_FIELDS, "too many fields");
            NodeKind::Struct {
                fields: FieldStates::new(count),
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
}

impl<F> Clone for NodeKind<F> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<F> Copy for NodeKind<F> {}

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
    fn clone(&self) -> Self {
        *self
    }
}

impl<F> Copy for FieldSlot<F> {}

/// Tracks initialization state of struct fields.
#[derive(Debug)]
pub(crate) struct FieldStates<F> {
    /// State of each field.
    slots: [FieldSlot<F>; MAX_NODE_FIELDS],

    /// Number of fields in the struct.
    pub(crate) count: u8,
}

impl<F> Clone for FieldStates<F> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<F> Copy for FieldStates<F> {}
impl<F> FieldStates<F> {
    /// Create field states for a struct with `count` fields.
    #[cfg_attr(creusot, creusot_std::macros::requires(count <= MAX_NODE_FIELDS))]
    pub(crate) fn new(count: usize) -> Self {
        assert!(count <= MAX_NODE_FIELDS, "too many fields");
        Self {
            slots: [FieldSlot::Untracked; MAX_NODE_FIELDS],
            count: count as u8,
        }
    }

    /// Mark a field as complete (initialized).
    #[cfg_attr(creusot, creusot_std::macros::requires(idx < self.count as usize))]
    #[cfg_attr(creusot, creusot_std::macros::requires(idx < MAX_NODE_FIELDS))]
    pub(crate) fn mark_complete(&mut self, idx: usize) {
        debug_assert!(idx < self.count as usize);
        self.slots[idx] = FieldSlot::Complete;
    }

    /// Mark a field as not started.
    #[cfg_attr(creusot, creusot_std::macros::requires(idx < self.count as usize))]
    #[cfg_attr(creusot, creusot_std::macros::requires(idx < MAX_NODE_FIELDS))]
    pub(crate) fn mark_not_started(&mut self, idx: usize) {
        debug_assert!(idx < self.count as usize);
        self.slots[idx] = FieldSlot::Untracked;
    }

    /// Set a field's child frame index.
    #[cfg_attr(creusot, creusot_std::macros::requires(idx < self.count as usize))]
    #[cfg_attr(creusot, creusot_std::macros::requires(idx < MAX_NODE_FIELDS))]
    pub(crate) fn set_child(&mut self, idx: usize, child: Idx<F>) {
        debug_assert!(idx < self.count as usize);
        self.slots[idx] = FieldSlot::Child(child);
    }

    /// Get a field's child frame index, if it has one.
    #[cfg_attr(creusot, creusot_std::macros::requires(idx < self.count as usize))]
    #[cfg_attr(creusot, creusot_std::macros::requires(idx < MAX_NODE_FIELDS))]
    pub(crate) fn get_child(&self, idx: usize) -> Option<Idx<F>> {
        debug_assert!(idx < self.count as usize);
        match self.slots[idx] {
            FieldSlot::Child(child) => Some(child),
            _ => None,
        }
    }

    /// Check if a field is complete (initialized).
    #[cfg_attr(creusot, creusot_std::macros::requires(idx < self.count as usize))]
    #[cfg_attr(creusot, creusot_std::macros::requires(idx < MAX_NODE_FIELDS))]
    pub(crate) fn is_init(&self, idx: usize) -> bool {
        debug_assert!(idx < self.count as usize);
        matches!(self.slots[idx], FieldSlot::Complete)
    }

    /// Get the raw slot for a field.
    #[cfg_attr(creusot, creusot_std::macros::requires(idx < self.count as usize))]
    #[cfg_attr(creusot, creusot_std::macros::requires(idx < MAX_NODE_FIELDS))]
    pub(crate) fn slot(&self, idx: usize) -> FieldSlot<F> {
        debug_assert!(idx < self.count as usize);
        self.slots[idx]
    }
}
