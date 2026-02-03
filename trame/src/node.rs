use crate::runtime::{IHeap, IShape, Idx};

/// Maximum fields tracked per node (for verification).
pub const MAX_NODE_FIELDS: usize = 8;

// ============================================================================

/// A node tracking construction of a single value.
pub(crate) struct Node<H: IHeap<S>, S: IShape> {
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

    /// Field index within parent (if applicable).
    pub(crate) field_in_parent: u32,
}

/// Completion state for a node.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NodeState {
    /// Node is currently partially-initialized ()
    Staged,

    /// Node has been finalized (complete).
    Sealed,
}

impl<H: IHeap<S>, S: IShape> Node<H, S> {
    pub(crate) fn kind_for_shape(shape: S) -> NodeKind<Self> {
        if let Some(st) = shape.as_struct() {
            NodeKind::Struct {
                fields: FieldStates::new(st.field_count()),
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
            parent: Idx::NOT_STARTED,
            field_in_parent: 0,
        }
    }
}

// ============================================================================

/// What kind of value a frame is building.
#[derive(Debug, Clone, Copy)]
pub(crate) enum NodeKind<F> {
    /// Scalar value (no internal structure).
    Scalar { initialized: bool },
    /// Struct with tracked fields.
    Struct { fields: FieldStates<F> },
}

impl<F> NodeKind<F> {}

// ============================================================================
// FieldState - tracks which fields are initialized
// ============================================================================

/// Tracks per-field state within a struct frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FieldSlot<F> {
    /// Field not touched.
    Untracked,
    /// Field fully initialized (no child frame).
    Complete,
    /// Field has a child frame (staged or complete).
    Child(Idx<F>),
}

/// Tracks initialization state of struct fields.
#[derive(Debug, Clone, Copy)]
pub(crate) struct FieldStates<F> {
    /// State of each field.
    slots: [FieldSlot<F>; MAX_NODE_FIELDS],

    /// Number of fields in the struct.
    pub(crate) count: u8,
}

impl<F> FieldStates<F> {
    /// Create field states for a struct with `count` fields.
    pub(crate) fn new(count: usize) -> Self {
        assert!(count <= MAX_NODE_FIELDS, "too many fields");
        Self {
            slots: [FieldSlot::Untracked; MAX_NODE_FIELDS],
            count: count as u8,
        }
    }

    /// Mark a field as complete (initialized).
    pub(crate) fn mark_complete(&mut self, idx: usize) {
        debug_assert!(idx < self.count as usize);
        self.slots[idx] = FieldSlot::Complete;
    }

    /// Mark a field as not started.
    pub(crate) fn mark_not_started(&mut self, idx: usize) {
        debug_assert!(idx < self.count as usize);
        self.slots[idx] = FieldSlot::Untracked;
    }

    /// Set a field's child frame index.
    pub(crate) fn set_child(&mut self, idx: usize, child: Idx<F>) {
        debug_assert!(idx < self.count as usize);
        self.slots[idx] = FieldSlot::Child(child);
    }

    /// Get a field's child frame index, if it has one.
    pub(crate) fn get_child(&self, idx: usize) -> Option<Idx<F>> {
        debug_assert!(idx < self.count as usize);
        match self.slots[idx] {
            FieldSlot::Child(child) => Some(child),
            _ => None,
        }
    }

    /// Check if a field is complete (initialized).
    pub(crate) fn is_init(&self, idx: usize) -> bool {
        debug_assert!(idx < self.count as usize);
        matches!(self.slots[idx], FieldSlot::Complete)
    }

    /// Check if a field is not started.
    pub(crate) fn is_not_started(&self, idx: usize) -> bool {
        debug_assert!(idx < self.count as usize);
        matches!(self.slots[idx], FieldSlot::Untracked)
    }

    /// Get the raw slot for a field.
    pub(crate) fn slot(&self, idx: usize) -> FieldSlot<F> {
        debug_assert!(idx < self.count as usize);
        self.slots[idx]
    }
}
