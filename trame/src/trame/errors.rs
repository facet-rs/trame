use crate::PathSegment;

/// Error during trame construction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TrameError {
    /// Tried to operate on a poisoned trame.
    Poisoned,
    /// Field index out of bounds.
    FieldOutOfBounds { index: usize, count: usize },
    /// Field already initialized.
    FieldAlreadyInit { index: usize },
    /// Not all fields initialized when trying to build.
    Incomplete,
    /// Current Node is not a struct.
    NotAStruct,
    /// Current Node is not complete (for `Op::End`).
    CurrentIncomplete,
    /// Already at root Node (can't end).
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
