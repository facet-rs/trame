use crate::PathSegment;

#[cfg(creusot)]
use creusot_std::macros::ensures;
#[cfg(creusot)]
use creusot_std::model::DeepModel;

/// Error during trame construction.
#[cfg_attr(creusot, derive(DeepModel))]
#[derive(Debug, Clone)]
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

impl PartialEq for TrameError {
    #[cfg_attr(
        creusot,
        ensures(result == (self.deep_model() == other.deep_model()))
    )]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Poisoned, Self::Poisoned) => true,
            (
                Self::FieldOutOfBounds { index: a, count: b },
                Self::FieldOutOfBounds { index: c, count: d },
            ) => a == c && b == d,
            (Self::FieldAlreadyInit { index: a }, Self::FieldAlreadyInit { index: b }) => a == b,
            (Self::Incomplete, Self::Incomplete) => true,
            (Self::NotAStruct, Self::NotAStruct) => true,
            (Self::CurrentIncomplete, Self::CurrentIncomplete) => true,
            (Self::AtRoot, Self::AtRoot) => true,
            (Self::ShapeMismatch, Self::ShapeMismatch) => true,
            (Self::UnsupportedPath { segment: a }, Self::UnsupportedPath { segment: b }) => a == b,
            (Self::UnsupportedSource, Self::UnsupportedSource) => true,
            (Self::DefaultUnavailable, Self::DefaultUnavailable) => true,
            _ => false,
        }
    }
}

impl Eq for TrameError {}
