//! Builder API for constructing operations.

use crate::ops::{Op, Path, PathSegment, Source};
use facet_core::Facet;

/// Builder for Set operations.
pub struct SetBuilder {
    path: Path,
}

impl<'facet> Op<'facet, *mut u8, &'static facet_core::Shape> {
    /// Start building a Set operation.
    pub fn set() -> SetBuilder {
        SetBuilder {
            path: Path::default(),
        }
    }
}

impl SetBuilder {
    /// Set at a single field index.
    pub fn at(mut self, index: u32) -> Self {
        self.path = self.path.then_field(index);
        self
    }

    /// Set at a path of field indices.
    pub fn at_path(mut self, indices: &[u32]) -> Self {
        for &i in indices {
            self.path = self.path.then_field(i);
        }
        self
    }

    /// Append to a collection (list, set, or map entry).
    pub fn append(mut self) -> Self {
        self.path = self.path.then_append();
        self
    }

    /// Navigate to root first.
    pub fn root(mut self) -> Self {
        self.path = Path::root();
        self
    }

    /// Add a segment to the path.
    pub fn then(mut self, seg: PathSegment) -> Self {
        self.path = self.path.then(seg);
        self
    }

    /// Complete with an immediate value.
    pub fn imm<'a, T: Facet<'a>>(
        self,
        value: &'a mut T,
    ) -> Op<'a, *mut u8, &'static facet_core::Shape> {
        Op::Set {
            dst: self.path,
            src: Source::from_ref(value),
        }
    }

    /// Complete with a default value.
    pub fn default(self) -> Op<'static, *mut u8, &'static facet_core::Shape> {
        Op::Set {
            dst: self.path,
            src: Source::default_value(),
        }
    }

    /// Complete with stage (push frame for incremental construction).
    pub fn stage(self) -> Op<'static, *mut u8, &'static facet_core::Shape> {
        Op::Set {
            dst: self.path,
            src: Source::stage(None),
        }
    }

    /// Complete with deferred stage (push frame for deferred incremental construction).
    pub fn stage_deferred(self) -> Op<'static, *mut u8, &'static facet_core::Shape> {
        Op::Set {
            dst: self.path,
            src: Source::stage_deferred(None),
        }
    }

    /// Complete with stage and capacity hint.
    pub fn stage_with_capacity(
        self,
        hint: usize,
    ) -> Op<'static, *mut u8, &'static facet_core::Shape> {
        Op::Set {
            dst: self.path,
            src: Source::stage(Some(hint)),
        }
    }

    /// Complete with deferred stage and capacity hint.
    pub fn stage_deferred_with_capacity(
        self,
        hint: usize,
    ) -> Op<'static, *mut u8, &'static facet_core::Shape> {
        Op::Set {
            dst: self.path,
            src: Source::stage_deferred(Some(hint)),
        }
    }
}
