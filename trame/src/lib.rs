//! # trame-formal
//!
//! Formally verified partial value construction.
//!
//! This crate is built from the ground up with Kani verification in mind.
//! Every operation is designed to be verifiable, with bounded state spaces
//! and explicit invariants.
//!
//! ## Design Principles
//!
//! 1. **One implementation** - Business logic is generic over heap
//! 2. **Verified heap** - Tracks state, asserts valid transitions, bounded for Kani
//! 3. **Real heap** - Performs actual memory operations (zero-cost, unbounded)

#![cfg_attr(kani, feature(stmt_expr_attributes))]
#![cfg_attr(kani, feature(proc_macro_hygiene))]

// Important rule: we do not declare all modules as pub, we will be very intentional
// about what our public interface is.
mod node;
mod ops;
mod trame;

// Re-export trame-runtime
pub use trame_runtime as runtime;

#[cfg(not(creusot))]
pub use ops::SetBuilder;
pub use ops::{Op, OpBatch, Path, PathSegment, Source};

#[cfg(not(creusot))]
pub use runtime::live::LRuntime;

#[cfg(not(creusot))]
pub use runtime::verified::{
    VRuntime, VShapeDef, VShapeHandle, VShapeStore, vshape_register, vshape_store,
    vshape_store_reset, vshape_view,
};

pub use runtime::IRuntime;
#[cfg(creusot)]
pub use runtime::creusot_rt::{CLayout, CRuntime, CShapeDef, CShapeHandle, CShapeStore};

#[cfg(not(creusot))]
pub use runtime::LiveRuntime;

pub use trame::{HeapValue, Trame, TrameError};
