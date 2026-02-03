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

pub(crate) mod runtime;

pub use ops::{Op, Path, PathSegment, Source};
pub use trame::{Trame, TrameError};
