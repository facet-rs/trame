# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0](https://github.com/facet-rs/trame/compare/trame-v0.0.0...trame-v0.1.0) - 2026-02-05

### Added

- implement recursive struct construction with begin_field/end_field
- add 5 new Kani proofs for nested struct support
- implement store-based shape system for nested struct support

### Fixed

- remove unused FieldStates import

### Other

- Start verifying apply_set, Scalar branch ([#6](https://github.com/facet-rs/trame/pull/6))
- real fieldstates lemma
- prove apply_set field bounds
- replace field index helper with proofs
- finish apply_set preconditions
- wire range_init checks into trame
- Closer
- Start bringing the learnings into the actual trame crate
- Proofs about length?
- 8 unproved files huh
- Try to reason about slots length
- Macro cleanup
- Remove arbitrary limits for C model
- MAX_NODE_FIELDS is leaking...
- More creusot proofs
- Wip prove less trivial stuff
- Set up intentionally-unprovable function
- remove unused dependencies flagged by cargo-shear
- Set up captain, bring all crates into the workspace
- Set up captain
- Replace IShapeEq with PartialEq
- Add creusot runtime scaffolding and workspace gating
- Missing imports
- Add ops helpers and lifetime semantics
- Fix use-after-drop bug in scalar overwrite, add VRuntime diagnostics
- Make sure proptest and fuzzing are set up correctly
- Set up prop testing
- fuzzing works
- Rehabilitate afl harness
- Test 2 ops / 3 ops
- Remove stage_same_alloc_id proof (tested internal fields)
- Migrate depth_tracking_correct proof to new VRuntime API
- Migrate nested_drop_cleanup proof to new VRuntime API
- Migrate end_op_incomplete_inner_fails proof to new VRuntime API
- Migrate stage_reenter_root_ok proof to new VRuntime API
- Migrate stage_end_lifecycle proof to new VRuntime API
- Migrate nested_struct_field_tracking proof to new VRuntime API
- Migrate any_init_order_completes proof to new VRuntime API
- Migrate out_of_bounds_rejected proof to new VRuntime API
- Migrate incomplete_finish_fails proof to new VRuntime API
- Migrate double_init_rejected proof to new VRuntime API
- Migrate struct_all_fields_required proof to new VRuntime API
- Migrate scalar_init_complete proof to new VRuntime API
- Migrate trame tests to new VRuntime API
- More trame tests uncommented
- Extract trame-runtime
- move tests/proofs around
- Enable soteria flamegraphs
- Fix clippy lints
- unsafe markers
- warnings--
- tests pass
- real pointers are hard
- HeapValue API etc.
- Real tests
- separate out tests/proofs
- Add alloc/alloc_owned, 'facet lifetime
- Stack pointers aw yiss
- Add live test
- scalar lifecycle test passes
- Factor out error type
- Let trame build its own heap and arena.
- Rejiggle shape store to be actually static in verified
- Move byte_range inside verified
- Comment tests out
- Start adapting tests
- Oooh cargo check passes
- Model default/drop_in_place differently
- Move more things in place
- Move shape proofs & tests
- Move live/verified around
- Building up both runtimes
- More runtime stuff
- I => interface, V => verified, L => live
- Unify runtime
- Add frame state enum
- Distinguish staged and complete path
- afl setup
- Remove partial field init hooks
- Implement ops
- Add Heap/Ptr drop proofs
- Add proofs for dropping heap subshpa
- Run just trame/sote
- Matches subshape in drop_in_place
- Wip Heap/Ptr
- Remove backend
- Wip refactor to heap/byterange
- Fill in design
- Design byterange tracking
- Document sote
- All proofs pass
- Remove slow proof
- Add Partial with Kani proofs
- Add minimal partial
- Try out soteria rust (spoiler: it's faster than kani)
- Introduce arena
- More proofs re: scalar/struct lifecycle
- Add RealBackend for production memory operations
- Here goes nothing
