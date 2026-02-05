# Verus proofs for `trame-runtime`

This folder contains standalone Verus proofs for core runtime invariants.

- `byte_range_clear_proof.rs` models the split/trim logic used by
  `ByteRangeTracker::clear_range` in
  `trame-runtime/src/verified/byte_range.rs`.
- `tree_init_recursive_proof.rs` models recursive struct-field initialization
  over a handle-based shape store.
- Main result: the computed output bytes are exactly
  `old_range \ cleared_range`, and any byte inside the cleared interval is absent.
- Additional lemmas prove:
  - no-overlap clear is identity
  - full-cover clear is empty
  - two clears compose as set-difference and commute
  - recursive all-init implies each child field is initialized

Run:

```bash
verus trame-runtime/verus/byte_range_clear_proof.rs
verus trame-runtime/verus/tree_init_recursive_proof.rs
```
