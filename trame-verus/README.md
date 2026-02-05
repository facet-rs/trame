# trame-verus

Verus proof crate for [trame](https://github.com/facet-rs/trame).

Contains crate-integrated Verus proofs for runtime invariants:

- `byte_range_clear_proof` models split/trim behavior used by
  `trame-runtime/src/verified/byte_range.rs`.
- `tree_init_recursive_proof` models recursive struct-field initialization
  over handle-based shapes.

Run proofs from workspace root with:

```bash
cargo verus verify --workspace --exclude trame-fuzz --exclude trame-proptest
```


## Sponsors

CI runs on [Depot](https://depot.dev/) runners. Thanks to Depot for the sponsorship!

## License

Licensed under either of Apache License, Version 2.0 or MIT license at your option.
