# trame-proptest

Property-based tests for [trame](https://github.com/facet-rs/trame).

Generates random shape stores (DAGs of scalar and struct types) and random operation
sequences, then runs them against `VRuntime` to catch logic bugs and invariant violations
that only surface with specific shape/operation combinations.

Run with `just prop` from the workspace root.


## Sponsors

CI runs on [Depot](https://depot.dev/) runners. Thanks to Depot for the sponsorship!

## License

Licensed under either of Apache License, Version 2.0 or MIT license at your option.
