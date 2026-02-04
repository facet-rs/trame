# trame

Formally verified partial value construction for [facet](https://github.com/facet-rs/facet).

This is the main crate. It provides the `Trame` state machine for incremental value
construction, the `Op` type for describing operations, and the `Path` type for
addressing fields within nested structures.

The implementation is generic over an `IRuntime` trait (defined in `trame-runtime`),
allowing the same business logic to run against both a real heap (`LRuntime`) and a
bounded verification heap (`VRuntime`).


## Sponsors

CI runs on [Depot](https://depot.dev/) runners. Thanks to Depot for the sponsorship!

## License

Licensed under either of Apache License, Version 2.0 or MIT license at your option.
