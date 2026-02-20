# trame-runtime

Runtime traits and implementations for [trame](https://github.com/facet-rs/trame).

Defines the `IRuntime`, `IHeap`, `IShape`, `IPtr`, and `IArena` traits that abstract
over memory operations, allowing trame's core logic to be verified with multiple active backends:

- **LRuntime** (Live Runtime) - Real memory operations via `std::alloc`. Zero-cost in production.
- **VRuntime** (Verified Runtime) - Bounded state tracking for Kani model checking. Fixed-size
  arrays, fat pointers with allocation tracking, explicit byte-range initialization tracking.
- **Creusot status** - `CRuntime` is retired; proof obligations are being migrated to the live/verified runtimes.


## Sponsors

CI runs on [Depot](https://depot.dev/) runners. Thanks to Depot for the sponsorship!

## License

Licensed under either of Apache License, Version 2.0 or MIT license at your option.
