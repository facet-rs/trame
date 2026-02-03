# trame

This is an attempt to rewrite facet-reflect from scratch and try to formally
verify all pieces of it.

Currently we are at two rewrites, one of which was pretty featureful (and
fuzzed). The current attempt is built to be verified by
[kani](https://github.com/model-checking/kani).

There's nothing really useful here yet, I'm still getting used to the formal
verification tools. But you can look around for fun for sure!

## Verification

The `trame` implementation has a bunch of Kani proofs you can run with
`just kani` in its directory.

## Spec

Trame has a work-in-progress spec you can visualize via the `tracey web` subcommand,
see [tracey](https://github.com/bearcove/tracey)

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.
