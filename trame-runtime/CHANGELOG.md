# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.0](https://github.com/facet-rs/trame/compare/trame-runtime-v0.1.0...trame-runtime-v0.2.0) - 2026-02-26

### Other

- Split spec into weighted sections and add IR/JIT drafts ([#51](https://github.com/facet-rs/trame/pull/51))
- Add first-class fixed array incremental construction ([#50](https://github.com/facet-rs/trame/pull/50))
- Support unsized smart-pointer payload staging (Arc<[T]>/Box<[T]>/Rc<[T]>) ([#48](https://github.com/facet-rs/trame/pull/48))
- Add map last-wins regressions and VRuntime/fuzz map coverage ([#44](https://github.com/facet-rs/trame/pull/44))
- Add runtime map abstraction and live map operations ([#42](https://github.com/facet-rs/trame/pull/42))
- Implement rope-backed stable staging for lists ([#37](https://github.com/facet-rs/trame/pull/37))
- implement append-stage list construction ([#35](https://github.com/facet-rs/trame/pull/35))
- add list shape metadata and remove verus traces ([#34](https://github.com/facet-rs/trame/pull/34))
- Add enum path support across trame and verified runtime ([#32](https://github.com/facet-rs/trame/pull/32))
- Implement deferred subtree semantics and generic trame-solver runtime integration ([#28](https://github.com/facet-rs/trame/pull/28))
- Retire creusot_rt and bind creusot contracts to live runtime
- Retire Verus path and add Option coverage end-to-end
- Creusot cleanup ([#26](https://github.com/facet-rs/trame/pull/26))
- Add spec for `std::alloc::Layout::size()` ([#14](https://github.com/facet-rs/trame/pull/14))
- patch up proofs via assume false ([#19](https://github.com/facet-rs/trame/pull/19))
- Generalize LRuntime over executable shapes and enable LRuntime+VShape ([#21](https://github.com/facet-rs/trame/pull/21))
- Switch memcpy API to typed copy descriptors ([#18](https://github.com/facet-rs/trame/pull/18))
- Add Box support and toy JSON Miri coverage ([#17](https://github.com/facet-rs/trame/pull/17))
