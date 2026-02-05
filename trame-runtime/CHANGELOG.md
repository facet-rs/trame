# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0](https://github.com/facet-rs/trame/releases/tag/trame-runtime-v0.1.0) - 2026-02-05

### Other

- Start verifying apply_set, Scalar branch ([#6](https://github.com/facet-rs/trame/pull/6))
- Start bringing the learnings into the actual trame crate
- add scalar range specs for drop/default
- relate scalar can_drop to range_init
- add range_init specs to memcpy
- scaffold range init predicate on heap
- Macro cleanup
- Remove arbitrary limits for C model
- More creusot proofs
- Set up intentionally-unprovable function
- Start adding creusot-specific proofs
- Set up captain, bring all crates into the workspace
- Set up captain
- Require PartialEq for IShape
- Clean Creusot artifacts and remove stub
- Add trait-level can_drop predicate
- Clean up creusot annotations
- Replace IShapeEq with PartialEq
- Add creusot ensures for equality
- Add creusot runtime scaffolding and workspace gating
- Fix kani proofs compile
- Fix use-after-drop bug in scalar overwrite, add VRuntime diagnostics
- Set up prop testing
- Extract trame-runtime
