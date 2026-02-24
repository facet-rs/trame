+++
title = "Specification Overview"
insert_anchor_links = "heading"
weight = 10
+++

This specification is split into focused sections:

- `ops.md`: Trame operation/state-machine semantics.
- `format-ir.md`: serialization IR contract (`SerProgram`) and compiler/executor
  split.
- `exec-jit.md`: execution engine and JIT semantics.
- `parse-ir.md`: deserialization-side parse/build IR semantics.

Conventions:

- Normative requirements are written as blockquote rules: `t[...]`.
- Informative text explains intent and examples.
