+++
title = "Trame Source"
insert_anchor_links = "heading"
weight = 20
+++

# Trame Source

This document specifies the human-authored `.trame` source layer used to
produce executable VM IR.

## Scope

This section defines:

- the `.trame` module surface (what authors write)
- source-level procedure and entrypoint structure
- a minimal vertical slice and its lowering contract into `vm-ir.md`

Machine execution and opcode semantics are specified in `vm-ir.md` and
`exec-jit.md`.

## Source vs Executable

`.trame` source is not executable VM IR. It is compiler input.

> t[format.trame.source-no-abi-field] `.trame` source modules MUST NOT contain
> executable ABI fields (for example `abi`).

> t[format.trame.source-no-shape-id-field] `.trame` source modules MUST NOT
> contain compiled shape ids (for example `shape-id`).

> t[format.trame.compile-emits-abi] Compilation to VM IR MUST attach ABI version
> in compiled output.

> t[format.trame.compile-emits-shape-id] Compilation to VM IR MUST attach
> concrete root `shape-id` in compiled output.

## Module Shape (S-expression)

Canonical source root:

```lisp
(trame
  (version 0)
  (procs (...))
  (entries (...)))
```

> t[format.trame.root-tag] Source modules MUST use `trame` as root tag.

> t[format.trame.root-key-order] Canonical source printer MUST emit root keys in
> order: `version`, `procs`, `entries`.

## Procedures

`procs` is a table of named structured procedures.

Each procedure body is written in source forms (for example `object`, `field`,
`unknown`, `read`, `write`) and is lowered by the compiler into VM IR CFG.

Canonical shape:

```lisp
(procs
  ((read-user
    (object
      (field "id"   (read number u32) (write (field 0)))
      (field "name" (read string)     (write (field 1)))
      (unknown skip)))))
```

> t[format.trame.procs-named] Procedures in `.trame` MUST be named by symbols
> and referenced by name from entries.

> t[format.trame.procs-structured-not-opcodes] `.trame` procedure bodies MUST be
> written in source forms, not raw VM opcode blocks.

> t[format.trame.procs-name-unique] Procedure names MUST be unique within one
> source module.

## Entrypoints

`entries` declares callable entrypoints for the module.

Each entry binds:

- a public entry name
- a target procedure name
- required runtime capabilities

Canonical shape:

```lisp
(entries
  ((read-user
    (proc read-user)
    (caps source build))))
```

`caps` values are capability labels, not directions. The v0 capability labels
are:

- `source`: source byte/code-unit input context required
- `build`: destination build/runtime context required
- `read`: destination read context required
- `sink`: output sink context required

> t[format.trame.entries-proc-ref-explicit] Every entry MUST explicitly name one
> target procedure via `(proc <name>)`.

> t[format.trame.entries-proc-ref-valid] Entry procedure references MUST resolve
> to an existing procedure in the same module.

> t[format.trame.entries-caps-explicit] Every entry MUST declare capabilities
> explicitly via `(caps ...)`.

> t[format.trame.entries-name-unique] Entry names MUST be unique within one
> source module.

## Vertical Slice v0

The v0 slice is intentionally narrow:

- object-with-string-keys traversal
- per-field scalar reads (`string`, `number u32`)
- per-field writes to destination field paths
- unknown field policy (`skip` or `reject`)

Supported source forms in v0:

- `(object <object-body...>)`
- `(field <string-name> (read string|number u32) (write (field <u32>)))`
- `(unknown skip|reject)`

> t[format.trame.v0-forms-closed] v0 compilers MUST reject unknown source forms.

> t[format.trame.v0-field-write-path-only] v0 `write` targets MUST be explicit
> destination field paths.

> t[format.trame.v0-unknown-policy-explicit] Unknown-field behavior MUST be
> explicit in source (`skip` or `reject`), not implicit compiler behavior.

## Lowering Contract to VM IR

Compilation binds:

- one `.trame` module
- one selected entry
- one concrete root shape (compile input)

Lowering output is one VM IR `Program`.

> t[format.trame.lower-entry-selected] Compilation MUST lower exactly one
> selected entry into one executable VM IR program.

> t[format.trame.lower-caps-to-kind] Lowering MUST derive VM IR `kind` from
> entry capabilities:
> `source+build -> decode`, `read+sink -> encode`.

> t[format.trame.lower-shape-validation] Field writes in source forms MUST be
> validated against the selected root shape during lowering.

> t[format.trame.lower-op-equivalence] Lowered build behavior MUST remain
> semantically equivalent to `ops.md` state-machine semantics.

## Worked v0 Example

Source:

```lisp
(trame
  (version 0)
  (procs
    ((read-user
      (object
        (field "id"   (read number u32) (write (field 0)))
        (field "name" (read string)     (write (field 1)))
        (unknown skip)))))
  (entries
    ((read-user
      (proc read-user)
      (caps source build)))))
```

Lowered VM IR (illustrative subset):

```lisp
(vmir
  (abi 1)
  (kind decode)
  (shape-id 42)
  (consts
    (strings ("id" "name"))
    (predicates ()))
  (code
    (procs (...))
    (entry-proc f0)))
```

This example shows the boundary:

- `.trame` contains author intent (`procs`, `entries`, `caps`)
- compiled VM IR contains executable details (`abi`, `shape-id`, blocks/opcodes)
