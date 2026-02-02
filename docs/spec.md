# Trame Specification

This document specifies the behavior of trame, a library for partial value
construction using facet reflection.

## Overview

Trame provides safe, verified APIs for incrementally constructing values of
types that implement `facet::Facet`. The core abstraction is `Partial<T>`,
which tracks which fields have been initialized and ensures memory safety.

## Memory Slot State Machine

> r[t.slot.states]
>
> Each memory slot in a partial value can be in one of three states:
> - **Unallocated**: No memory has been reserved
> - **Allocated**: Memory is reserved but not initialized
> - **Initialized**: Memory contains a valid value

> r[t.slot.transitions]
>
> Valid state transitions:
> - Unallocated → Allocated (via `allocate`)
> - Allocated → Initialized (via `initialize`)
> - Initialized → Allocated (via `take` or `clear`)
> - Allocated → Unallocated (via `deallocate`)

> r[t.slot.invalid]
>
> Invalid transitions that must be prevented:
> - Unallocated → Initialized (skipping allocation)
> - Initialized → Unallocated (leaking memory)
> - Double initialization
> - Use after deallocation

## Struct Construction

> r[t.struct.fields]
>
> For struct types, each field is tracked independently. A struct is fully
> initialized when all of its fields are initialized.

> r[t.struct.build]
>
> A partial struct can only be converted to a complete value when all fields
> are initialized. Attempting to build an incomplete struct must fail.

## Safety Invariants

> r[t.safety.no-leak]
>
> Memory must never be leaked. If a value is initialized, it must either be:
> - Taken out and returned to the caller
> - Dropped properly when the partial is dropped

> r[t.safety.no-double-free]
>
> Values must never be dropped twice. Once a value is taken or the slot is
> cleared, it must not be dropped again.

> r[t.safety.no-uninit]
>
> Uninitialized memory must never be read as if it were initialized.
