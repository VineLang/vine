# Scratch

## What is vine?

Vine is an experimental new programming language based on interaction nets.

Vine is multi-paradigm language, supporting both functional and imperative patterns, with a strong
degree of interop.

Vine includes most modern language features, although it does not yet have a type system.

Vine currently has:

- IO
- functions
- standard control flow constructs
- mutable variables
- ADTs & pattern matching
- module system
- type system

Vine does not yet have:

- package manager
- debugger

## Why is vine?

## How is vine?

Vine compiles to [Ivy](./ivy/), a low-level interaction combinator programming language.

Ivy code runs on the [IVM](./ivm/), a performant interaction combinator runtime.

- language features/semantics
  - by default, Rust
  - value, place, space
  - inverse operator
  - module system
  - inline ivy

- advanced
  - wire cutting (place value space inverse)
  - compiler implementation details
    - XFG, interfaces, stages
