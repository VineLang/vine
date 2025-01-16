# Extrinsics

Extrinsics are how programs running in the IVM interact with the outside world.

An extrinsic value is a nilary agent that represents an external entity. They
are opaque to the interaction net, and can only be manipulated and queried via
extrinsic functions and extrinsic branches.

An extrinsic function is an agent that represents an external operation. They
operate solely on extrinsic values, and return only extrinsic values. Extrinsic
functions may have side effects. Extrinsic functions are the only way for the
interaction net to manipulate extrinsic values.

An extrinsic branch is a ternary agent that represents a boolean query on an
external entity. They query a single extrinsic value, and select one of two
branches. Extrinsic branches are the only way for the interaction net to inspect
extrinsic values.

Currently, there is a fixed set of extrinsics built-in to IVM. In the future,
they will be extensible ([#54](https://github.com/VineLang/vine/issues/54)).

## Extrinsic Value Types

### `N32`

An `N32` extrinsic value represents a 32-bit natural number. They can be written
as integer literals in Ivy programs.

Vine's `N32`, `Char`, and `Bool` types are all represented by `N32` extrinsic
values.

### `F32`

An `F32` extrinsic value represents a 32-bit floating-point number. They can be
written as float literals in Ivy programs.

### `IO`

An `IO` extrinsic value represents an IO handle, and can be used to perform IO.
`IO` values cannot be written in Ivy programs. When an Ivy program is run, the
`::main` net is connected to an `IO` value. This is the only way to obtain an
`IO` value.

## Extrinsic Functions

### General Arithmetic

The `@add`/`@sub`/`@mul`/`@div`/`@rem` extrinsic functions perform arithmetic on
`N32` or `F32` values. They input two values and output one. If both inputs are
`N32`s, the output will be an `N32`; otherwise, the output is an `F32`.

### Comparisons

The `@eq`/`@ne`/`@lt`/`@le` extrinsic functions take two `N32` or two `F32`
values and return an `N32` value representing the result of the comparison (`1`
for true, and `0` for false).

### Specialized Arithmetic

The
`@n32_shl`/`@n32_shr`/`@n32_rotl`/`@n32_rotr`/`@n32_and`/`@n32_or`/`@n32_xor`
extrinsic functions take two `N32` values, perform a bitwise operation, and
output an `N32` value.

The `@n32_add_high`/`@n32_mul_high` extrinsic functions return the 32
most-significant bits of the 64-bit result of an operation on two `N32` values.

### IO

The `@io_print_char` extrinsic functions takes an `IO` value and an `N32` value
representing a Unicode scalar value, prints the character to stdout, and returns
an `IO` value.

The `@io_print_byte` extrinsic function takes an `IO` value and an `N32` value,
prints the least-significant byte to stdout, and returns an `IO` value.

The `@io_flush` extrinsic function takes an `IO` value and an `N32` value,
flushes stdout, discards the `N32`, and returns an `IO` value.

The `@io_read_byte` extrinsic function takes an `IO` value and an `N32` value,
reads a byte from stdin, and returns that byte as an `N32`, or the `N32`
parameter if no byte could be read.

The `@seq` extrinsic function takes two extrinsic values and returns the first.
It is useful for sequentializing side effects from other extrinsic functions.
