# Primitive Types

## `N32`

The type `N32` describes natural numbers[^nats], represented with 32 bits of
precision.

`N32` values can be written as literals in decimal (`46`), hex (`0x2e`), or
binary (`0b101110`). Digits can be separated with underscores (`1_000_000`).

`N32`s support the usual arithmetic and bitwise operators (`4 * 11 + 2`,
`5 << 3 | 6`)

## `F32`

The type `F32` describes 32-bit floating-point numbers (following IEEE 754).

`F32` values can be written as literals (`4.6e1`). The decimal point is
required.

`F32`s support the usual arithmetic operators (`3.6 * 12.3 + 1.72`).

## `Char`

The type `Char` describes Unicode scalar values. `Char`s are primarily used
within `String`s.

`Char` values can be written as literals using single quotes (`'.'`).

`Char`s support adding an `N32`, resulting in another `Char` (`'a' + 4`), as
well as subtracting another `Char`, resulting in an `N32` (`'G' - 'A'`).

## `Bool`

The type `Bool` describes booleans.

The two `Bool` values can be written as literals (`true`, `false`).

`Bool`s support the usual short-circuiting logical operators (`&&`, `||`, `!`)
and non-short-circuiting ("bitwise") operators (`&`, `|`, `^`).

Expressions that evaluate to booleans are called [conditions](./conditions.md).

## `IO`

`IO` is a special primitive type used to interact with the outside world. Values
of this type cannot be explicitly constructed; instead, an `IO` handle is passed
in to `main` at the start of the program. See the section on [IO](../io.md) for
more detail.

---

[^nats]: Natural numbers are non-negative integers. In other programming
    languages, they are often referred to as "unsigned integers". Seeing as
    positive integers do have a sign (namely, a positive sign), the only truly
    unsigned integer is zero.
