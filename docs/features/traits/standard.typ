#import "/lib.typ": *

= Standard Traits

The standard library defines several traits, many of which control the behavior of built-in operators.

Implementations of the `Concat` trait control the behavior of the #op[`++`] operator.

Implementations of the `Cast` trait control the behavior of the #op[`as`] operator,
  which can be used for explicit type casting (#expr[`123 as String`]).
The `Cast` trait is also used in string interpolation to cast interpolated values into #ty[`String`]s.

== Arithmetic Operators

Implementations of
  the `Add`, `Sub`, `Mul`, `Div`, `Rem`, and `Neg` traits (from #vi[`std::ops::arithmetic`])
  control the behavior of
  the #op[`+`], #op[`-`], #op[`*`], #op[`/`], #op[`%`], and unary #op[`-`] operators.

The standard library defines implementations of these traits for all numeric types.

One can opt-in to element-wise arithmetic on pairs (#expr[`(1, 2) + (3, 4) == (4, 6)`])
by importing the implementations from #vi[`std::ops::elementwise`].

== Bitwise Operators

Implementations of
  the `And`, `Or`, `Xor`, `Not`, `Shl`, and `Shr` traits (from #vi[`std::ops::bitwise`])
  control the behavior of
  the #op[`&`], #op[`|`], #op[`^`], #op[`!`], #op[`<<`], and #op[`>>`] operators.

The standard library defines implementations of these traits for all integer types,
  and implementations of the first four for booleans.

(The logical operators #op[`&&`], #op[`||`], and #op[`=>`] cannot be overloaded.)

== Comparison Operators

Implementations of the `Eq` trait control the behavior of the #op[`==`] and #op[`!=`] operators.

Implementations of the `Lt` and `Le` traits control the behavior of the #op[`<`] and #op[`<=`] operators.
(The behavior of #op[`>`] and #op[`>=`] is implicitly derived from these.)

In most cases, rather than implementing `Lt` and `Le` directly, one should implement the `Ord` trait,
  which represents a total order and is used by things like #ty[`Map`].
(There are standard implementations of `Lt` and `Le` for any type that `Ord` is implemented for.)

== `Show`

The `Show` trait defines a way to represent the structure of a value for printing,
  which is useful for debugging.
Most types should implement the `Show` trait.

The #fn[`show`] method provided by the `Show` trait returns an instance of the `Show` enum,
  which can then be cast to a string to print.

For example:

```vi
[1, 2, 3, 4].show() as String // "[1, 2, 3, 4]"

let value = (true, Some(23 * 2), [(1, "one"), (2, "two"), (3, "three")] as Map)
io.println("{value.show()}")
// (true, Some(46), Map({ 1: "one", 2: "two", 3: "three" }))
```

Things that are too long to fit on one line will get broken into several lines:

```vi
let value = "
a very long list of strings
so long that all of the strings
could not possibly all fit
onto a single line
".split_trim("\n");
io.println("{value.show()}")
// [
//   "a very long list of strings",
//   "so long that all of the strings",
//   "could not possibly all fit",
//   "onto a single line",
// ]
```
