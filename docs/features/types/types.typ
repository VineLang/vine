#import "/lib.typ": *

= Types

Vine is statically typed. Every expression has a type, like #ty[`Bool`] or
#ty[`List[N32]`]. Type parameters are written in square brackets.

Types fall in to several categories:

- @primitive-types[primitive types] are fundamental types defined by the
  compiler
- @standard-types[standard types] are commonly-used types defined by the
  standard library
- @structs[structs] and @enums[enums] are user-defined types

#let children = (
  "primitives.typ",
  "standard.typ",
  "composite.typ",
  "structs.typ",
  "enums.typ",
)

