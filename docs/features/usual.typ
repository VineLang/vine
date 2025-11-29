#import "/lib.typ": *

= The Usual <usual>

Vine has the usual complement of standard programming language features, including:

- @n32[integers]: #expr[`1 + 2 * 3`]
- @f32[floats]: #expr[`1.0 + 2.0 * 3.0`]
- @conditions[booleans] (including short-circuiting):
  #expr[`true and !(false or 1 == 2)`]
- @list[lists]: #expr[`[1, 2, 3] ++ [4, 5, 6]`]
- @string[strings]: #expr[`"12 + 34 = {12 + 34}"`]
- @composite[tuples]: #expr[`(1, 1.0, "abc")`], #expr[`(1, 2).0`]
- @variables[variables]: #vi[`let x = 5; x += 1`]
- @control-flow[control flow]:
  - #vi[`if condition { ... } else { ... }`]
  - #vi[`while condition { ... }`]
  - #vi[`return value`]
  - #vi[`break`], #vi[`continue`]

(Try some of these snippets in the `vine repl`!)

Many of Vine's features are influenced by Rust,
  and it has a similar expression-oriented syntax, type system, and module system.
