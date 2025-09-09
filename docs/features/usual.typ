# The Usual

Vine has the usual complement of standard programming language features,
including:

- [integers](./types/primitives.md#n32): `1 + 2 * 3`
- [floats](./types/primitives.md#f32): `1.0 + 2.0 * 3.0`
- [booleans](./conditions.md) (including short-circuiting):
  `true && !(false || 1 == 2)`
- [lists](./types/standard.md#list): `[1, 2, 3] ++ [4, 5, 6]`
- [strings](./types/standard.md#string): `"abc" ++ "def"`
- [tuples](./types/structs.md): `(1, 1.0, "abc")`, `(1, 2).0`
- [variables](./variables.md): `let x = 5; x += 1`
- basic control flow:
  - `if condition { ... } else { ... }`
  - `while condition { ... }`
  - `loop { ... }` (loops until `break`)
  - `return value`
  - `break`, `continue`
  - loop labels: `while.label ... { ... break.label ... }`

(Try some of these snippets in the `vine repl`!)

Many of Vine's features are influenced by Rust, and it has a similar
expression-oriented syntax, type system, and module system.
