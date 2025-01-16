# The Usual

Vine has the usual complement of standard programming language features,
including:

- integer literals and operations: `1 + 2 * 3`
- float literals and operations: `1.0 + 2.0 * 3.0`
- booleans and boolean operators (including short-circuiting):
  `true && !(false || 1 == 2)`
- character, string, and list literals and operations:
  `"abc" ++ "def" ++ ['g', 'h', 'i']`
- tuples: `(1, 1.0, "abc")`, `(1, 2).0`
- variables: `let x = 5; x += 1`
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
