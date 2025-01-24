# Scratch

---

Vine parser drives the lexer and can instruct it to lex differently in different
parts of the grammar.

Parsing inline ivy is handled by the Vine parser swapping in the ivy parser &
lexer.

Parser is intentionally limited to one token of lookahead. Allows for parsing
error reporting to detail expected tokens. Parsing is `O(n)` since there is no
backtracking. Tokens are streamed from lexer to parser without vector data
structure.

---

```
fn(T) -> T ~~ (~T, T)
&T ~~ (T, ~T)
fn(T) -> T ~~ ~&T
fn(T) ~~ ~T
fn(&T) ~~ fn(T) -> T

   fn(&T)
~~ fn(~(fn(T) -> T))
~~ fn() -> fn(T) -> T)
~~ fn(T) -> T
```

---
