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

Types have a flex of none/fork/drop/full. the type has no/fork/drop/full
flexibility

---

```rs
#[builtin = "Fork"]
pub trait Fork[T] {
  fn .fork(&self: &T) -> T;
}

#[builtin = "Drop"]
pub trait Drop[T] {
  fn .drop(self: T);
}
```

place : T* value : T+ space : T-

~place : ~T* ((~T)*) ~value : ~T- ((~T)-) ~space : ~T+ ((~T)+)

- desired that the following holds: `coerce(expr) == ~coerce_inv(~expr)`

- coerce `(value; space) : T` to a value
  - if `T` forkable, `out: Fork::fork(&(value; space))`
  - if `~T` droppable, `Drop::drop(~space); out: value`
  - if neither / both, error
- coerce `(value; space): T` to a space
  - if `T` droppable, `Drop::drop(value); out: space`
  - if `~T` forkable, `out: ~Fork::fork(&(~space; ~value))`
  - if neither / both, error

- coerce `value : T` to a place
  - if `T` droppable: `out: (value; ~> Drop::drop(@))`

- coerce `space : T` to a place
  - if `~T` droppable: `out: (~~> Drop::drop(@); space)`

- `place_to_value(place[T]) == ~place_to_space(~place[T])`
  - suppose `~T` is droppable
    - `place_to_value(place[T]) == ~drop_value_use_space(~place[T])`
    - `place_to_value(place[T]) == drop_space_use_value(place[T])`

coerce place to value if forkable: fork and take value coerce place to space if
dropable, drop value use space
