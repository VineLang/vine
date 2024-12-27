# Patterns

All variables are declared using variable patterns. Other types of patterns can be used to unwrap
data (typically, these patterns will have variable patterns as subpatterns).

For example, tuple patterns unwrap (aka destructure) tuples:

```rs
let tuple = (1, 2);
let (x, y) = tuple;
x // 1
y // 2
```

Patterns are _refutable_ if they may not always match. For example, enum variant patterns like
`Some(x)` are refutable, because the value might be of a different variant. Refutable patterns are
only allowed in `match` arms and `is` expressions; all other patterns must be _irrefutable_.

See `examples/fib_repl.vi` for an example of `match` expressions.
