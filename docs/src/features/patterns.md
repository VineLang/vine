# Patterns

*Patterns* appear in:

- `let` bindings (`let pattern = ...`)
- `fn` parameters (`fn foo(pattern: ...) { ... }`)
- `match` arms (`match foo { pattern => { ... } }`)
- `is` expressions (`foo is pattern`)

Patterns are used to declare variables, unwrap data, and match values.

## Complete Patterns

Variables are declared using *variable patterns*.

```rs
let x = 1; // here, `x` is a variable pattern
```

Variable patterns are often subpatterns of other patterns.

*Tuple patterns* unwrap tuples:

```rs
let tuple = (1, 2);
let (x, y) = tuple; // here, `(x, y)` is a tuple pattern
                    // and `x` and `y` are variable subpatterns
x // 1
y // 2
```

Similarly, *struct patterns* unwrap structs:

```rs
struct Foo(N32, String);

let foo = Foo(123, "abc");
let Foo(x, y) = foo;
x // 123
y // "abc"
```

The `_` pattern is a complete pattern that discards the matched value.

```rs
let tuple = (1, 2);
let (x, _) = tuple; // discard the second field
x // 1
```

These are all *complete patterns*, because they match all values of their type.
Only complete patterns can be used in `let` bindings and `fn` parameters.

Other kinds of complete patterns include:

- [reference/dereference patterns](./references)
- [inverse patterns](./inverse)

## Incomplete Patterns

*Enum patterns* match values of an enum type. They are *incomplete patterns*,
because they do not match all values of their type; they only match one variant
of the enum. Any pattern with incomplete subpatterns is also incomplete.

Incomplete patterns can be used in `match` expressions to perform pattern
matching. For example:

```rs
fn display(score: Option[N32]) -> String {
  match score {
    Some(value) => "score: " ++ value.to_string(),
    None => "no score",    
  }
}

display(Some(123)) // "score: 123"
display(None) // "no score"
```

Incomplete patterns can also be used with the
[`is` operator](./conditions.md#the-is-operator).
