# The `is` Operator

The `is` operator checks if an expression matches some pattern, and returns a boolean. Any variables
bound by the patterns are in scope in subsequent 'true-paths' including `&&` chains, then-blocks of
an `if`, and the body of a `while`.

```rs
let option = Some(1)
if option is Some(value) && value > 0 {
  value // evaluates to 1
} else {
  // value is not bound in this scope
}
```

Note that `!`s and `||`s break the true-path, so something like `!(a is Some(x)) || x > 0` is
invalid. The _implies_ operator can be used instead for this: `a is Some(x) => x > 0`.
