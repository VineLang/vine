# References

References to local variables (and other [places]) can be made using the `&` reference operator.
(Currently, unlike Rust, all references are mutable.)

References can be unwrapped using the `&` reference pattern. This creates a _live binding_; any
changes to the local variable will mutate the referenced variable.

```rs
let x = 0;
increment(&x); // pass a reference to `x`
x // 1

fn increment(&x: &N32) { // unwrap the reference
  x += 1;
}
```

There are also a `*` dereference operator and a `*` dereference pattern, but the behavior is ,...a

References can also be dereferenced using the `*` operator. However, there are currently some
limitations to this, and reference patterns are the preferred method.
