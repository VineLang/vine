# Composite Types

A composite type is a fixed-sized, heterogenous collection of values.

There are two kinds of composite types: *tuples* and *objects*.

```rs
// tuple
let a = (1, 'a', 4.6); // a: (N32, Char, F32)

// object
let b = { p: false, q: "xyz" }; // b: { p: Bool, r: String }
```

The values of a tuple are accessed by their index.

```rs
let a = (1, 'a', 4.6);
a.0 // 1
a.1 // 'a'
a.2 // 4.6

a.2 *= 10.0;
a.2 // 46.0
```

The values of an object are accessed by their key.

```rs
let b = { p: false, q: "xyz" };
b.p // false
b.q // "xyz"

b.p = !b.p;
b.p // true
```
