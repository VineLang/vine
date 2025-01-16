# Structs

Structs are fixed-sized, heterogenous collections of values.

Structs are either *tuples* or *objects*, and are either *anonymous* or *named*.

```rs
// anonymous tuple struct
let a = (1, 'a', 4.6); // a: (N32, Char, F32)

// anonymous object struct
let b = { p: false, q: "xyz" }; // b: { p: Bool, r: String }
```

```rs
// named tuple struct
struct Foo(N32, Char, F32);
let foo = Foo(1, 'a', 4.6); // foo: Foo

// named object struct
struct Bar { p: Bool, r: String }
let bar = Bar({ p: false, q: "xyz" }); // bar: Bar
```

The values of a tuple struct are accessed by their index.

```rs
let a = (1, 'a', 4.6);
a.0 // 1
a.1 // 'a'
a.2 // 4.6

a.2 *= 10.0;
a.2 // 46.0
```

The values of an object struct are accessed by their key.

```rs
let b = { p: false, q: "xyz" };
b.p // false
b.q // "xyz"

b.p = !b.p;
b.p // true
```

Named struct types can be generic.

```rs
struct Pair[T](T, T);

let u = Pair(1, 2); // u: Pair[N32]
let v = Pair(true, false); // v: Pair[Bool]

struct Box[T] { value: T }

let x = Box({ value: 46 }); // x: Box[N32]
let y = Box({ value: "abc" }); // y: Box[String]
```
