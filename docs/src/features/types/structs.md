# Structs

A *struct* creates a new type that wraps some *content*.

```rs
// define a struct named `Id` with content type `N32`
struct Id(N32);
```

Structs can be constructed by specifying their content:

```rs
let id = Id(46); // id: Id
```

A struct type is distinct from its content type:

```rs
let id: Id = 46; // error; expected type `Id`, found type `N32`
let num: N32 = Id(46); // error; expected type `N32`, found type `Id`
```

To access the content of a struct, one can use the unwrap operator, `!`:

```rs
let id = Id(46);
let num = id!; // num: N32
num // 46
```

Structs are also *nominal*; a struct type is distinct from any other struct
type, even one with the same content:

```rs
struct Foo(String);
struct Bar(String);
let foo: Foo = Bar("foo"); // error: expected type `Foo`, found type `Bar`
```

It's often useful to have struct types which wrap multiple fields; this can be
accomplished by having the content be a composite type:

```rs
struct Point((N32, N32));
let p = Point((1, 2));
// for tuples, the inner set of parentheses can be omitted:
let p = Point(1, 2);

p! // (1, 2)
p!.0 // 1
p!.1 // 2

// the `!` can be omitted when accessing a field:
p.0 // 1
p.1 // 2
```

```rs
struct Place({
  latitude: F32,
  longitude: F32,
});

let magnetic_north_pole = Place({
  latitude: 86.494,
  longitude: 162.867,
});

magnetic_north_pole! // { latitude: 86.494, longitude: 162.867 }
magnetic_north_pole!.latitude // 86.494
magnetic_north_pole!.longitude // 162.867

// the `!` can be omitted when accessing a field:
magnetic_north_pole.latitude // 86.494
magnetic_north_pole.longitude // 162.867
```

Struct types can also be generic.

```rs
struct Pair[T]((T, T));

let u = Pair(1, 2); // u: Pair[N32]
let v = Pair(true, false); // v: Pair[Bool]

struct Box[T]({ value: T })

let x = Box({ value: 46 }); // x: Box[N32]
let y = Box({ value: "abc" }); // y: Box[String]
```

By default, the content of a struct is private to the module the struct was
defined in:

```rs
mod points {
  pub struct Point((N32, N32));
  
  pub const origin: Point = Point(0, 0);
}

use points::{Point, origin};
Point(1, 2) // error; content of `Point` is private
origin.0 // error; content of `Point` is private
```

The content can be made public by giving it a visibility of `pub`:

```rs
mod points {
  pub struct Point(pub (N32, N32));
  //               ^-- visibility of the content
  
  pub const origin: Point = Point(0, 0);
}

use points::{Point, origin};
Point(1, 2) // ok
origin.0 // ok
```
