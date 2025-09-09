# References

A *reference* is a value that represents a place.

References can be created using the `&` reference operator.

```rs
let x = 1; // x: N32
let r = &x; // r: &N32
```

`r` is now a reference to the place that `x` evaluated to. It is of type `&N32`,
the type representing references to `N32`s.

References can be unwrapped using the `&` reference pattern.

```rs
let x = 1;
let r = &x;

let &y = r; // unwrap the reference `r` into a place, and name it `y`
y = 2; // write the value `2` into the place

x // 2
```

References can be passed to function calls to allow mutation of local variables:

```rs
fn increment(&n: &N32) { 
  n += 1;
}

let x = 0;
increment(&x);
x // 1
```

Under the hood, methods that mutate their receiver use references:

```rs
let x = [1, 2, 3];
x.push_back(4); // equivalent to: `List::push_back(&x, 4)` (note the reference!)
x // [1, 2, 3, 4]
```

References are also useful when a function only needs to access part of a data
structure:

```rs
// Inefficient

fn is_empty(list: List[N32]) -> N32 {
  list.len() != 0
}

let x = [1, 2, 3, 4];
let e = is_empty(x); // inefficient; clones the whole list
x // [1, 2, 3, 4]
e // false
```

```rs
// Efficient

fn is_empty(&list: &List[N32]) -> N32 {
  list.len() != 0
}

let x = [1, 2, 3, 4];
let e = is_empty(&x); // efficient; no clones necessary
x // [1, 2, 3, 4]
e // false
```

(This is why `List::len` takes its receiver by reference, despite not needing to
mutate it.)

## Dereference Operator

The place contained in a reference can also be accessed with the `*` dereference
operator:

```rs
let x = 1;
let r = &x;
*r = 2;
x // 2
```

(Note that there are currently some bugs/limitations with this; if you have a
reference stored in a variable, it is preferred to use reference patterns
instead.)

This can be useful when a function returns a reference:

```rs
let x = [1, 2, 3];
*x.get(2) = 4;
x // [1, 2, 4]
```

The `*` operator can also be written as a postfix operator using `.*`:

```rs
let x = [1, 2, 3];
x.get(2).* = 4;
x // [1, 2, 4]
```

## Dereference Pattern

The `*` dereference *pattern* takes a value pattern of type `&T`, and yields a
place pattern of type `T`. This can be used, for example, to "split" a reference
to a struct into references to its fields.

```rs
struct Pair(N32, N32);

fn increment(&n: &N32) { 
  n += 1;
}

fn increment_pair(r: &Pair) {
  let &Pair(*x, *y) = r; // x: &N32, y: &N32
  increment(x);
  increment(y);
}

let pair = Pair(0, 0);
increment_pair(&pair);
pair // Pair(1, 1)
```
