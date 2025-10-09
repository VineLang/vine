#import "/lib.typ": *

= References <references>

A _reference_ is a value that represents a place.

References can be created using the #vi[`&`] reference operator.

```vi
let x = 1; // x: N32
let r = &x; // r: &N32
```

`r` is now a reference to the place that `x` evaluated to.
It is of type #ty[`&N32`], the type representing references to #ty[`N32`]s.

References can be unwrapped using the #ty[`&`] reference pattern.

```vi
let x = 1;
let r = &x;

let &y = r; // unwrap the reference `r` into a place, and name it `y`
y = 2; // write the value `2` into the place

x // 2
```

References can be passed to function calls to allow mutation of local variables:

```vi
fn increment(&n: &N32) { 
  n += 1;
}

let x = 0;
increment(&x);
x // 1
```

Under the hood, methods that mutate their receiver use references:

```vi
let x = [1, 2, 3];
x.push_back(4); // equivalent to: `List::push_back(&x, 4)` (note the reference!)
x // [1, 2, 3, 4]
```

References are also useful when a function only needs to access part of a data structure:

```vi
// Inefficient

fn is_empty(list: List[N32]) -> N32 {
  list.len() != 0
}

let x = [1, 2, 3, 4];
let e = is_empty(x); // inefficient; clones the whole list
x // [1, 2, 3, 4]
e // false
```

```vi
// Efficient

fn is_empty(&list: &List[N32]) -> N32 {
  list.len() != 0
}

let x = [1, 2, 3, 4];
let e = is_empty(&x); // efficient; no clones necessary
x // [1, 2, 3, 4]
e // false
```

(This is why #vi[`List::len`] takes its receiver by reference,
  despite not needing to mutate it.)

== Dereference Operator

The place contained in a reference can also be accessed with the #vi[`*`] dereference operator:

```vi
let x = 1;
let r = &x;
*r = 2;
x // 2
```

This can be useful when a function returns a reference:

```vi
let x = [1, 2, 3];
*x.get(2) = 4;
x // [1, 2, 4]
```

The #op[`*`] operator can also be written as a postfix operator using #op[`.*`]:

```vi
let x = [1, 2, 3];
x.get(2).* = 4;
x // [1, 2, 4]
```

== Dereference Pattern

The #op[`*`] dereference _pattern_ takes a value pattern of type #ty[`&T`],
  and yields a place pattern of type #ty[`T`].
This can be used, for example, to "split" a reference to a struct into references to its fields.

```vi
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
