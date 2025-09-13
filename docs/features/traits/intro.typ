#import "/lib.typ": *

= Using Traits

A _trait_ defines functionality that can be shared between types. For example,
one could define an `Equal` trait to represent equality.

```vi
trait Equal[T] {
  fn .equal(x: T, y: T) -> Bool;
}
```

The `Equal` trait is generic on a type #ty[`T`], and has a single method, #fn[`equal`],
which tests for equality between two values of type #ty[`T`].

Traits only define the signature of their methods, so to actually call these
methods, the trait has to be _implemented_.

```vi
struct Color({ r: N32, g: N32, b: N32 });
const red: Color = Color({ r: 255, g: 0, b: 0 });
const green: Color = Color({ r: 0, g: 255, b: 0 });
const blue: Color = Color({ r: 0, g: 0, b: 255 });

impl equal_color: Equal[Color] {
  fn equal(x: Color, y: Color) -> Bool {
    x.r == y.r && x.g == y.g && x.b == y.b
  }
}
```

Now, we've defined an implementation named `equal_color` of the trait `Equal`
for the type #ty[`Color`], so we can now call the #fn[`equal`] method on #ty[`Color`]s:

```vi
red.equal(blue) // false
red.equal(red) // true
```

If we wanted to use this method on other types, we could create more
implementations.

```vi
impl equal_bool: Equal[Bool] {
  fn equal(x: Bool, y: Bool) -> Bool {
    x && y || !x && !y
  }
}
```

(Most of the time, the names of implementations don't matter, but they are used
to disambiguate if multiple implementations could apply.)

== Implementation Parameters

The advantage of using traits instead of just defining individual functions for
types is that, with traits, one can abstract over any type that has an
implementation of a certain trait. For example, we could write a `not_equal`
function:

```vi
fn not_equal[T; Equal[T]](a: T, b: T) -> Bool {
  !a.equal(b)
}
```

The #fn[`not_equal`] function takes one type parameter, #ty[`T`], and one _implementation
parameter_, of the trait #vi[`Equal[T]`]. (The semicolon inside the generic
parameters separates the type parameters from the implementation parameters.)
Since `a` and `b` are of type #ty[`T`], and there is an implementation of #vi[`Equal[T]`],
`equal` can be called on `a` and `b`.

We can then call this function on any types that we've implemented `Equal` for.

```vi
not_equal(red, blue) // true
not_equal(true, true) // false
```

Without traits, we would have to manually implement this function for every type
we created an #fn[`equal`] method for.

Implementations themselves can also be generic and take implementation
parameters. This allows us to implement #vi[`Equal[List[T]]`] for any #ty[`T`] where
#vi[`Equal[T]`] is implemented:

```vi
impl equal_list[T; Equal[T]]: Equal[List[T]] {
  fn equal(x: List[T], y: List[T]) -> Bool {
    if x.len() != y.len() {
      return false;
    }
    while x.pop_front() is Some(a) && y.pop_front() is Some(b) {
      if not_equal(a, b) {
        return false;
      }
    }
    true
  }
}
```

The signature of `equal_list` says that for any type #ty[`T`], if there is an
implementation of #vi[`Equal[T]`], then `equal_list` implements #vi[`Equal[List[T]]`]. We
can thus now check equality for lists of colors or lists of booleans:

```vi
[red, green, blue].equal([red, blue, green]) // false
[true, false].equal([true, false]) // true
```

We can even check equality for lists of lists of colors!

```vi
[[red, blue], [green, red]].equal([[red, blue], [green, red]]) // true
[[], [red]].equal([[red], []]) // false
```
