#import "/lib.typ": *

= Fork and Drop <flex>

Every type in Vine has a _flexibility_,
  which is the combination of whether its values can be _forked_
  and whether its values can be _dropped_.

Forking a value gives two copies of the value; dropping a value destroys it.
Behind the scenes, any time a value is used multiple times, it is first forked,
  and any time a value is not used, it is dropped.
```vi
let used_multiple_times = 123;
(used_multiple_times, used_multiple_times)
// equivalent to:
let used_multiple_times = 123;
(used_multiple_times.fork(), used_multiple_times)
```
```vi
let used_zero_times = 456;
// equivalent to:
let used_zero_times = 456;
used_zero_times.drop();
```

There are several places in Vine where one can specify the flexibility of a type,
  including in type parameters, struct/enum definitions, and closure expressions.

- By default, the type is neither forkable nor droppable; its values must be used exactly once.
- With an annotation of #vi[`?`], the type will be droppable; its values may be used zero or one times.
- With an annotation of #vi[`+`], the type will be forkable; its values may be used one or more times.
- With an annotation of #vi[`*`], the type will be forkable and droppable; its values may be used zero or more times.

(This syntax parallels regex quantifiers.)

== The `Fork` and `Drop` traits

The behavior of forking/dropping values of a type is controlled by the `Fork` and `Drop` traits:

```vi
trait Fork[T] {
  fn .fork(&self: &T) -> T;
}

trait Drop[T] {
  fn .drop(self: T);
}
```

A type #ty[`T`] is forkable if there is an implementation of #vi[`Fork[T]`],
  and droppable if there is an implementation of #vi[`Drop[T]`].

Most types implement both `Fork` and `Drop`.
A notable exception is #ty[`IO`], which can't be forked or dropped.
This means that at any point in the execution of a Vine program,
  there is exactly one IO handle,
  so all IO operations are ordered.

When defining a struct or enum, the flexibility can be annotated
  after the #vi[`struct`]/#vi[`enum`] keyword,
  to automatically implement `Fork` and/or `Drop` for the type
  whenever its type parameters all implement `Fork`/`Drop`.
```vi
pub struct* Named[T](String, T);

// equivalent to:

pub struct Named[T](String, T);
pub mod Named {
  pub impl fork[T; Fork[T]]: Fork[Named[T]] {
    fn fork(&Named[T](name, value)) -> Named[T] {
      Named(name.fork(), value.fork())
    }
  }
  pub impl drop[T; Drop[T]]: Drop[Named[T]] {
    fn drop(Named[T](name, value)) {
      name.drop();
      value.drop();
    }
  }
}
```

When writing a generic function, the flexibility of a type parameter can be annotated,
  which automatically adds `Fork`/`Drop` implementation parameters.
```vi
fn lots_of[T*](value: T, length: N32) -> List[T] {
  let list = [];
  for _ in 0..length {
    list.push_back(value);
  }
  list
}

// equivalent to:

fn lots_of[T; Fork[T], Drop[T]](value: T, length: N32) -> List[T] {
  ...
}
```

When writing a closure, its flexibility can be annotated after the #vi[`fn`] keyword.
The flexibility of a closure corresponds to the number of times it can be called.
```vi
let called_exactly_once = fn (...) { ... };
let called_at_most_once = fn? (...) { ... };
let called_at_least_once = fn+ (...) { ... };
let called_any_number_of_times = fn* (...) { ... };
```
