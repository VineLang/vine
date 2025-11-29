#import "/lib.typ": *

= Values, Spaces, and Places

The result of an expression in Vine has one of three _forms_:
  a value, a space, or a place.

- a _value_ represents some piece of data
- a _space_ represents somewhere that data can be put
- a _place_ is the combination of a value and a space.

We'll use the following code example to explore these concepts further.

```vi
let x = 1 + 2;
x + 1;
x = 5;
x *= 2;
```

On line 1, the expression #expr[`1 + 2`] results in the value #expr[`3`].
The variable #expr[`x`] is then declared with an initial value of #expr[`3`].

On line 2, the expression #expr[`x`] results in its value #expr[`3`],
  so the expression #expr[`x + 1`] results in the value #expr[`4`].
This expression is unused so its result is simply discarded.

On line 3, we have an assignment statement.
This expects a space on the left-hand side,
  and a value on the right-hand side,
  and will put the value into the space.
In this particular case, the expression #expr[`x`] on the left-hand side results in a space --
  whatever value is put into this space will become the new value of #expr[`x`].
Then, the assignment operator puts the value #expr[`5`] into the space.
The value of #expr[`x`] is now #expr[`5`].

On line 4, the #op[`*=`] operator expects a place on the left-hand side,
  and a value on the right-hand side.
Thus, #expr[`x`] results in its place,
  which is the combination of the value #expr[`5`]
  and a space to put a new value for #expr[`x`].
The #op[`*=`] operator uses the value to calculate #expr[`5 * 2`],
  and puts the value #expr[`10`] into #expr[`x`]'s space.
  Thus, the value of #expr[`x`] is now #expr[`10`].

== Expression Resolution

=== Expression Positions

Every expression is located in either a value position, a space position, or a place position.
The form of the expression's position determines the form of the result of the expression.

We saw above that the expression #expr[`x`] could result in a value, a space, or a place;
  this is determined by the form of its position.

- On line 2, in #expr[`x + 1`], #expr[`x`] is in a value position, so it results in a value.
- On line 3, in #expr[`x = 5`], #expr[`x`] is in a space position, so it results in a space.
- On line 4, in #expr[`x *= 2`], #expr[`x`] is in a place position, so it results in a place.

=== Expression Forms

Independent of its position, every expression has a form --
  it is either a value expression, a space expression, or a place expression.
For example, variable expressions, like #expr[`x`], are place expressions.

The form of an expression determines the form of what the expression *evaluates to*.
If this form does not match the form of the expression's position,
  it is _coerced_ to become the result.

- On line 2, in #expr[`x + 1`], #expr[`x`] evaluates to a place.
  It is in a value position, so it is coerced to a value.
- On line 3, in #expr[`x = 5`], #expr[`x`] evaluates to a place.
  It is in a space position, so it is coerced to a space.
- On line 4, in #expr[`x *= 2`], #expr[`x`] evaluates to a place.
  It is in a place position, so it is not coerced.

=== Coercion

There are three rules for coercion:

- If `Fork[T]` is implemented,
    when a place of type `T` is coerced to a value,
    the value is `Fork::fork(&place)`.
- If `Drop[T]` is implemented,
    when a place of type `T` is coerced to a space,
    the resulting space is the place's space;
    the place's value is dropped.
- If `Drop[T]` is implemented,
    when a value of type `T` is coerced to a place,
    the resulting place is a combination of the value
    and a space that drops the updated value.

// In code:
// ```vi
// fn place_to_value[T; Fork[T]](&place: &T) -> T {
//   Fork::fork(&place)
// }
//
// fn place_to_space[T; Drop[T]](&place: &T) -> ~T {
//   let &(value; space) = &place;
//   Drop::drop(value)
//   ~space
// }
//
// fn value_to_place[T; Drop[T]](value: T) -> &T {
//   &(value; |> t { Drop::drop(t) })
// }
// ```

These rules result in the behavior described in the first section.
