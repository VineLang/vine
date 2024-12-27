# Values, Places, and Spaces

An expression in Vine can evaluate to one of three things: a value, a place, or a space.

- a _value_ represents some piece of data
- a _space_ represents somewhere that data can be put
- a _place_ is the combination of a value and a space.

We'll use the following code example to explore these concepts further.

```rs
1 | let x = 1 + 2;
2 | x = 4;
3 | x + 1;
4 | x *= 2;
```

On line 1, the expression `1 + 2` evaluates to the value `3`. The variable `x` is then declared with
an initial value of `3`.

On line 2, we have an assignment statement. This expects a space on the left-hand side, and a value
on the right-hand side, and will put the value into the space. In this particular case, the
expression `x` on the left-hand side evaluates to a space – whatever value is put into this space
will become the new value of `x`. Then, the assignment operator puts the value `4` into the space.
The value of `x` is now `4`.

On line 3, the expression `x` evaluates to its value `4`, so the expression `x + 1` evaluates to the
value `5`. This expression is unused so its result is simply discarded.

On line 4, the `*=` operator expects a place on the left-hand side, and a value on the right-hand
side. Thus, `x` evaluates to its place which is the combination of the value `4` and a space to put
a new value for `x`. The `*=` operator uses the value to calculate `4 * 2`, and puts the value `8`
into `x`'s space. Thus, the value of `x` is now `8`.

## Expression Positions

Every expression is in either a value position, a place position, or a space position.

We saw above that the expression `x` could evaluate to a value, a place, or a space, depending upon
what kind of position it was in.

- on line 2, in `x = 4`, `x` is in a space position, so it evaluates to a space
- on line 3, in `x + 1`, `x` is in a value position, so it evaluates to a value
- on line 4, in `x *= 2`, `x` is in a place position, so it evaluates to a place

In particular, variable expressions, like `x`, are place expressions (expressions that evaluate to
places). When a place expression is used in a value or space position, the place is _coerced_ to a
value or a space:

- when a place is coerced to a value, the resulting value is a copy of the place's value; the other
  copy is put into the place's space.

- when a place is coerced to a space, the resulting space is the place's space; the place's value is
  discarded.

These two rules result in the behavior described above.

Values and spaces cannot be coerced. It is an error to have a value expression in a non-value
position, or a space expression in a non-space position.
