# Values, Spaces, and Places

The result of an expression in Vine has one of three *forms*: a value, a space,
or a place.

- a *value* represents some piece of data
- a *space* represents somewhere that data can be put
- a *place* is the combination of a value and a space.

We'll use the following code example to explore these concepts further.

```rs
let x = 1 + 2;
x + 1;
x = 5;
x *= 2;
```

On line 1, the expression `1 + 2` results in the value `3`. The variable `x` is
then declared with an initial value of `3`.

On line 2, the expression `x` results in its value `3`, so the expression
`x + 1` results in the value `4`. This expression is unused so its result is
simply discarded.

On line 3, we have an assignment statement. This expects a space on the
left-hand side, and a value on the right-hand side, and will put the value into
the space. In this particular case, the expression `x` on the left-hand side
results in a space – whatever value is put into this space will become the new
value of `x`. Then, the assignment operator puts the value `5` into the space.
The value of `x` is now `5`.

On line 4, the `*=` operator expects a place on the left-hand side, and a value
on the right-hand side. Thus, `x` results in its place, which is the combination
of the value `5` and a space to put a new value for `x`. The `*=` operator uses
the value to calculate `5 * 2`, and puts the value `10` into `x`'s space. Thus,
the value of `x` is now `10`.

## Expression Resolution

### Expression Positions

Every expression is located in either a value position, a space position, or a
place position. The form of the expression's position determines the form of the
result of the expression.

We saw above that the expression `x` could result in a value, a space, or a
place; this is determined by the form of its position.

- On line 2, in `x + 1`, `x` is in a value position, so it results in a value.
- On line 3, in `x = 5`, `x` is in a space position, so it results in a space.
- On line 4, in `x *= 2`, `x` is in a place position, so it results in a place.

### Expression Forms

Independent of its position, every expression has a form - it is either a value
expression, a space expression, or a place expression. For example, variable
expressions, like `x`, are place expressions.

The form of an expression determines the form of what the expression *evaluates
to*. If this form does not match the form of the expression's position, it is
*coerced* to become the result.

- On line 2, in `x + 1`, `x` evaluates to a place. It is in a value position, so
  it is coerced to a value.
- On line 3, in `x = 5`, `x` evaluates to a place. It is in a space position, so
  it is coerced to a space.
- On line 4, in `x *= 2`, `x` evaluates to a place. It is in a place position,
  so it is not coerced.

### Coercion

There are two rules for coercion.

- When a place is coerced to a value, the resulting value is a copy of the
  place's value; the other copy is put into the place's space.

- When a place is coerced to a space, the resulting space is the place's space;
  the place's value is discarded.

These two rules result in the behavior described in the first section.

Values and spaces cannot be coerced. It is an error to have a value expression
in a non-value position, or a space expression in a non-space position.
