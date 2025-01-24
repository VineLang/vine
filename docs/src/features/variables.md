# Variables

The primary way to declare new variables is with the `let` statement.

```rs
let x = 4; // declare the variable `x` with an initial value of `4`
x // 4
x = 6; // assign a new value, `6`, to `x`
x // 6
```

You can also declare a new variable with no initial value.

```rs
let x;
// `x` has no value!
x = 5; // assign `5` to `x`
x // 5
```

Using a variable with no value will currently lead to unexpected behavior. In
the future, this will be a type error.

(Note that, in more advanced uses of the `let` statement, declaring a variable
with an initializer is not always equivalent to declaring it no initializer and
then assigning to it.)

All variables have a type, which is inferred by default.

```rs
let x = "abc"; // x: String
x = 5; // error!
```

The type of a variable can also be explicitly specified with a type annotation:

```rs
let x: String = "abc";
let y: String = 5; // error!
```

## Shadowing

Declaring a variable with the same name as a variable already in scope *shadows*
the previous variable, making it inaccessible for the duration of the scope.

```rs
let x = 1;
x // 1
let x = "abc";
x // "abc"
```

This is different from reassigning the variable in a number of ways; for one,
the new variable can have a different type (like in the above example).

Variables shadowed within a block will no longer be shadowed after the end of
the block.

```rs
let x = 1;
x // 1
if x > 0 {
  let x = "abc";
  x // "abc"
}
x // 1
```
