# The Inverse

Vine's concept of the *inverse* is not something seen in other programming
languages, as it is a concept that is only possible due to the unique properties
of interaction nets.

The *inverse type* `~N32` represents an *expectation* of an `N32`. Such an
expectation must be *fulfilled* with an `N32`. The type `~~N32` is equivalent to
the type `N32`.[^1]

The *inverse operator* can be applied to a value, a space, or a place.

- The inverse operator, when applied to a space `s` of type `T`, evaluates to a
  value of type `~T`. This value is the expectation of a `T` that will be put
  into the space `s`.

- The inverse operator, when applied to a value `v` of type `T`, evaluates to a
  space of type `~T`. Whatever expectation is put into the space will be
  fulfilled with the value `v`.

- The inverse operator, when applied to a place `p` of type `T`, evaluates to a
  place `q` of type `~T`. The value of `q` is the inverse of the space of `p`;
  the space of `q` is the inverse of the value of `p`.

The *inverse pattern* is essentially the reverse of the inverse operator, and
can be used to unwrap inverse types.

## Out Parameters

The inverse operator can be used to implement out parameters:

```rs
// `foo` takes an expectation of an `N32` as its parameter
fn foo(~n: ~N32) {
  // and fulfills it with `123`
  n = 123;
}

let x: N32;
let e = ~x; // e: ~N32
// `e` is now an expectation of an `N32` to put into `x`
foo(e); // we pass `e` to `foo`, which fulfills it with `123`
x // 123
```

(References can serve a similar role, but references also carry with them a
current value.)

## DIY Functions

The inverse operator can also be used to implement functions from scratch. The
following code using functions:

```rs
let add_one = fn(x: N32) { x + 1 };
let a = 1;
let b = add_one(a);
b // 2
```

Could be written without functions like so:

```rs
// `add_one` will be a tuple:
// - the first element will be an expectation of the input
// - the second element will be the output
let add_one: (~N32, N32) = do {
  let x: N32;
  (
    ~x, // create an expectation for the input, which will be put into `x`
    x + 1, // calculate the output
  )
};
let a = 1;
let b = do {
  let (~i, o) = add_one; // destructure the tuple
  i = a; // fulfill the input expectation with `a`
  o // evaluate to the output
};
b // 2
```

Note that here, the order in which these computations can resolve does not align
with the imperative order of execution; the fulfillment on line 14 must be
resolved before `x + 1` on line 8 can be resolved. This value is in a sense
flowing "backwards"; counter to the usual forward flow.

## Backwards Flow

This effect can be generalized; if you invert all uses of a variable, the
variable will flow "backwards":

```rs
// Normal, forward flow:
let x: String;
x = "a";
io.println("0: " ++ x);
io.println("1: " ++ x);
x = "b";
io.println("2: " ++ x);
io.println("3: " ++ x);
x = "c";
```

```
0: a
1: a
2: b
3: b
```

```rs
// Inverted, backward flow:
let ~x: String;
~x = "a";
io.println("0: " ++ ~x);
io.println("1: " ++ ~x);
~x = "b";
io.println("2: " ++ ~x);
io.println("3: " ++ ~x);
~x = "c";
```

```
0: b
1: b
2: c
3: c
```

Normally, writing to a variable affects accesses on later lines. For an inverted
variable, writing to a variable affects accesses on *earlier* lines.

This gets extra peculiar when you initialize the variable:

```rs
// Normal, forward flow:
let x = "a";
io.println("0: " ++ x);
io.println("1: " ++ x);
x = "b";
io.println("2: " ++ x);
io.println("3: " ++ x);
x = "c";
io.println("4: " ++ x);
io.println("5: " ++ x);
```

```
0: a
1: a
2: b
3: b
4: c
5: c
```

```rs
// Inverted, backward flow:
let ~x = "a";
io.println("0: " ++ ~x);
io.println("1: " ++ ~x);
~x = "b";
io.println("2: " ++ ~x);
io.println("3: " ++ ~x);
~x = "c";
io.println("4: " ++ ~x);
io.println("5: " ++ ~x);
```

```
0: b
1: b
2: c
3: c
4: a
5: a
```

The initialization of a normal variable affects accesses on lines before any
reassignment of the variable. The initialization of an inverted variable affects
accesses on lines *after* any reassignment of the variable.

## Time Travel

At this point, a natural question is: when is this useful?

It turns out that there are many situations where the inverse operator is
useful, and allows writing code that could not be expressed without it.

Consider the following function:

```rs
fn sub_min(&list: &List[N32]) {
  let min_acc = None[N32];

  let it = list.iter();
  while it.next() is Some(&val) {
    if min_acc is Some(m) => val < m {
      min_acc = Some(val);
    }
  }

  let min = min_acc.unwrap();

  let it = list.iter();
  while it.next() is Some(&val) {
    val -= min
  }
}
```

This function calculates the minimum of a list of numbers, and subtracts every
number in the list by that minimum. For example:

```rs
let x = [4, 3, 7, 9];
sub_min(&x);
x // [1, 0, 4, 6]
```

This function currently iterates over the list twice; once to calculate the
minimum, and once to do the subtraction. At a first glance, it looks like there
is no way to merge these two loops, because you need to calculate the minimum of
all the numbers before you can subtract from any of them.

The only way you could merge these loops is if you could somehow know what the
minimum value was before the loop even started.

As it turns out, this *is* possible, using the inverse operator. Since an
inverted variable flows "backwards in time", we can use one to send the minimum
values from the end of the loop back to all of the previous iterations of the
loop.

```rs
fn sub_min(&list: &List[N32]) {
  // our accumulator will still flow forwards, as usual
  let min_acc = None[N32];

  // but we'll have an inverted variable to store the final minimum value
  let ~min: N32;

  let it = list.iter();
  while it.next() is Some(&val) {
    // as we iterate over the list, we update `min_acc` as usual
    if min_acc is Some(m) => val < m {
      min_acc = Some(val);
    }
    
    // but we simultaneously subtract the final minimum from the value
    val -= ~min;
  }

  // now that we know the real minimum, we set the inverted variable,
  // propagating this value back to all of the loop iterations
  ~min = min_acc.unwrap();
}
```

This is mind-bending to think about, but extremely useful once you get the hang
of it!

---

[^1]: Any value can be considered to be an expectation of an expectation of
    itself. Consider the value `5`. It expects to be used, at some point. Let's
    call the thing that will use it `u`. `u` is expecting an `N32`. (That
    expectation will eventually be fulfilled with `5`.) Since `u` is expecting
    an `N32`, `u` is an `~N32`. Since `5` is expecting `u`, and `u` is an
    `~N32`, `5` is an `~~N32` (in addition to being an `N32`). Thus, the types
    `N32` and `~~N32` are equivalent.
