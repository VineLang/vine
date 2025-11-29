#import "/lib.typ": *

= The Inverse <inverse>

*Note*:
This page is *bad* and _should feel bad_.
It is left here for posterity.
The inverse operator is a _niche, low-level_ feature,
  and is not explained well here. 

#t.hr()

Vine's concept of the _inverse_ is not something seen in other programming languages,
  as it is a concept that is only possible due to the unique properties of interaction nets.

The _inverse type_ #ty[`~N32`] represents an _expectation_ of an #ty[`N32`].
Such an expectation must be _fulfilled_ with an #ty[`N32`].
The type #ty[`~~N32`] is equivalent to the type #ty[`N32`].
#footnote[
  Any value can be considered to be an expectation of an expectation of itself.
  Consider the value #vi[`5`].
  It expects to be used, at some point.
  Let's call the thing that will use it `u`.
  `u` is expecting an #ty[`N32`].
  (That expectation will eventually be fulfilled with #vi[`5`].)
  Since `u` is expecting an #ty[`N32`], `u` is an #ty[`~N32`].
  Since #vi[`5`] is expecting `u`, and `u` is an #ty[`~N32`], #vi[`5`] is an #ty[`~~N32`]
    (in addition to being an #ty[`N32`]).
  Thus, the types #ty[`N32`] and #ty[`~~N32`] are equivalent.
]

The _inverse operator_ can be applied to a value, a space, or a place.

- The inverse operator, when applied to a space `s` of type `T`,
    evaluates to a value of type `~T`.
  This value is the expectation of a `T` that will be put into the space `s`.

- The inverse operator, when applied to a value `v` of type `T`,
    evaluates to a space of type `~T`.
  Whatever expectation is put into the space will be fulfilled with the value `v`.

- The inverse operator, when applied to a place `p` of type `T`,
    evaluates to a place `q` of type `~T`.
  The value of `q` is the inverse of the space of `p`;
    the space of `q` is the inverse of the value of `p`.

The _inverse pattern_ is essentially the reverse of the inverse operator,
and can be used to unwrap inverse types.

== Out Parameters

The inverse operator can be used to implement out parameters:

```vi
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

(References can serve a similar role,
  but references also carry with them a current value.)

== DIY Functions

The inverse operator can also be used to implement functions from scratch.
The following code using functions:

```vi
let add_one = fn(x: N32) { x + 1 };
let a = 1;
let b = add_one(a);
b // 2
```

Could be written without functions like so:

```vi
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

Note that here, the order in which these computations can resolve
  does not align with the imperative order of execution;
  the fulfillment on line 14 must be resolved before `x + 1` on line 8 can be resolved.
This value is in a sense flowing "backwards";
  counter to the usual forward flow.

== Backwards Flow

This effect can be generalized;
  if you invert all uses of a variable,
  the variable will flow "backwards":

```vi
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

```vi
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

Normally, writing to a variable affects accesses on later lines.
For an inverted variable,
  writing to a variable affects accesses on _earlier_ lines.

This gets extra peculiar when you initialize the variable:

```vi
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

```vi
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

The initialization of a normal variable affects
  accesses on lines before any reassignment of the variable.
The initialization of an inverted variable affects
  accesses on lines _after_ any reassignment of the variable.

== Time Travel

At this point, a natural question is: when is this useful?

It turns out that there are many situations where the inverse operator is useful,
  and allows writing code that could not be expressed without it.

Consider the following function:

```vi
fn sub_min(&list: &List[N32]) {
  let min_acc = None[N32];

  let it = list.iter();
  while it.next() is Some(&val) {
    if min_acc is Some(m) impl val < m {
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

This function calculates the minimum of a list of numbers,
  and subtracts every number in the list by that minimum.
For example:

```vi
let x = [4, 3, 7, 9];
sub_min(&x);
x // [1, 0, 4, 6]
```

This function currently iterates over the list twice;
  once to calculate the minimum, and once to do the subtraction.
At a first glance, it looks like there is no way to merge these two loops,
  because you need to calculate the minimum of all the numbers
  before you can subtract from any of them.

The only way you could merge these loops
  is if you could somehow know what the minimum value was
  before the loop even started.

As it turns out, this _is_ possible, using the inverse operator.
Since an inverted variable flows "backwards in time",
  we can use one to send the minimum values from the end of the loop
  back to all of the previous iterations of the loop.

```vi
fn sub_min(&list: &List[N32]) {
  // our accumulator will still flow forwards, as usual
  let min_acc = None[N32];

  // but we'll have an inverted variable to store the final minimum value
  let ~min: N32;

  let it = list.iter();
  while it.next() is Some(&val) {
    // as we iterate over the list, we update `min_acc` as usual
    if min_acc is Some(m) impl val < m {
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

This is mind-bending to think about,
but extremely useful once you get the hang of it!
