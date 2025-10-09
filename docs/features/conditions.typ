#import "/lib.typ": *

= Conditions <conditions>

Conditions are expressions which evaluate to booleans (the #ty[`Bool`] type).

Conditions include:

- the usual:
  - boolean literals (#expr[`true`], #expr[`false`])
  - short-circuiting logical operators (#op[`&&`], #op[`||`], #op[`!`])
  - non-short-circuiting ("bitwise") operators (#op[`&`], #op[`|`], #op[`^`])
  - comparison operators (#op[`==`], #op[`!=`], #op[`<`], #op[`<=`], #op[`>`], #op[`>=`])
- comparison chains
- the #op[`is`] operator
- the implication operator (#op[`=>`])

== Comparison Chains

In Vine, you can chain comparison operators.

For example, #expr[`a < b < c`] is equivalent to #expr[`(a < b) & (b < c)`].

More generally, #expr[`a() < b() < c()`] is equivalent to

```vi
do {
  let x = a();
  let y = b();
  let z = c();
  (x < y) & (y < z)
}
```

Note that comparison chains do not short-circuit;
  all of the subexpressions are evaluated exactly once.

All of the comparison operators can be chained.
As a contrived example, these are equivalent:

```vi
1 == 1 < 2 <= 3 > 0 != 5 // true
(1 == 1) & (1 < 2) & (2 <= 3) & (3 > 0) & (0 != 5) // true
```

== The #op[`is`] Operator <is>

The `is` operator checks if an expression matches some pattern, and returns a boolean.

```vi
let option = Some(1);
option is Some(_); // true
option is None; // false
```

Any variables bound by the patterns are in scope in subsequent *true-paths*
  including #op[`&&`] chains, then-blocks of an #vi[`if`], and the body of a #vi[`while`].

```vi
let option = Some(1);

option is Some(value) && value > 0; // true

if option is Some(value) {
  value; // 1
} else {
  // `value` is not bound in this scope
}
```

A true-path is broken by negation (#op[`!`]) and disjunction (#op[`||`]).

```vi
// invalid:
!(option is Some(value)) && value > 0

// invalid:
option is Some(value) || value > 0
```

== Implication

In logic, the statement "P implies Q" is true if, whenever P is true, Q is also true.
This is equivalent to "P is false, or Q is true".
Vine has an implication operator, #op[`=>`], with the same semantics.

```vi
true => true // true
true => false // false
false => true // true
false => false // true
```

The implies operator also continues the true-path;
  variables bound in the left-hand side are in scope in the right-hand side:

```vi
let x = Some(1);
x is Some(value) => value > 0 // true
x is Some(value) => value == 0 // false

let y = None;
y is Some(value) => value > 0 // true
y is Some(value) => value == 0 // true
```

A common pattern in other languages
  is to write `value == null || validate(value)`
  to validate a nullable value.
In Vine, this is written with the implication operator:

```vi
value is Some(value) => validate(value)
```
