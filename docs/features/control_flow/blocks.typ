#import "/lib.typ": *

= Blocks

Control flow is structured with _blocks_,
  delimited with curly braces #vi[`{}`],
  which can contain statements,
  and may optionally end with an expression.

Blocks which end in an expression
  evaluate to the value of that expression.
For example, the body of a function is a block,
  and the return value of the function is the value
  that the block evalautes to.

```vi
fn add(a: N32, b: N32) -> N32 {
  a + b
}

add(1, 2) // 3
```

=== `do`

The #vi[`do`] expression allows embedding a block inside an expression.

```vi
let sum = do {
  let a = 1;
  let b = 2;
  a + b
};
sum // 3
```

