# Variables

Variables can be declared using `let` statements, `fn` parameters, `match` arms,
and conditionals, using [variable patterns](./patterns.md).

### Shadowing

A variable can be shadowed by redeclaring it:

```
let x = 1;
x; // 1
let x = "abc"
x; // abc
```

```
fn foo( &io: &IO, x : N32 ){
  io.print( x.to_string() );
  io.print( " " );
  let x = 5
  io.print( x.to_string() );
}

foo( &io, 2); // prints: 2 5
```

A variable declared in a child block does not affect the variable in the parent
block:

```
let x = 10;
x; // 10

let y = do {
  let x = 5;
  x // 5
}
y; // 5

if( x == 10 ) {
  let x = 99;
  x; // 99
}
x; // 10
```
