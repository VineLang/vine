# Hello, world!

Let's run our first Vine program!

```rs
// vine/examples/hello_world.vi

pub fn main(&io: &IO) {
  io.println("Hello, world!");
}
```

We can run this with:

```
vine run vine/examples/hello_world.vi
```

You should see something like the following:

```sh
Hello, world!

Interactions
  Total                  297
  Annihilate             159
  Commute                  0
  Copy                    17
  Erase                   34
  Expand                  46
  Call                    27
  Branch                  14

Memory
  Heap                   480 B
  Allocated            6_016 B
  Freed                6_016 B

Performance
  Time                     0 ms
  Speed            8_054_237 IPS
```

At the top, notice that it successfully printed `Hello, world!`.

At the bottom, you'll also see several [_statistics_](../ivy/statistics) printed
out. You don't need to worry about them for now; you can disable them with
`--no-stats` if you'd like.

\* If you want to disable them, you can use `--no-stats`.

Let's break down what just happened.

- `fn main`: we declare a function, named `main`
- `pub`: it should be public
- `(&io: &IO)`: it takes a single argument:
  - `&IO`: of type `&IO`
  - `io`: we bind it to the variable `io`
- `{ ... }`: in the body of the function:
  - `io.println`: we call the `println` method on `IO`
  - `"Hello, world!"`: we pass it the string `Hello, world!`

\*

- `fn main`: declares a function, named `main`
  - `pub`: makes it public
  - `(&io: &IO)`: it takes a single parameter:
    - `&IO`: of type `&IO`
    - `io`: to be bound to the variable `io`
  - `{ ... }`: in the function body
    - `io.println`: uses the `println` method on `IO`
    - `("Hello, world!")`: calls it with the string `Hello, world!`

Every Vine program will contain a `main` definition of this form.

\* Every Vine program must contain a `pub main(&io: &IO){}` function.
