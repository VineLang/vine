#import "/lib.typ": *

= The REPL <repl>

Vine has a
  #link("https://en.wikipedia.org/wiki/Read-eval-print_loop")[REPL],
  launched with `vine repl`.

When you launch the REPL, it should look like this:
```vi
$ vine repl

let io: IO = <IO>;
> 
```

At the bottom is the prompt (`>`)
  where you can enter Vine code.
Above that is a listing of all of the variables in scope,
  with their types and values.
The session starts with a variable `io` of type `IO`
  containing an IO handle.

Entering an expression will evaluate it,
  print the result,
  and display another prompt.


```vi
$ vine repl

let io: IO = <IO>;
> 12 + 34
46

let io: IO = <IO>;
>
```

You can also enter statements:

```vi
$ vine repl

let io: IO = <IO>;
> let x = 12;

let io: IO = <IO>;
let x: N32 = 12;
> x += 34;

let io: IO = <IO>;
let x: N32 = 46;
> 
```

And you can use the IO handle:


```vi
$ vine repl

let io: IO = <IO>;
> io.println("Hello, world!");
Hello, world!

let io: IO = <IO>;
> let response = io.prompt("enter a response: ");
enter a response: Hello, REPL!

let io: IO = <IO>;
let response: Option[String] = Some("Hello, REPL!");
> 
```

The REPL also supports a few commands; you can type `/help` to see a list of commands.

You can use `/clear` to unbind local variables, ending their scope.

```vi
$ vine repl

let io: IO = <IO>;
> let x = 46;

let io: IO = <IO>;
let x: N32 = 46;
> /clear x

let io: IO = <IO>;
> 
```

You can pass `--lib <path>` to load libraries (for example, a Vine program you want to troubleshoot):

```vi
$ vine repl --lib vine/examples/fib_repl.vi

let io: IO = <IO>;
> use #fib_repl::fib;

let io: IO = <IO>;
> fib(7)
13
```

