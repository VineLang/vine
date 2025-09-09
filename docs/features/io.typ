# IO

All IO in Vine is done by calling functions on an IO handle, passed in to `main`
at the start of the program. For example, here is a basic `cat` program:

```rs
use std::option::Option::Some;

pub fn main(&io: &IO) {
  while io.read_line() is Some(line) {
    io.println(line);
  }
}
```

Any function that needs to do IO must take a reference to this handle.

```rs
use std::option::Option::Some;

pub fn main(&io: &IO) {
  if io.prompt("What is your name? ") is Some(name) {
    greet(&io, name);
  }
}

fn greet(&io: &IO, name: String) {
  io.println("Hello, " ++ name ++ "!");
}
```

Any function that does not get passed a reference to an IO handle is *pure*
(i.e. does not interact with the outside world).

IO calls are asynchronous and do not block the execution of the rest of the
program.

Every IO function takes the IO handle by reference, and the IO handle is updated
after every low-level IO operation (like printing or reading a byte). This
ensures that the IO is executed in the expected order, despite being
asynchronous.
