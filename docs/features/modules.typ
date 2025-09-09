# Modules

Vine programs are structured into *modules*. Every Vine file is a module.
Modules consist of a collection of *items*.

```rs
// main.vi

// function item named `main`
pub fn main(&io: &IO) {
  io.println("Hello, world!");
}

// constant item named `answer`
const answer: N32 = 42;
```

Every item has a *visibility*, which determines where they can be accessed. By
default, items are private, and can only be accessed within the module they were
defined in. Items can be made public with the `pub` keyword; public items can be
accessed anywhere.

Modules can have *submodules*.

```rs
// main.vi

mod messages {
  pub const greeting: String = "Hello, world!";
  pub const farewell: String = "Goodbye, world!";
}

pub fn main(&io: &IO) {
  io.println(messages::greeting);
  io.println(messages::farewell);
}
```

Submodules can be included from a separate file:

```rs
// messages.vi

pub const greeting: String = "Hello, world!";
pub const farewell: String = "Goodbye, world!";
```

```rs
// main.vi

mod messages = "./messages.vi";

pub fn main(&io: &IO) {
  io.println(messages::greeting);
  io.println(messages::farewell);
}
```

Items from other modules can be imported with a `use` item:

```rs
// main.vi

mod messages = "./messages.vi";

use messages::{greeting, farewell};

pub fn main(&io: &IO) {
  io.println(greeting);
  io.println(farewell);
}
```
