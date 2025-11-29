#import "/lib.typ": *

= Modules

Vine programs are comrpised of _modules_.
Every Vine file is a module.
Modules consist of a collection of _items_.

```vi
// main.vi

// function item named `main`
pub fn main(&io: &IO) {
  io.println("Hello, world!");
}

// constant item named `answer`
const answer: N32 = 42;
```

Every item has a _visibility_, which determines where they can be accessed.
By default, items are private,
  and can only be accessed within the module they were defined in.
Items can be made public with the `pub` keyword;
  public items can be accessed anywhere.

== Submodules

Modules can have _submodules_.

```vi
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

A submodule can be included from a separate file by specifying its path:

```vi
// main.vi

mod messages = "./messages.vi";

pub fn main(&io: &IO) {
  io.println(messages::greeting);
  io.println(messages::farewell);
}
```

```vi
// messages.vi

pub const greeting: String = "Hello, world!";
pub const farewell: String = "Goodbye, world!";
```

It is idiomatic for modules which have children
  to have a path `<name>/<name>.vi`,
  and for their children to be at path `<name>/<child>.vi`
  (or `<name>/<child>/<child>.vi` if it also has children).
When this convention is followed,
  submodule paths can be omitted,
  and will be found automatically.

```vi
// main/main.vi

mod messages; // automatically refers to `main/messages.vi`

use messages::{greeting, farewell};

pub fn main(&io: &IO) {
  io.println(greeting);
  io.println(farewell);
}
```

```vi
// main/messages.vi

pub const greeting: String = "Hello, world!";
pub const farewell: String = "Goodbye, world!";
```

== Imports

Items from other modules can be imported with a `use` item:

```vi
// main.vi

mod messages = "./messages.vi";

use messages::{greeting, farewell};

pub fn main(&io: &IO) {
  io.println(greeting);
  io.println(farewell);
}
```
