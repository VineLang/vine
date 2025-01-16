# Ivy and the IVM

Vine compiles to [Ivy](https://github.com/VineLang/vine/tree/main/ivy), a
low-level interaction-combinator programming language.

Ivy code runs on the [IVM](https://github.com/VineLang/vine/tree/main/ivm), a
performant interaction combinator runtime.

## Ivy Overview

An Ivy program consists of a series of named global nets. The name of a global
net must start with `::`, and may contain arbitrary identifier characters or
additional `::`s (e.g. `::foo::bar`).

Ivy nets are specified with a syntax based on the
[interaction calculus](https://en.wikipedia.org/wiki/Interaction_nets#Interaction_calculus);
each net has a root tree, attached to its singular free port; any number of
pairs of trees; and a wiring specified by pairs of variables.

```rs
// definition of the net `::main` (which is the entrypoint of the program)
::main {
  fn(io _) // <-- the root tree, a combinator with label `fn`
  // ^  ^-- eraser node
  // '----- a variable, representing one half of a wire
  io = @io_print_char(::char::i @io_print_char(::char::v _))
  // ^-- pair         ^^^^^^^^^                ^^^^^^^^^
  //             global net reference    extrinsic function node
}

// more global net definitions; here serving the role of constants
::char::i { 105 }
::char::v { 118 }
::char::nl { 10 }
//           ^^-- external value node
```

See [Ivy's Interaction System](./interaction-system.md).
