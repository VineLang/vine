# Ivy

Ivy is a low-level interaction-combinator programming language; the interaction-net equivalent of
assembly.

See [`./examples/`](./examples/) for examples of Ivy.

```sh
cargo run -r --bin ivy run ivy/examples/$NAME.iv
```

## Language Overview

An Ivy program consists of a series of named global nets. The name of a global net must start with
`::`, and may contain arbitrary identifier characters or additional `::`s (e.g. `::foo::bar`).

Ivy nets are specified with a syntax based on the
[interaction calculus](https://en.wikipedia.org/wiki/Interaction_nets#Interaction_calculus); each
net has a root tree, attached to its singular free port, and any number of pairs of trees, and a
wiring specified by pairs of variables.

```rs
// definition of the net `::main` (which is the entrypoint of the program)
::main {
  fn(io _) // <-- the root tree, a combinator with label `fn`
  // ^  ^-- eraser node
  // '----- a variable, representing one half of a wire
  io = @io_print_char(::char::h @io_print_char(::char::i @io_print_char(::char::nl _)))
  // ^-- pair                   ^-- external function node              ^-- reference to a global net
}

// more global net definitions; here serving the role of constants
::char::h { 104 }
::char::i { 105 }
::char::nl { 10 }
//            ^-- external value nodes
```

### Node Types

#### Combinators

The primary kind of node in Ivy is a _combinator_, denoted `label(left right)` for any identifier
`label` and children `left` and `right`. Each label defines a separate node type; the identifier is
arbitrary otherwise.

When two combinators with the same label `l` interact, they _annihilate_:

```
l(a0 a1) = l(b0 b1)
-------------------
a0 = b0
a1 = b1
```

Otherwise, when a combinator with label `l` interacts with any other type of node `t`, they
_commute_:

```
l(a0 a1) = t(b0 b1 ... bN)
--------------------------
a0 = t(w00 w01 ... w0N)
a1 = t(w10 w11 ... w1N)
b0 = l(w00 w10)
b1 = l(w01 w11)
...
bN = l(w0N w1N)
```

#### Erasers

An eraser node, written `_`, has no children, and when it interacts with any other node, erases each
of its children.

```
_ = t(x0 x1 ... xN)
-------------------
x0 = _
x1 = _
...
xN = _
```

#### Global Net References

These nilary nodes are observationally equivalent to their corresponding net definitions.

[TODO: explain in more detail]

#### External Values & Functions

These are extensions to the base interaction combinator system that allow using native integers,
floats, and IO.

[TODO: explain in more detail]
