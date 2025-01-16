# Stacked Flow Graphs

Many compilers use Control Flow Graphs (CFGs) to represent the control flow of a
function. The Vine compiler uses Stacked Flow Graphs (SFGs), an extension of
CFGs, a representation that allows for greater parallelization (among other
benefits).

## Layers, Stages, and Steps

An SFG consists of a forest of *layers*. Each layer is itself analogous to a
CFG, and consists of several *stages* (analogous to basic blocks in a CFG).

Flow begins in the initial stage of the initial layer. Flow can transfer between
layers. Once flow exits a layer, flow returns to the previous layer. This is
analogous to a call stack (hence the "stacked" flow graph).

Each stage contains a series of *steps*. Flow visits each step in a stage
sequentially.

A step can:

- *invoke* a variable
- *transfer* to another layer
- *diverge* to an ancestor layer
- perform a primitive operation, such as constructing a tuple, or calling a
  function

After the last step of a stage, if the stage has a *terminator*, flow transfers
to another stage in the same layer. Otherwise, flow exits the layer.

## Interfaces and Transfers

Every stage has an *interface*. Within a layer, multiple stages may have the
same interface. Every interface has one or more stages; interfaces with exactly
one stage are *unconditional*.

A transfers specifies a *target* interface. If there are multiple stages with
that interface, the transfer must supply a *payload* which will determine which
stage is transferred to. The interface specifies the type of this payload, and
how it is used.

For example, a boolean interface has two stages. One stage is associated with a
payload of `true`; the other with `false`.

## Forestry and Divergence

Recall that layers are structured in a forest; the forest has one or more root
layers, and every non-root layer has a single parent layer.

When transferring to another layer (i.e. in a transfer step), the target must be
in either a root layer, or a child of the current layer.

A step can *diverge*, causing flow to immediately exit several layers. A diverge
step specifies a target ancestor layer, and optionally a stage in that layer.
Flow exits layers until it reaches the target ancestor layer. If a stage is
specified, flow transfers to that stage; otherwise, the target layer is exited
as well.

(Divergence is used to implement features like `return`, `break`, and
`continue`. In the 'call stack' analogy, divergence is analogous to throwing an
exception that is caught by a function higher up on the stack.)

In Vine, SFGs are *normalized* before being emitted. Normalization removes all
divergence by splitting stages at points where flow may diverge.
