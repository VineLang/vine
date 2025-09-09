# Architecture

The **cli** is the entrypoint to the compiler, and collects all of the
compilation options. Most notably, it creates a list of *entrypoints*, paths to
top-level modules (the main module, the standard library, and any third-party
libraries).

The **loader** then takes this list of entrypoints and queries the file system
for their contents. It invokes the *parser* (which in turn invokes the *lexer*)
to parse the file into an *AST*. The loader then finds any files included in
this AST, and recursively loads them. Once this process is complete, the loader
returns an AST node representing the entire compilation unit.

The **charter** then takes the AST and uses it to build the *chart*, a directory
of all of the *definitions* in the program. Definitions can have different kinds
of *bindings*; each definition can be bound to a value, pattern, type, trait,
and/or implementation. A definition can also have *members*; e.g. a module will
have a member for every item within it. The chart also contains AST nodes for
various kinds of items, including constants, functions, traits, etc.

The **resolver** then passes over the chart, resolving the AST nodes into *Typed
Intermediate Representation*. TIR is fully typed, so the resolver performs type
inference and type checking. TIR also refers to items by their ids, rather than
paths, so the resolver finds the referent of every path and reports any errors.

The **finder** is used by the resolver, as well as later stages, to finding
methods and implementations when they are not explicitly specified. It does this
by iterating over all candidates and recursively finding implementation
arguments for each.

The **distiller** then distills TIR into *Vine Intermediate Representation*. VIR
is much simpler than the AST or TIR; compound expressions are distilled into
sequential, atomic steps, and high-level control-flow constructs are distilled
into a [*Stacked Flow Graph*](./sfg). The distiller is also responsible for
checking the forms of expressions and patterns, inserting coercions, and
reporting resulting errors.

The **normalizer** then transforms the VIR to remove all of the *divergence*
from the SFG.

The **analyzer** then performs various analyses on the VIR, including
reachability analysis, and dataflow analysis (determining which locals are used
by which *stages*, and how).

The **specializer** computes the set of *specializations*, instantiations of a
given *fragment* (an item that emits code; i.e. a constant or function) with a
given set of implementation arguments.

The **emitter** then converts each specialization of the VIR into a collection
of *Ivy nets*, one for each stage.
