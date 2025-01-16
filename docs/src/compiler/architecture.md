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

The **resolver** then takes the AST and uses it to build a module graph. The
nodes of the module graph are called *definitions*. Definitions can have
different kinds of *bindings*, usually represented as AST nodes (taken out of
the original compilation unit AST). For example, an `fn` item will have a value
binding, and the parameters and body of the function will be represented as AST
nodes. The resolver also disambiguates certain AST nodes, e.g. differentiating
local variables from constants.

The **checker** then passes over the module graph and checks the types and forms
of all expressions. It also disambiguates AST nodes based on the inferred types
and forms; e.g. desugaring method calls and inserting explicit coercions.

The **distiller** then takes every value binding and distills the AST into *Vine
Intermediate Representation*. VIR is much simpler than the AST; compound
expressions are distilled into sequential, atomic steps, and high-level
control-flow constructs are distilled into a [*Stacked Flow Graph*](./sfg).

The **normalizer** then transforms the VIR to remove all of the *divergence*.

The **analyzer** then performs various analyses on the VIR, including
reachability analysis, and dataflow analysis (determining which locals are used
by which *stages*, and how).

The **emitter** then converts the VIR into a collection of *Ivy nets*.
