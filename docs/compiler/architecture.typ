#import "/lib.typ": *

= Architecture

The *cli* is the entrypoint to the compiler, and collects all of the
compilation options. Most notably, it creates a list of _entrypoints_, paths to
top-level modules (the main module, the standard library, and any third-party
libraries).

The *loader* then takes this list of entrypoints and queries the file system
for their contents. It invokes the _parser_ (which in turn invokes the _lexer_)
to parse the file into an _AST_. The loader then finds any files included in
this AST, and recursively loads them. Once this process is complete, the loader
returns an AST node representing the entire compilation unit.

The *charter* then takes the AST and uses it to build the _chart_, a directory
of all of the _definitions_ in the program. Definitions can have different kinds
of _bindings_; each definition can be bound to a value, pattern, type, trait,
and/or implementation. A definition can also have _members_; e.g. a module will
have a member for every item within it. The chart also contains AST nodes for
various kinds of items, including constants, functions, traits, etc.

The *resolver* then passes over the chart, resolving the AST nodes into _Typed
Intermediate Representation_. TIR is fully typed, so the resolver performs type
inference and type checking. TIR also refers to items by their ids, rather than
paths, so the resolver finds the referent of every path and reports any errors.

The *finder* is used by the resolver, as well as later stages, to finding
methods and implementations when they are not explicitly specified. It does this
by iterating over all candidates and recursively finding implementation
arguments for each.

The *distiller* then distills TIR into _Vine Intermediate Representation_. VIR
is much simpler than the AST or TIR; compound expressions are distilled into
sequential, atomic steps, and high-level control-flow constructs are distilled
into a @sfg[_Stacked Flow Graph_]. The distiller is also responsible for
checking the forms of expressions and patterns, inserting coercions, and
reporting resulting errors.

The *normalizer* then transforms the VIR to remove all of the _divergence_
from the SFG.

The *analyzer* then performs various analyses on the VIR, including
reachability analysis, and dataflow analysis (determining which locals are used
by which _stages_, and how).

The *specializer* computes the set of _specializations_, instantiations of a
given _fragment_ (an item that emits code; i.e. a constant or function) with a
given set of implementation arguments.

The *emitter* then converts each specialization of the VIR into a collection
of _Ivy nets_, one for each stage.
