#import "/lib.typ": *

= IVM Overview <ivm>

IVM is a performant interaction combinator runtime.
It is used to run Ivy programs
  (and thus also Vine programs, since they compile to Ivy).

To run an Ivy program on the IVM,
  each named global net is translated into a list of instructions for assembling the net
  (e.g. create a node, then link these two ports).
These instructions are used to perform @expand[Expand] interactions.

The IVM then boots, creating a single active pair
  between a global agent for the `::main` net, and an @extrinsic-io[`IO` extrinsic value].

The IVM then enters its main loop to perform interactions.
The first interaction will always be
  an Expand interaction between the `::main` agent and the `IO` value.
The IVM follows the instructions for assembling the `::main` net.
This creates more active pairs, which are continually reduced.
@call[Call] interactions may have side effects,
  allowing the net to interact with the outside world.
Once all interactions are performed,
  the net will be in normal form.
Since the starting net had no free ports,
  if no vicious circles are formed,
  the final net will empty.

== Parallel Evaluation

The IVM can also be run in a parallel mode.
In parallel mode, after boot, the IVM spawns the configured number of *worker* threads.
The main thread then assumes the role of the *dispatch* thread.

The worker threads are responsible for performing interactions,
  whilst the dispatch thread distributes active pairs among the workers.
Each worker has two buffers of active pairs:
  one that it is currently reducing,
  and one that is shared with the dispatch thread.
This shared buffer is used to
  pass work from the dispatch to the worker (if the worker is idle)
  or from the worker to the dispatch (if the worker is overloaded).
