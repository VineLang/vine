#import "/lib.typ": *

= Ivy's Interaction System <interaction-system>

Ivy's interaction system is based on the symmetric interaction combinators,
  with various extensions.

== Agent Types

=== Combinators

Ivy has a (theoretically) unlimited number of binary combinator agent types,
  each identified with a _label_.
Two combinators of the same label annihilate,
  whilst two combinators of different labels commute.

Ivy also has an eraser agent, which is a nilary combinator.

=== Globals

Ivy programs are structured as a collection of named _global nets_.
Each global net corresponds to a nilary _global agent_.
A global agent expands into the corresponding global net when necessary during interaction.

=== Extrinsics

_Extrinsic agents_ represent entities and operations external to the interaction net.

- _extrinsic values_ are nilary agents that represent external entities
- _extrinsic functions_ are binary agents that represent external operations
- _extrinsic branches_ are ternary agents that represent a boolean query on an external entity

Extrinsics are discussed in more detail on the @extrinsics[corresponding page].

== Interaction Rules

Ivy has 7 categories of interaction rules:

- Annihilate
- Commute
- Copy
- Erase
- Expand
- Call
- Branch

For combinator agents, the Annihilate, Commute, Copy, and Erase rules
  behave equivalently to the standard rules for symmetric interaction combinators.

=== Annihilate

When two non-nilary agents of the same type interact, they annihilate.
The wires previously connected to their auxiliary ports are linked together.

=== Commute

When two non-nilary agents of different types interact, they commute,
  analogously to interaction combinators.

=== Copy

When a nilary agent and a non-nilary agent of certain types interact,
  the nilary agent is copied to each of the non-nilary agent's auxiliary wires.
This happens when:

- the nilary agent is an eraser
- the nilary agent is an extrinsic value,
    and the non-nilary agent is a combinator
- the nilary agent is a global agent,
    and the non-nilary agent is a combinator
    of a label that does not appear in the global net
    (or any global net transitively referenced)

=== Erase

When two nilary agents interact, they are erased,
  unless one is an extrinsic value and the other is a global agent.

=== Expand <expand>

When a global agent interacts with another agent, it is expanded
  (unless the Copy or Erase rules above apply).
The global agent is simply replaced with the corresponding global net
  (and the other agent is untouched).

=== Call <call>

When an extrinsic value interacts with an extrinsic function,
  the associated operation is performed on the associated entity,
  and some extrinsic value is returned.

=== Branch

When an extrinsic value interacts with an extrinsic branch,
  the associated query is performed on the associated entity.
Based on the boolean result,
  one of the first two auxiliary wires is linked to the third,
  and the other is connected to an eraser.
