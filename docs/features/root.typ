#import "/lib.typ": *

= The Root Library

`#root` is Vine's standard library.
It provides:
- types fundamental to Vine,
  including @root-numeric-N32[`N32`],
  @root-IO[`IO`],
  @root-data-List[`List`],
  and @root-unicode-String[`String`]
- @root-ops[operator traits] which define the behavior
  of Vine's operators, including
  @root-ops-arithmetic-Add[`+`],
  @root-ops-bitwise-BitAnd[`&`],
  and @root-ops-Cast[`as`].
- the @root-prelude[prelude],
  which defines items that are in scope
  without needing to be explicitly imported
- a number of useful types and utilities, such as
  @root-data-Array[`Array`],
  @root-data-Map[`Map`],
  @root-debug[`debug`],
  and @root-rng[`rng`]

See the @root[API Docs for `#root`] for more info.



