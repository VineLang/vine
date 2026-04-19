#import "/lib.typ": *

= Ivy and the IVM

*Note:* These docs are significantly out of date; Ivy and the IVM have been significantly reworked since these were last updated. See #link("https://github.com/VineLang/vine/issues/544")[\#544] for more information.

#t.hr()

Vine compiles to @ivy[Ivy], a low-level interaction-combinator programming language.

Ivy code runs on the @ivm[IVM], a performant interaction combinator runtime.

#let children = (
  "ivy.typ",
  "interaction_system.typ",
  "ivm.typ",
  "extrinsics.typ",
  "statistics.typ",
)
