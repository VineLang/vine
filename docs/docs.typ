#import "lib.typ": *

= The Vine Programming Language

Vine is an experimental new programming language based on interaction nets. #todo[explain interaction nets (somewhere)]

Vine is a multi-paradigm language,
  featuring seamless interop between functional and imperative patterns.

#todo[why vine?]

See
#link("https://github.com/VineLang/vine/tree/main/vine/examples/")[`vine/examples/`]
for examples of Vine.

```nu
cargo run -r --bin vine run vine/examples/$NAME.vi
```

If you're curious to learn more, join the
#link("https://discord.gg/bgUPV8KjDv")[Vine Discord server].

(Vine is still under heavy development; many things will change.)

#todo[discuss debug mode]

#let children = (
  "starting/starting.typ",
  "features/features.typ",
  "compiler/compiler.typ",
  "ivy_ivm/ivy_ivm.typ",
)

#todos

