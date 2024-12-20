<h1 align="center">
  <img src="https://vine.dev/favicon-1024.png" width="128" align="center">
  <p>The Vine Programming Language</p>
</h1>

Vine is an experimental new programming language based on interaction nets.

Vine is still under heavy development; there are many bugs, and many things will change.

See [`vine/examples/`](./vine/examples/) for examples of Vine.

```sh
cargo run -r --bin vine run vine/examples/$NAME.vi
```

If you're curious to learn more, join the [Vine Discord server](https://discord.gg/bgUPV8KjDv).

## Sub-Projects

Vine compiles to [Ivy](./ivy/), a low-level interaction-combinator programming language.

Ivy code runs on the [IVM](./ivm/), a performant interaction combinator runtime.
