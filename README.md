<h1 align="center">
  <img src="https://vine.dev/favicon-1024.png" width="128" align="center">
  
  The Vine Programming Language
</h1>

Vine is an experimental new programming language based on interaction nets.

Vine is still under heavy development; there are many bugs, and many things will change.

See [`vine/examples/`](./vine/examples/) for examples of Vine.

```sh
# to compile a vine program to ivy (vine's assembly language):
cargo run --bin vine vine/examples/$NAME.vi vine/std/std.vi > target/out.iv

# to run an ivy program:
cargo run --release --bin ivy target/out.iv
```

If you're curious to learn more, join the [Vine Discord server](https://discord.gg/bgUPV8KjDv).
