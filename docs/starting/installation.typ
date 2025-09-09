# Installation

Make sure you have [Rust](https://www.rust-lang.org/tools/install) installed.

```sh
# clone the Vine repository
git clone https://github.com/VineLang/vine
cd ./vine

# install the Vine cli
cargo install --path cli

# make sure everything is working
vine run vine/examples/hello_world.vi
```

You should see:

```
Hello, world!
```

Head over to the next page to learn more about what just happened.

## Optional: Vine VSCode Extension

Make sure you have [Node.js](https://nodejs.org/en/download/package-manager)
installed.

```sh
cd lsp/client
npm i
```

Then, run "Developer: Install Extension from Location..." and select the
`lsp/client` directory.

You can check that it's working by opening one of the example files and making a
syntax error. When you save, you should see an error appear.

## Optional: Vine Workspace Configuration

### LSP

```json
// .vscode/settings.json
{
  // list all of your top-level Vine programs (globs are supported)
  "vine.entrypoints": ["src/main.vi"]
}
```

Make sure you reload the window after changing this file.

### Formatter

Make sure you have [`dprint`](https://dprint.dev/install/) installed.

```json
// dprint.json
{
  "exec": {
    "commands": [
      {
        "exts": ["vi"],
        "command": "vine fmt"
      }
    ]
  },
  "plugins": ["https://plugins.dprint.dev/exec-0.5.0.json"]
}
```
