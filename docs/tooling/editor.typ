#import "/lib.typ": *

= Editor Configuration <editor>

== VSCode

Install the #link("https://marketplace.visualstudio.com/items?itemName=VineLang.vine")[Vine extension] from the VSCode Marketplace.

For VSCode forks, check the latest version on the marketplace and download
```
https://marketplace.visualstudio.com/_apis/public/gallery/publishers/vinelang/vsextensions/vine/<VERSION>/vspackage
```
You can then install this with "Extensions: Install from VSIX".

=== LSP Configuration

```json
// .vscode/settings.json
{
  // list all of your top-level Vine programs (globs are supported)
  "vine.entrypoints": ["src/main.vi"]
}
```

Make sure you reload the window after changing this file.

== Helix

```toml
# ~/.config/helix/languages.toml

[[language]]
name = "vine"
scope = "source.vine"
file-types = ["vi"]
language-servers = ["vine"]
formatter = { command = "vine", args = ["fmt"] }
grammar = "vine"
auto-format = true
comment-tokens = ["//"]

[[grammar]]
name = "vine"
source.path = "/absolute/path/to/vine-repo/lsp/tree-sitter-vine/"
```

To build the tree sitter grammar,
  make sure you have #link("https://nodejs.org/en/download/package-manager")[Node.js] installed,
  and run

```bash
cd lsp/tree-sitter-vine
npm i
npx tree-sitter generate
hx --grammar build
mkdir -p ~/.config/helix/runtime/queries
ln -s /absolute/path/to/vine-repo/lsp/tree-sitter-vine/queries ~/.config/helix/runtime/queries/vine
```

=== LSP Configuration

```toml
# your-project/.helix/languages.toml

[language-server.vine]
command = "vine"
# list all of your top-level Vine programs (globs are supported)
args = ["lsp", "--", "src/main.vi"]
```
