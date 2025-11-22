import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { Language, Parser, Query, Tree } from "web-tree-sitter";

let client: LanguageClient;
let parser: Parser;
let language: Language;
let highlightQuery: Query;

const captureToTokenType: Record<string, string> = {
  "keyword": "keyword",
  "punctuation": "punctuation",
  "operator": "operator",
  "string": "string",
  "constant.numeric": "number",
  "constant.character.escape": "constant.character.escape",
  "comment": "comment",
  "function": "function",
  "namespace": "namespace",
  "type": "type",
};

const tokenTypes = Object.values(captureToTokenType);

const capturesToTokenTypeIndex = Object.fromEntries(
  Object.entries(captureToTokenType).map((
    [capture, tokenType],
  ) => [capture, tokenTypes.indexOf(tokenType)]),
);

const legend = new vscode.SemanticTokensLegend(Object.values(captureToTokenType));

let parsedTrees: Map<string, Tree> = new Map();

// Use to signal to VS Code that semantic tokens change on each document edit.
// Otherwise VS Code bundles multiple edits / key strokes and only update the semantic token
// sporadically. Since we provide syntax highlighting through the semantic tokens, we want them to
// update on each key stroke.
const changeEmitter = new vscode.EventEmitter<void>();

module.exports = { activate, deactivate };

async function activate(context: vscode.ExtensionContext) {
  activateLsp(context);
  await activateHighlighting(context);
}

function activateLsp(context: vscode.ExtensionContext) {
  context.subscriptions.push(vscode.commands.registerCommand("vine.restartServer", restartServer));

  const config = vscode.workspace.getConfiguration("vine");

  const entrypoints = config.get("entrypoints") as string[];
  const cli = config.get("cli") as string[];
  const lspOptions = config.get("lspOptions") as string[];

  const outputChannel = vscode.window.createOutputChannel("Vine LSP");

  const run = {
    command: cli[0],
    args: [...cli.slice(1), "lsp", ...lspOptions, ...entrypoints],
    options: {},
  };

  const serverOptions = {
    run,
    debug: run,
  };

  let clientOptions = {
    documentSelector: [{ scheme: "file", language: "vine" }],
    outputChannel,
  };

  client = new LanguageClient("vine-language-server", "Vine LSP", serverOptions, clientOptions);
  client.start();

  async function restartServer() {
    await client.restart();
  }
}

async function activateHighlighting(context: vscode.ExtensionContext) {
  await Parser.init();

  const wasmPath = context.asAbsolutePath("dist/tree-sitter-vine.wasm");
  language = await Language.load(wasmPath);

  parser = new Parser();
  parser.setLanguage(language);

  const highlightsPath = context.asAbsolutePath("dist/highlights.scm");

  const highlightsText = (await vscode.workspace.fs.readFile(
    vscode.Uri.file(highlightsPath),
  )).toString();

  highlightQuery = new Query(language, highlightsText);

  context.subscriptions.push(
    vscode.languages.registerDocumentSemanticTokensProvider(
      { language: "vine" },
      new TreeSitterSemanticTokensProvider(changeEmitter.event),
      legend,
    ),
  );

  // Tracking of files for incremental parsing
  context.subscriptions.push(
    vscode.workspace.onDidCloseTextDocument(async (doc) => {
      if (doc.languageId !== "vine") return;
      parsedTrees.delete(doc.uri.toString());
    }),
  );
  context.subscriptions.push(
    vscode.workspace.onDidChangeTextDocument(async (e) => {
      if (e.document.languageId !== "vine") return;
      const doc = e.document;

      const uri = doc.uri.toString();
      let tree = parsedTrees.get(uri);

      if (tree === undefined) {
        tree = parser.parse(doc.getText());
      } else {
        applyChanges(tree, e.contentChanges);
        const newTree = parser.parse(doc.getText(), tree);
        tree.delete();
        tree = newTree;
      }
      parsedTrees.set(uri, tree);
      changeEmitter.fire();
    }),
  );
}

function deactivate() {
  return client?.stop();
}

function applyChanges(tree: Tree, changes: readonly vscode.TextDocumentContentChangeEvent[]) {
  // Apply changes from end to front of the file so that positions stay valid.
  changes = [...changes].sort((a, b) =>
    (b.range.start.line - a.range.start.line)
    || (b.range.start.character - a.range.start.character)
  );

  for (const change of changes) {
    const insertedLines = change.text.split(/\r\n|\r|\n/);

    tree.edit({
      startIndex: change.rangeOffset,
      oldEndIndex: change.rangeOffset + change.rangeLength,
      newEndIndex: change.rangeOffset + change.text.length,
      startPosition: {
        row: change.range.start.line,
        column: change.range.start.character,
      },
      oldEndPosition: {
        row: change.range.end.line,
        column: change.range.end.character,
      },
      newEndPosition: {
        row: change.range.start.line,
        column: insertedLines.length === 1
          ? change.range.start.character + insertedLines[0].length
          : insertedLines[insertedLines.length - 1].length,
      },
    });
  }
}

class TreeSitterSemanticTokensProvider implements vscode.DocumentSemanticTokensProvider {
  constructor(public onDidChangeSemanticTokens: vscode.Event<void>) {}

  async provideDocumentSemanticTokens(
    doc: vscode.TextDocument,
    _token: vscode.CancellationToken,
  ): Promise<vscode.SemanticTokens> {
    let tree = parsedTrees.get(doc.uri.toString());
    if (tree === undefined) {
      const source = doc.getText();
      tree = parser.parse(source);
      parsedTrees.set(doc.uri.toString(), tree);
    }

    const builder = new vscode.SemanticTokensBuilder(legend);

    for (const { name, node } of highlightQuery.captures(tree.rootNode)) {
      const tokenType = capturesToTokenTypeIndex[name];
      if (tokenType === undefined) continue;

      const start = node.startPosition;
      const end = node.endPosition;

      const length = doc.offsetAt(new vscode.Position(end.row, end.column))
        - doc.offsetAt(new vscode.Position(start.row, start.column));

      builder.push(start.row, start.column, length, tokenType);
    }

    return builder.build();
  }
}
