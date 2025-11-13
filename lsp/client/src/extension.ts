import * as path from "path";
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
  "constant.character.escape": "string",
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

let parsed_trees: Map<string, Tree> = new Map();

// Use to signal to VS Code that semantic tokens change on each document edit.
// Otherwise VS Code bundles multiple edits / key strokes and only update the semantic token
// sporadically. Since we provide syntax highlighting throuhg the semantic tokens, we want them to
// update on each key stroke.
const changeEmitter = new vscode.EventEmitter<void>();

module.exports = { activate, deactivate };

async function activate(context: vscode.ExtensionContext) {
  // Start tree sitter syntax highlighting
  await Parser.init();

  const wasmPath = context.asAbsolutePath(
    path.join("..", "tree-sitter-vine", "tree-sitter-vine.wasm"),
  );
  language = await Language.load(wasmPath);

  parser = new Parser();
  parser.setLanguage(language);

  const highlightsPath = context.asAbsolutePath(
    path.join("..", "tree-sitter-vine", "queries", "highlights.scm"),
  );

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
    vscode.workspace.onDidOpenTextDocument(async (doc) => {
      if (doc.languageId !== "vine") return;
      parsed_trees.set(doc.uri.toString(), parser.parse(doc.getText()));
    }),
  );
  context.subscriptions.push(
    vscode.workspace.onDidCloseTextDocument(async (doc) => {
      if (doc.languageId !== "vine") return;
      parsed_trees.delete(doc.uri.toString());
    }),
  );
  context.subscriptions.push(
    vscode.workspace.onDidChangeTextDocument(async (e) => {
      if (e.document.languageId !== "vine") return;
      const doc = e.document;

      const uri = doc.uri.toString();
      let tree = parsed_trees.get(uri);

      if (tree === undefined) {
        tree = parser.parse(doc.getText());
      } else {
        applyChanges(tree, e.contentChanges);
        const newTree = parser.parse(doc.getText(), tree);
        tree.delete();
        tree = newTree;
      }
      parsed_trees.set(uri, tree);
      changeEmitter.fire();
    }),
  );

  // Start LSP
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

function deactivate() {
  return client?.stop();
}

function applyChanges(tree: Tree, changes: readonly vscode.TextDocumentContentChangeEvent[]) {
  // Apply changes from end to front of the file so that positions stay valid.
  for (
    const change of [...changes].sort((a, b) =>
      (b.range.start.line - a.range.start.line)
      || (b.range.start.character - a.range.start.character)
    )
  ) {
    const startIndex = change.rangeOffset;
    const oldEndIndex = change.rangeOffset + change.rangeLength;
    const newEndIndex = change.rangeOffset + change.text.length;

    const startPosition = {
      row: change.range.start.line,
      column: change.range.start.character,
    };
    const oldEndPosition = {
      row: change.range.end.line,
      column: change.range.end.character,
    };

    // Compute new end position from inserted text
    const newText = change.text;
    const newLines = newText.split(/\r\n|\r|\n/);

    let newEndRow: number;
    let newEndColumn: number;

    if (newLines.length === 1) {
      newEndRow = startPosition.row;
      newEndColumn = startPosition.column + newLines[0].length;
    } else {
      newEndRow = startPosition.row + newLines.length - 1;
      newEndColumn = newLines[newLines.length - 1].length;
    }

    const newEndPosition = { row: newEndRow, column: newEndColumn };

    tree.edit({
      startIndex,
      oldEndIndex,
      newEndIndex,
      startPosition,
      oldEndPosition,
      newEndPosition,
    });
  }
}

class TreeSitterSemanticTokensProvider implements vscode.DocumentSemanticTokensProvider {
  onDidChangeSemanticTokens: vscode.Event<void>;

  constructor(onDidChangeSemanticTokens: vscode.Event<void>) {
    this.onDidChangeSemanticTokens = onDidChangeSemanticTokens;
  }

  async provideDocumentSemanticTokens(
    doc: vscode.TextDocument,
    _token: vscode.CancellationToken,
  ): Promise<vscode.SemanticTokens> {
    let semanticTokens: vscode.SemanticTokens;

    let tree = parsed_trees.get(doc.uri.toString());
    if (tree === undefined) {
      const source = doc.getText();
      tree = parser.parse(source);
      parsed_trees.set(doc.uri.toString(), tree);
    }

    const builder = new vscode.SemanticTokensBuilder(legend);

    for (const { name, node } of highlightQuery.captures(tree.rootNode)) {
      const tokenType = capturesToTokenTypeIndex[name];
      if (tokenType === undefined) continue;

      const start = node.startPosition;
      const end = node.endPosition;

      const length = doc.offsetAt(new vscode.Position(end.row, end.column))
        - doc.offsetAt(new vscode.Position(start.row, start.column));

      builder.push(
        start.row,
        start.column,
        length,
        tokenType,
      );
    }

    return builder.build();
  }
}
