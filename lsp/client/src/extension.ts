import * as path from "path";
import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { Language, Parser, Query } from "web-tree-sitter";

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
      new TreeSitterSemanticTokensProvider(),
      legend,
    ),
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

class TreeSitterSemanticTokensProvider implements vscode.DocumentSemanticTokensProvider {
  async provideDocumentSemanticTokens(
    doc: vscode.TextDocument,
    _token: vscode.CancellationToken,
  ): Promise<vscode.SemanticTokens> {
    const source = doc.getText();

    const tree = parser.parse(source); // for MVP, no incremental updates

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
