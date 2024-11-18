
import * as vscode from "vscode"
import { LanguageClient } from "vscode-languageclient/node"

let client: LanguageClient;

module.exports = { activate, deactivate }

async function activate(context: vscode.ExtensionContext) {
  context.subscriptions.push(vscode.commands.registerCommand("vine.restartServer", restartServer))

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
    await client.restart()
  }
}

function deactivate() {
  return client?.stop();
}

