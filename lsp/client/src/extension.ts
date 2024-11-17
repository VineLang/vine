
import {
 ExtensionContext,
 window,
 commands,
 workspace,
} from "vscode";

import {
 Executable,
 LanguageClient,
 LanguageClientOptions,
 ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export async function activate(context: ExtensionContext) {
  context.subscriptions.push(commands.registerCommand("vine.restartServer", restartServer))

  const config = workspace.getConfiguration("vine");

  const entrypoints = config.get("entrypoints") as string[];

  const outputChannel = window.createOutputChannel("Vine LSP");

  const run: Executable = {
    command: "cargo",
    args: ["run", "--bin", "vine-lsp", "--", ...entrypoints],
    options: {},
  };

  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };

  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "vine" }],
    outputChannel,
  };

  client = new LanguageClient("vine-language-server", "Vine LSP", serverOptions, clientOptions);
  client.start();

  async function restartServer() {
    await client.restart()
  }
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}

