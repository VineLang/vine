
const vscode = require("vscode")
const { LanguageClient } = require("vscode-languageclient/node")

/** @type {LanguageClient} */
let client;

module.exports = { activate, deactivate }

/** @param {vscode.ExtensionContext} context */
async function activate(context) {
  context.subscriptions.push(vscode.commands.registerCommand("vine.restartServer", restartServer))

  const config = vscode.workspace.getConfiguration("vine");

  const entrypoints = config.get("entrypoints");
  const cli = config.get("cli");
  const lspOptions = config.get("lspOptions");

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

