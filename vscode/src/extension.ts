import * as vscode from "vscode";
import {
  FumolaSemanticTokensProvider,
  legend
} from "./semanticTokens";

export function activate(context: vscode.ExtensionContext) {
  const provider = new FumolaSemanticTokensProvider();

  context.subscriptions.push(
    vscode.languages.registerDocumentSemanticTokensProvider(
      { language: "fumola" },
      provider,
      legend
    )
  );
}

export function deactivate() {}