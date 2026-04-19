import * as vscode from "vscode";
import { parse } from "./parser";

const tokenTypes = ["keyword", "type", "function"];
const tokenModifiers: string[] = [];

export const legend = new vscode.SemanticTokensLegend(
  tokenTypes,
  tokenModifiers
);

export class FumolaSemanticTokensProvider
  implements vscode.DocumentSemanticTokensProvider
{
  provideDocumentSemanticTokens(
    document: vscode.TextDocument
  ): vscode.ProviderResult<vscode.SemanticTokens> {
    const builder = new vscode.SemanticTokensBuilder(legend);

    const text = document.getText();
    const ast = parse(text);

    for (const node of ast.nodes) {
      const tokenType = mapKindToToken(node.kind);
      if (tokenType === -1) continue;

      const pos = document.positionAt(node.start);
      builder.push(
        pos.line,
        pos.character,
        node.length,
        tokenType,
        0
      );
    }

    return builder.build();
  }
}

function mapKindToToken(kind: string): number {
  switch (kind) {
    case "Keyword":
      return tokenTypes.indexOf("keyword");
    case "Type":
      return tokenTypes.indexOf("type");
    case "Function":
      return tokenTypes.indexOf("function");
    default:
      return -1;
  }
}