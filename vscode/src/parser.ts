export type Node = {
  kind: string;
  start: number;
  length: number;
};

export function parse(text: string): { nodes: Node[] } {
  // 🔴 Replace with your real parser
  // Minimal fake example:

  const nodes: Node[] = [];

  const keywordRegex = /\b(type|func|public|force)\b/g;

  for (const match of text.matchAll(keywordRegex)) {
    nodes.push({
      kind: "Keyword",
      start: match.index!,
      length: match[0].length
    });
  }

  return { nodes };
}