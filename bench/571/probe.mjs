// ^ticket(571): each of ^report(28)'s traps, run on a Fumola wasm build: node probe.mjs <bindings dir>
import fs from "node:fs"; import path from "node:path";
const dir = path.resolve(process.argv[2]);
const F = await import(path.join(dir, "fumola_wasm.js"));
F.initSync({ module: fs.readFileSync(path.join(dir, "fumola_wasm_bg.wasm")) });
const cases = [
  ["text !=, different", `"a" != "b"`], ["text !=, same", `"a" != "a"`],
  ["text pattern", `switch ("E") { case "E" { 1 }; case _ { 0 } }`],
  ["bool pattern", `switch (true) { case true { 1 }; case _ { 0 } }`],
  ["Nat - bigger Nat", `3 - 5`], ["Int + Nat", `(3 - 5) + 1`], ["Nat + Int", `1 + (3 - 5)`],
  ["Int <= Nat", `(3 - 5) <= 1`], ["Int == Nat, same number", `(5 - 1) == 4`],
  ["Float <", `0.25 < 0.5`], ["Float >=", `0.25 >= 0.5`],
  ["text escape \\\"", `"a\\"b"`], ["text escape \\n", `"a\\nb"`],
  ["variant, one tuple payload", `#G(1, 2)`], ["variant, double parens", `#G((1, 2))`],
  ["text <", `"a" < "b"`],
];
for (const [what, text] of cases) {
  const id = F.fumola_create(); let r;
  try { r = JSON.parse(F.fumola_eval_top(id, text)); } catch (e) { r = { ok: false, error: String(e) }; }
  console.log(what.padEnd(30), text.padEnd(46), r.ok ? JSON.stringify(r.value ?? r).slice(0, 110) : "ERROR " + JSON.stringify(r.error || r.kind || r).slice(0, 120));
}
