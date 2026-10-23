import fs from "node:fs"; import path from "node:path";
const dir = path.resolve(process.argv[2]);
const F = await import(path.join(dir, "fumola_wasm.js"));
F.initSync({ module: fs.readFileSync(path.join(dir, "fumola_wasm_bg.wasm")) });
const cases = [
  ["Int(4) == Nat(4)", `((2 - 6) * (0 - 1)) == 4`], ["Int(4) value", `(2 - 6) * (0 - 1)`],
  ["payload ends in a variant, single", `#G(1, 2, #D)`], ["payload ends in a variant, double", `#G((1, 2, #D))`],
  ["match a tuple payload, single", `switch (#G((1, 2))) { case (#G(a, b)) { a + b }; case _ { 0 } }`],
  ["match a tuple payload, double", `switch (#G((1, 2))) { case (#G((a, b))) { a + b }; case _ { 0 } }`],
  ["func returns a variant, single", `func f(c, r) { #G(c, r, #D) }; f(1, 2)`],
  ["Int pattern", `switch (0 - 1) { case (-1) { 1 }; case _ { 0 } }`],
  ["Float pattern", `switch (0.5) { case 0.5 { 1 }; case _ { 0 } }`],
];
for (const [what, text] of cases) {
  const id = F.fumola_create(); let r;
  try { r = JSON.parse(F.fumola_eval_top(id, text)); } catch (e) { r = { ok: false, error: String(e) }; }
  console.log(what.padEnd(36), text.padEnd(70), r.ok ? JSON.stringify(r.value ?? r).slice(0, 150) : "ERROR " + JSON.stringify(r.error || r.kind || r).slice(0, 140));
}
