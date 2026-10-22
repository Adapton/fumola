// MEASURING ONLY (^ticket(555), branch bench/555-json): ^report(27)'s World rule in Fumola (wasm, node), the map passed
// as source text (fumola_eval_top) against passed as JSON (fumola_eval_top_with), with the held HashMap and the
// neighbours-only call beside them; median ms a call, Fumola's steps, answers checked.
//   node measure.mjs <bindings dir> <world.fum> <sizes> <out.json>
import fs from "node:fs";
import { heldSetup } from "./held.fum.js";
const [dir, src, sizesArg, out] = process.argv.slice(2);
const F = await import(dir + "/fumola_wasm.js");
F.initSync({ module: fs.readFileSync(dir + "/fumola_wasm_bg.wasm") });
const cmd = v => v.tag === "Variant" ? (v.value.value === null ? { tag: v.value.name } : { tag: v.value.name, value: cmd(v.value.value) })
  : v.tag === "Tuple" || v.tag === "List" ? v.value.map(cmd) : v.tag === "Int" ? +v.value : v.value;
const cellText = (c, r) => `{ col = ${c}; row = ${r}; kind = "grass"; ways = ["N", "E", "S", "W"] }`;
const cellJs = (c, r) => ({ col: c, row: r, kind: "grass", ways: ["N", "E", "S", "W"] });
const q = a => a.slice().sort((x, y) => x - y)[a.length >> 1];
const rows = [];
for (const n of sizesArg.split(",").map(Number)) {
  const id = F.fumola_create();
  const check = s => { const r = JSON.parse(s); if (!r.ok) throw new Error(JSON.stringify(r).slice(0, 300)); return r; };
  check(F.fumola_eval_top(id, fs.readFileSync(src, "utf8")));
  const ac = Math.floor(n / 2) - 1, ar = Math.floor(n / 2);
  const all = [], allJs = [], near = [], nearJs = [];
  for (let r = 0; r < n; r++) for (let c = 0; c < n; c++) {
    all.push(cellText(c, r)); allJs.push(cellJs(c, r));
    if (Math.abs(c - ac) <= 1 && Math.abs(r - ar) <= 1) { near.push(cellText(c, r)); nearJs.push(cellJs(c, r)); }
  }
  check(F.fumola_eval_top(id, heldSetup("[" + all.join(", ") + "]")));
  const want = JSON.stringify({ tag: "GoSquare", value: [ac + 1, ar, "W", { tag: "Done" }] });
  const g = cells => ({ at_col: ac, at_row: ar, cols: n, rows: n, cells });
  const variants = {
    passed_text: () => F.fumola_eval_top(id, `world_list({ at_col = ${ac}; at_row = ${ar}; cols = ${n}; rows = ${n}; cells = [${all.join(", ")}] }, #Way("E"))`),
    // the JSON written in the call, as a host would: JSON.stringify included in the time
    passed_json: () => F.fumola_eval_top_with(id, `world_list(g, #Way("E"))`, JSON.stringify({ g: g(allJs) })),
    near_text: () => F.fumola_eval_top(id, `world_list({ at_col = ${ac}; at_row = ${ar}; cols = ${n}; rows = ${n}; cells = [${near.join(", ")}] }, #Way("E"))`),
    near_json: () => F.fumola_eval_top_with(id, `world_list(g, #Way("E"))`, JSON.stringify({ g: g(nearJs) })),
    held_map: () => F.fumola_eval_top(id, `world_held_map(${ac}, ${ar}, ${n}, ${n}, "E")`),
  };
  const row = { n, cells: n * n };
  for (const [k, f] of Object.entries(variants)) {
    for (let i = 0; i < 3; i++) check(f());
    const ts = [], st = []; const t0 = performance.now(); let got;
    for (let i = 0; i < 60; i++) { const t = performance.now(); const r = check(f()); ts.push(performance.now() - t); st.push(r.counts.step); got = JSON.stringify(cmd(r)); if (performance.now() - t0 > 2500 && i >= 8) break; }
    if (got !== want) throw new Error(`${k} at ${n}: ${got}, wanted ${want}`);
    row[k] = +q(ts).toFixed(3); row[k + "_steps"] = q(st);
  }
  F.fumola_drop(id);
  console.log(JSON.stringify(row)); rows.push(row);
}
fs.writeFileSync(out, JSON.stringify(rows, null, 1));
