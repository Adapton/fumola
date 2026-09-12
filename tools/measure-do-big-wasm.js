// `do big` against `do small`, in wasm, from node. Run by measure-do-big-wasm.sh,
// which builds the runtime and generates the bindings this requires beside it.
//
// Two tables. The first is the control from benches/do_big.rs: a loop the
// recursive evaluator runs entirely, and the same recursion once reachable and
// once behind a force. The second is mergeSort at four sizes, loading the module
// tree embedded in the wasm the way the playground does.
//
// Step counts are read back through fumola_steps_taken and asserted equal
// between the modes before any time is printed. A timing comparison between two
// runs that did different amounts of work would be meaningless, so a mismatch
// exits non-zero rather than printing a ratio.
//
// `same` is the noise floor: the same program in the same mode, timed again.
// Per issue #68 a wasm instance's memory is never reclaimed within one process,
// so later runs can be slower than earlier ones for reasons that have nothing
// to do with the mode; the floor is what makes that visible. A ratio no further
// from 1.00 than its floor is not a result.
const F = require('./fumola_wasm.js');

const RUNS = 3;

function fresh(withMergeSort) {
  const id = F.fumola_create();
  F.fumola_realize(id);
  const m = JSON.parse(F.fumola_ensure_mode(id, 'graphical'));
  if (!m.ok) throw new Error('ensure_mode: ' + JSON.stringify(m));
  if (withMergeSort) {
    const imp = JSON.parse(F.fumola_eval_top(id, 'import M "fumola/examples/mergeSort/mergeSort";'));
    if (!imp.ok) {
      console.error('import failed:', JSON.stringify(imp));
      console.error('modules:', F.fumola_modules());
      process.exit(1);
    }
  }
  return id;
}

function timeMin(program, withMergeSort) {
  let best = Infinity, steps = 0;
  for (let i = 0; i < RUNS; i++) {
    const id = fresh(withMergeSort);
    const before = F.fumola_steps_taken(id);
    const t0 = process.hrtime.bigint();
    const out = JSON.parse(F.fumola_eval_top(id, program));
    const t1 = process.hrtime.bigint();
    if (!out.ok) { console.error('eval failed:', JSON.stringify(out)); process.exit(1); }
    steps = F.fumola_steps_taken(id) - before;
    best = Math.min(best, Number(t1 - t0) / 1e6);
    F.fumola_drop(id);
  }
  return { ms: best, steps };
}

function table(title, rows, withMergeSort) {
  console.log('\n' + title + '\n');
  console.log('workload'.padEnd(22) + 'steps'.padStart(10) + 'small (ms)'.padStart(12)
    + 'big (ms)'.padStart(12) + 'big'.padStart(8) + 'same'.padStart(8));
  console.log('-'.repeat(72));
  for (const [name, body] of rows) {
    const small = timeMin(`do small { ${body} }`, withMergeSort);
    const big = timeMin(`do big { ${body} }`, withMergeSort);
    const again = timeMin(`do small { ${body} }`, withMergeSort);
    if (small.steps !== big.steps) {
      console.error(`${name}: step counts differ (${small.steps} vs ${big.steps}); timing is meaningless`);
      process.exit(1);
    }
    console.log(name.padEnd(22) + String(small.steps).padStart(10)
      + small.ms.toFixed(1).padStart(12) + big.ms.toFixed(1).padStart(12)
      + (big.ms / small.ms).toFixed(2).padStart(8) + (again.ms / small.ms).toFixed(2).padStart(8));
  }
}

const fib = 'func fib(n : Nat) : Nat { if (n < 2) n else fib(n - 1) + fib(n - 2) }; ';
table(`wasm32, node ${process.version}, graphical mode -- control`, [
  ['loop, 0 reads', 'var i = 0; var s = 0; while (i < 20000) { s := s + i * 2; i := i + 1 }; s'],
  ['fib 22', fib + 'fib(22)'],
  ['fib 22, in a force', fib + 'force (thunk { fib(22) })'],
], false);

table('mergeSort', [8, 16, 23, 44].map(size => [
  `scene, size ${size}`,
  `let r = M.generateSceneFullDemand(10, ${size}, null); r.sceneData`,
]), true);

console.log('\nstep counts asserted equal between the modes. `same` is the noise floor.\n');
