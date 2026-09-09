import sys
mode, size = sys.argv[1], sys.argv[2]
tail = {
  "baseline": '    ()\n  };\n  let l = run(null); let r = run(?rootOf);\n  "done"',
  "fumola":   '    A.Diff.nodeValsFromNodes(A.peekHistory().nodes)\n  };\n'
              '  let l = run(null); let r = run(?rootOf);\n'
              '  let d = A.Diff.nodeValsDiff(l, r);\n'
              '  (d.equal.size(), d.notEqual.size(), d.onlyLeft.size(), d.onlyRight.size())',
  "native":   '    A.Native.nodeValsNow()\n  };\n'
              '  let l = run(null); let r = run(?rootOf);\n'
              '  let c = A.Native.diff(l, r);\n'
              '  (c.equal, c.notEqual, c.onlyLeft, c.onlyRight)',
}[mode]
# The highest-level gap in range roots the input tree; removing it is the worst case, and the
# one every earlier table used.
print(f'''import A "fumola/system/adapton";
import M "fumola/collections/levelTree";
import List "fumola/collections/List";
import LazyList "fumola/collections/LazyList";
import R "fumola/collections/randomInput";
do {{
  let seed = 10;
  let size = {size};
  let rootOf = do {{
    var i = 2; var best = 2; var bestLevel = 0;
    while (i <= size) {{
      let l = prim "symbolLevel" i;
      if (l > bestLevel) {{ bestLevel := l; best := i }};
      i += 1
    }};
    best
  }};
  let run = func(remove : ?Symbol) {{
    A.reset();
    let array = R.generateRandomInput(seed, size);
    let list_ = `ListFromArray := thunk {{
      do within space `inputList {{ List.fromIter(array.vals()) }}
    }};
    let list = force list_;
    do ? {{
      let editor = List.Edit.cursor(list!.next);
      let editor = List.Edit.gotoSymbol(editor, remove!)!;
      List.Edit.remove(editor);
    }};
    let tree_ = `LevelTreeFromList := thunk {{
      do within space `inputTree {{ M.fromList(list) }}
    }};
    let tree = force tree_;
    let sorted_ = do within space `mergeSort {{ M.lazyMergeSort_(tree) }};
    let _ = LazyList.takeN_(sorted_, size);
{tail}
}}''')
