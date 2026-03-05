#### LazyList.LazyListCell.fumola-type-listing ####
```
type LazyListCell =  {
  element : X;
  symbol : Symbol;
  next : LazyList_<X>;
}
```

#### LazyList.LazyList.fumola-type-listing ####
```
type LazyList = ?LazyListCell<X>
```

#### LazyList.LazyList_.fumola-type-listing ####
```
type LazyList_ = Pointer<Thunk<LazyList<X>>>
```

#### LazyList.merge.fumola-func-listing ####
```
func merge(xs: LazyList<X>, ys: LazyList<X>) : LazyList<X> {
  switch (xs, ys) {
    case (_, null) xs;
    case (null, _) ys;
    case (?xc, ?yc)
    {
      Counters.inc`merge;
      if (xc.element <= yc.element) {
        ?{
          element=xc.element;
          symbol=xc.symbol;
          next=xc.symbol := thunk { merge(force (xc.next), ?yc) }
        }
      } else {
        ?{
          element=yc.element;
          symbol=yc.symbol;
          next=yc.symbol := thunk { merge(?xc, force (yc.next)) }
        }
      }
    };
  }
}
```

#### LazyList.takeAll.fumola-func-listing ####
```
func takeAll(l: LazyList<X>) : [(Symbol, X)] {
  switch (force l) {
    case null [];
    case (?lc) { [(lc.symbol, lc.element)] # takeAll(lc.next) }
  }
}
```

#### LazyList.takeN.fumola-func-listing ####
```
func takeN(l: LazyList<X>, len: Nat) : [(Symbol, X)] {
  if (len == 0) { return [] };
  switch (force l) {
    case null [];
    case (?lc) { [(lc.symbol, lc.element)] # takeN(lc.next, len - 1) }
  }
}
```

#### LazyList.takeOne.fumola-func-listing ####
```
func takeOne(l: LazyList<X>) : [(Symbol, X)] {
  switch (force l) { case null []; case (?lc) { [(lc.symbol, lc.element)] } }
}
```

#### List.fromIter.fumola-func-listing ####
```
func fromIter(iter: Iter<(Symbol, X)>) : List<X> {
  switch (iter.next()) {
    case null null;
    case (?(symbol, element))
    { ?{symbol=symbol; element; next=fromIter(iter)} }
  }
}
```

#### List.List.fumola-type-listing ####
```
type List = ? {
  element : X;
  symbol : Symbol;
  next : Pointer<List<X>>
}
```

#### SeqTree.Binary.fumola-type-listing ####
```
type Binary =  {
  symbol : Symbol;
  level : Nat;
  left : SeqTree_<X>;
  right : SeqTree_<X>
}
```

#### SeqTree.Element.fumola-type-listing ####
```
type Element =  {
  element : X;
  symbol : Symbol
}
```

#### SeqTree.fromList.fumola-func-listing ####
```
func fromList(list: List<X>) : ?SeqTree_<X> {
  do ? {
    do within space `SeqTreeFromList{
      let {symbol; element; next} = list!;
      let level = prim "symbolLevel"symbol;
      let (tree, nil) = fromListRec(
        next,
        symbol - `element := #element {symbol=symbol; element},
        null
      );
      assert (nil == null);
      tree
    }
  }
}
```

#### SeqTree.fromListRec.fumola-func-listing ####
```
func fromListRec(list: List<X>, left: SeqTree_<X>, parentLevel: ?Nat) : (
  SeqTree_<X>,
  List<X>
) {
  switch list {
    case null (left, null);
    case (?{symbol; element; next})
    {
      let level = prim "symbolLevel"symbol;
      if (levelLte(level, parentLevel)) {
        let rightLeft = symbol - `element := #element {
          symbol=symbol;
          element
        };
        let (right, next) = force (
          symbol - `rec1 := thunk { fromListRec(next, rightLeft, ?level) }
        );
        let left = symbol - `binary := #binary {
          symbol=symbol;
          level;
          left;
          right
        };
        force (
          symbol - `rec2 := thunk { fromListRec(next, left, parentLevel) }
        );
      } else { (left, list) }
    };
  }
}
```

#### SeqTree.lazyMergeSort.fumola-func-listing ####
```
func lazyMergeSort(t: SeqTree<X>) : LazyList_<X> {
  do within space `lazyMergeSort{
    `result := thunk {
      reduceUp(
        t,
        null,
        func (symbol: Symbol, element: X) : LazyList<X> {
          ?{symbol=symbol; element; next=nullThunkPointer()}
        },
        func (
          symbol: Symbol,
          xs: LazyList<X>,
          ys: LazyList<X>
        ) : LazyList<X> {
          do within space (`merge - symbol){
            LazyList.merge(xs, ys)
          }
        },
      )
    }
  }
}
```

#### SeqTree.levelLte.fumola-func-listing ####
```
func levelLte(level: Nat, parentLevel: ?Nat) : bool {
  switch parentLevel { case null true; case (?p) level <= p; }
}
```

#### SeqTree.metrics.fumola-func-listing ####
```
func metrics(tree: Pointer<SeqTree<X>>){
  switch (@ tree) {
    case (#empty) { {depth=0; size=0} };
    case (#element(e)) { {depth=1; size=1} };
    case (#binary(b))
    {
      let ml = metrics(b.left);
      let mr = metrics(b.right);
      {depth=max(ml.depth, mr.depth) + 1; size=ml.size + mr.size}
    };
  }
}
```

#### SeqTree.reduceUp.fumola-func-listing ####
```
func reduceUp(
  t: SeqTree<X>,
  empty: Y,
  element: (Symbol, X) -> Y,
  binary: (Symbol, Y, Y) -> Y
) : Y {
  switch t {
    case (#empty) empty;
    case (#element(e)) element(e.symbol, e.element);
    case (#binary(b))
    {
      force (
        b.symbol := thunk {
          binary(
            b.symbol,
            reduceUp(@ (b.left), empty, element, binary),
            reduceUp(@ (b.right), empty, element, binary)
          )
        }
      )
    };
  }
}
```

#### SeqTree.SeqTree.fumola-type-listing ####
```
type SeqTree = {
  #empty;
  #element : Element<X>;
  #binary : Binary<X>
}
```

#### SeqTree.SeqTree_.fumola-type-listing ####
```
type SeqTree_ = Pointer<SeqTree<X>>
```

