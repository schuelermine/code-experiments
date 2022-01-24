with builtins;
let fold-shim = { _type = [ "magic" "fold-shim" ]; _empty = true; };
    skip-fold-shim = f: a: b: if a == fold-shim then b else f a b;
    sort-by = f: sort (a: b: f a < f b);
    maximum = a: b: if a > b then a else b;
    maximum-of-list = builtins.foldl' (skip-fold-shim maximum) fold-shim;
    replicate = n: a: if n <= 0 then [ ] else [ a ] ++ replicate (n - 1) a;
in rec { get-k-d-tree-from-point-set =
  { dimensions ? maximum-of-list point-set, 
    point-set ? [ ], cut-direction ? 0, default-coordinate ? 0 }: if length point-set != 0 then
    let get-coordinate = point: if length point >= cut-direction then elemAt point cut-direction else default-coordinate;
        point-count = length point-set;
        sorted-points = sort-by get-coordinate point-set;
        median-point = elemAt sorted-points (floor (point-count / 2));
        cut-position = get-coordinate median-point;
        remaining-points = filter (point: point != median-point) sorted-points;
        partitioned-points = partition (point: get-coordinate point > cut-position) remaining-points;
        next-cut-direction = if cut-direction + 1 == dimensions then 0 else cut-direction + 1;
    in { _type = [ "k-d-tree" "node" ];
         point = median-point;
         lt-leaf = get-k-d-tree-from-point-set {
           point-set = partitioned-points.wrong;
           cut-direction = next-cut-direction;
           inherit dimensions;
         };
         ge-leaf = get-k-d-tree-from-point-set {
           point-set = partitioned-points.right;
           cut-direction = next-cut-direction;
           inherit dimensions;
         };
         inherit dimensions cut-direction; }
    else { _type = [ "k-d-tree" "leaf"];
           _empty = true; };
           get-closest-point-using-k-d-tree =
  { k-d-tree, target-point ? replicate k-d-tree.dimensions 0 }: null; }
