
fun inorderRankUppass tree =
let
  fun makeData lchild rchild myData =
  let
    val (llsum, lrsum, _) = case lchild of
                                 Leaf(data) => data
                               | Node(_, data, _) => data
    val (rlsum, rrsum, _) = case rchild of
                                 Leaf(data) => data
                               | Node(_, data, _) => data
  in
    (llsum + lrsum + 1, rlsum + rrsum + 1, 0)
  end
in
  uppass tree makeData
end

(*
* the type 'a will be (sum of left subtree, sum of right subtree, inorder rank)
*)


(*
* The recursive invariant is that the inorder rank has already been computed
* for the parent. 
*)
fun inorderRankDownpass tree =
let
  fun makeDataLChild myData parentData =
  let
    val numAboveAndLeft = #3 parentData - #1 parentData
  in
    (#1 myData, #2 myData, #1 myData + numAboveAndLeft)
  end

  fun makeDataRChild myData parentData =
  let
    val numAboveAndLeft = #3 parentData + 1
  in
    (#1 myData, #2 myData, #1 myData + numAboveAndLeft)
  end

  val nullData = (0,0,0)
in
  downpass makeDataLChild makeDataRChild tree nullData
end

fun inorderRank2 tree =
let
  val tree' = inorderRankUppass tree
in
  inorderRankDownpass tree'
end

(*
*
* below is the hard-coded implementation of in order rank, without the
* abstraction of using up-passes and down-passes
*)

fun getChildSums tree =
  case tree of
       Leaf(data) => Leaf(data)
     | Node(lchild, _, rchild) =>
         let
           val lchild' = getChildSums lchild
           val rchild' = getChildSums rchild
           val (llsum, lrsum, _) = case lchild' of
                                        Leaf(data) => data
                                      | Node(_, data, _) => data
           val (rlsum, rrsum, _) = case rchild' of
                                        Leaf(data) => data
                                      | Node(_, data, _) => data
           val data' = (llsum + lrsum + 1, rlsum + rrsum + 1, 0)
         in
           Node(lchild', data', rchild')
         end


fun getInorderRank tree =
let
  val tree' = getChildSums tree
  fun inorderRankRec tree numAboveAndLeft =
    case tree of
         Leaf(_) => Leaf((0,0,numAboveAndLeft))
       | Node(lchild, data, rchild) =>
           let
             val rchildNumAboveAndLeft = (#1 data) + numAboveAndLeft + 1
             val (lchild', rchild') = ForkJoin.par (fn _ => inorderRankRec lchild
             numAboveAndLeft, fn _ => inorderRankRec rchild
             rchildNumAboveAndLeft)
             val data' = (#1 data, #2 data, (#1 data) + numAboveAndLeft)
           in
             Node(lchild', data', rchild')
           end
in
  case tree' of
       Leaf(_) => Leaf((0,0,0))
     | Node(lchild, data, rchild) =>
         let
           val (lchild', rchild') = ForkJoin.par (fn _ => inorderRankRec lchild 0,
           fn _ => inorderRankRec rchild (#1 data + 1))
           val data' = (#1 data, #2 data, #1 data)
         in
           Node(lchild', data', rchild')
         end
end

