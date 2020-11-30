



datatype 'a Tree = Node of 'a Tree * 'a * 'a Tree | Leaf of 'a

(*
* the type 'a will be (sum of left subtree, sum of right subtree, inorder rank)
*
*
*)

fun makeBinaryTree depth = 
  case depth of
       0 => Leaf((0, 0, 0))
     | x =>
         let
           val data = (0, 0, 0)
         in
           Node(makeBinaryTree (x - 1), data, makeBinaryTree (x - 1))
         end

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

fun print3Tuple tuple =
let
  val (x, y, z) = tuple
in
  print ("(" ^ (Int.toString x) ^ " " ^ (Int.toString y) ^ " " ^ (Int.toString
  z) ^ ")")
end

fun printNTimes str n =
  case n of
       0 => ()
     | n' => (print str; printNTimes str (n' - 1))

fun printTree tree currDepth =
  case tree of
       Leaf(data) =>
         (printNTimes " " currDepth; print3Tuple data; print "\n")
     | Node(lchild, data, rchild) =>
         (printTree rchild (currDepth + 1);
         printNTimes " " currDepth; print3Tuple data; print "\n";
         printTree lchild (currDepth + 1))

fun getInorderRank tree =
let
  val tree' = getChildSums tree
  fun inorderRank tree numBeforeMe =
    case tree of
         Leaf(_) => Leaf((0,0,numBeforeMe))
       | Node(lchild, data, rchild) =>
           let
             val rchildNumBefore = (#1 data) + numBeforeMe + 1
             val (lchild', rchild') = ForkJoin.par (fn _ => inorderRank lchild
             numBeforeMe, fn _ => inorderRank rchild rchildNumBefore)
             val data' = (#1 data, #2 data, (#1 data) + numBeforeMe)
           in
             Node(lchild', data', rchild')
           end
in
  case tree' of
       Leaf(_) => Leaf((0,0,0))
     | Node(lchild, data, rchild) =>
         let
           val (lchild', rchild') = ForkJoin.par (fn _ => inorderRank lchild 0,
           fn _ => inorderRank rchild (#1 data + 1))
           val data' = (#1 data, #2 data, #1 data)
         in
           Node(lchild', data', rchild')
         end
end

val tree = makeBinaryTree 2
val _ = printTree tree 0
val _ = print "\n"
val tree' = getInorderRank tree
val _ = printTree tree' 0
