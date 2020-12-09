
(*
* the type 'a will be (sum of left subtree, sum of right subtree, inorder rank)
*)


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
let
  val Tree(treeRef) = tree
in
  case !treeRef of
       Leaf(data) =>
         (printNTimes "   " currDepth; print3Tuple data; print "\n")
     | Node(lchild, data, rchild) =>
         (printTree rchild (currDepth + 1);
         printNTimes "   " currDepth; print3Tuple data; print "\n";
         printTree lchild (currDepth + 1))
end


val tree = makeBinaryTree 2
val _ = printTree tree 0
