
(*
* the type 'a will be (sum of left subtree, sum of right subtree, inorder rank)
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


fun makeUnbalancedBinaryTree depthL depthR = 
let
  val lChild = makeBinaryTree depthL
  val rChild = makeBinaryTree depthR
  val data = (0,0,0)
in
  Node(lChild, data, rChild)
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
         (printNTimes "   " currDepth; print3Tuple data; print "\n")
     | Node(lchild, data, rchild) =>
         (printTree rchild (currDepth + 1);
         printNTimes "   " currDepth; print3Tuple data; print "\n";
         printTree lchild (currDepth + 1))


val tree = makeBinaryTree 2
val unbalancedTree = makeUnbalancedBinaryTree 2 4
val _ = print "\n"
val tree' = inorderRank2 tree
val _ = printTree tree' 0
