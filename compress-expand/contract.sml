(* A node has a value, and two out-edges
   Each out edge is rep'd by the index of the node in the "tree"
 *)


structure TreeContraction = 
struct
type edgeData = int
type nodeData = int

datatype node = Node of nodeData * (int * edgeData) * (int * edgeData)
datatype tree = node Array

fun mkRandomTree (genNode: int -> nodeData) 
                 (genEdge: int * int -> edgeData) 
                 (n: int) : tree  =
  let     
    val tree = Array.... 
    fun mk i =  
      
  in
  end   


fun compressData (nodeAcc: edgeData, upEdge: edgeData): edgeData
----- upEdge ----- node

fun rakeData (nodeAcc: edgeData, upEdge: edgeData): edgeData
---- upEdge --- node 

fun collectData (left: edgeData, right: edgeData, node: nodeData): edgeData
   node = contracted left * contracted right * node data itself

fun doCompressData (node as (nodeData, leftEdge, rightEdge), upEdge)= 
  let 
    val (leftEdgePos, leftEdgeData) = leftEdge
    val (rightEdgePos, rightEdgeDat) = rightEdge
    val (upEdgePos, upEdgeDat) = upEdge
    val accNode = collectData (leftEdgeData, rightEdgeData, nodeData)
  in
      

(* leaves are represented as Nodes whose both children are Nil
* This is needed so that we can represent nodes with only one child.
* We could have a Leaf constructor, but then there would be two different
* ways to represent a leaf and that would be confusing
*)

datatype 'a Tree = Tree of ('a Node) ref | Nil


fun makeBinaryTree depth =
  case depth
    of 0 =>
    let
      val leafRef = ref (Node(Nil, (0,0,0), Nil))
    in
      (Tree(leafRef), Seq.fromList [leafRef])
    end
     | x =>
    let
      val (lChild, lSeq) = makeBinaryTree (x - 1)
      val (rChild, rSeq) = makeBinaryTree (x - 1)
      val data = (0,0,0)
    in
      (Tree(ref (Node(lChild, data, rChild))), Seq.append (lSeq, rSeq))
    end


fun makeUnbalancedBinaryTree depthL depthR = 
let
  val (lChild, lSeq) = makeBinaryTree depthL
  val (rChild, rSeq) = makeBinaryTree depthR
  val data = (0,0,0)
in
  (Tree(ref (Node(lChild, data, rChild))), Seq.append (lSeq, rSeq))
end


(*
* So now that you have a tree. What can you do with the tree now? you can 
* the compex function will apply a function to every item in the tree.
* how do you know when you have collapsed the tree completely? Also you need to


     | Node(Nil, _, rChild) =>
         let
           val Tree(rChildRef) = rChild
           val rChildIsLeaf = case !rChildRef
                                of Node(Nil, _, Nil) => true
                                 | _ => false
         in
           rChildIsLeaf
         end
     | Node(lChild, _, Nil) =>
         let
           val Tree(lChildRef) = lChild
           val lChildIsLeaf = case !lChildRef
                                of Node(Nil, _, Nil) => true
                                 | _ => false
         in
           lChildIsLeaf
         end
* keep track of 
*)




fun isRakeable node =
  case node
    of Node(Nil, _, Nil) => false
     | Node(lChild, _, rChild) =>
         let 
           val Tree(lChildRef) = lChild
           val Tree(rChildRef) = rChild
           val lChildIsLeafOrNil = case lChild
                                     of Nil => true
                                      | Tree(lChildRef) =>
                                          case !lChildRef
                                            of Node(Nil, _, Nil) => true
                                             | _ => false
           val rChildIsLeafOrNil = case rChild
                                     of Nil => true
                                      | Tree(rChildRef) =>
                                          case !rChildRef
                                            of Node(Nil, _, Nil) => true
                                             | _ => false
         in
           lChildIsLeafOrNil and rChildIsLeafOrNil (* can't both be Nil *)
         end

fun childIsCompressible node =

(* TODO *)

fun compex rakeMakeData (tree, treeSeq) =
let
  fun applyToEachNode node =
    case node
      of Node(Nil, data, Nil) => Node(Nil, data, Nil)
       | Node(lChild, data, rChild) =>
           let
             val Tree(lChildRef) = lChild
             val Tree(rChildRef) = rChild
           in
             case !lChildRef
               of Node(Nil, lData, Nil) =>
                 case !rChildRef
                   of Node(Nil, rData, Nil) =>
                     Node(Nil, rakeMakeData lData rData, Nil)
                    | Node(rlChild, rData, rrChild) => 



(* apply rake to every node *)



(*
* The up-pass feeds data from children to parents. So the function f takes
* two trees and it's own current data and returns what its new data should be.
* The two trees are the children of the current node.
*
* This function won't do anything to the leaves because they have new children.
* If you need to change the leaves at the bottom of the tree before you
* perform an uppass, you should write a separate function to initialize the
* leaves.
*
* This function does an up-pass because no information flows from parent to
* child--information only flows from child to parent. The recursive call
* 'uppass' doesn't take anything from the parent, so the children have to do
* their computations without any extra data. The parent can make use of the 
* new data in the children though, so information is flowing up.
*)

(*
fun uppass tree (makeData: 'a Tree -> 'a Tree -> 'a -> 'a) =
  case tree of
       Leaf(data) => Leaf(data)
     | Node(lchild, data, rchild) =>
         let
           val lchild' = uppass lchild makeData
           val rchild' = uppass rchild makeData
           val (lchild', rchild') = ForkJoin.par (fn _ => uppass lchild
           makeData, fn _ => uppass rchild makeData)
           val data' = makeData lchild' rchild' data
         in
           Node(lchild', data', rchild')
         end


*)

(*
* Because it's a downpass, you should be able to write a tail-recursive
* function.
* It's impossible to do a tail-recursive function here though because a new
* node needs to get allocated on the heap when you push information down to 
* the child. You don't know the address of this node, so you have to return it
* and then incorporate it into the new tree you're building. If you could
* reuse the same memory addresses, you could write the function to update the
* data in place, and then it could be a tail-recursive function.
*)

(*
fun downpass makeDataLChild makeDataRChild tree nullData =
let
  fun downpassRec makeData tree parentData =
    case tree of
         Leaf(data) => Leaf(makeData data parentData)
       | Node(lchild, data, rchild) =>
           let 
             val data' = makeData data parentData
             val lchildClosure = fn _ => downpassRec makeDataLChild lchild data'
             val rchildClosure = fn _ => downpassRec makeDataRChild rchild data'
             val (lchild', rchild') = ForkJoin.par (lchildClosure, rchildClosure)
           in
             Node(lchild', data', rchild')
           end
in
  downpassRec makeDataLChild tree nullData
end
*)
