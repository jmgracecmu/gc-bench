
datatype 'a Tree = Node of 'a Tree * 'a * 'a Tree | Leaf of 'a

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

