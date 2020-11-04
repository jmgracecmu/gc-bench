(*
* Notes
*
* 
*
*)

structure G = AdjacencyGraph(Int)

fun ifCondApp cond update source target =
  (* update returns true if the target was successfully updated *)
  if cond target
  then
    if update source target then Some of target else None
  else
    None

fun appToNbrsOfSource graph cond update source =
let
  val targets = Seq.map (ifCondApp cond update source) (G.neighbors graph source)
  val suc = filter (fn x => case x of Some x' => true | None => false) targets
in
  Seq.map (fn Some x => x) suc
end

(*
* assumes that graph is in the offset * degrees * nbrs format
* edgeMap should return all the targets that we applied the function update to
*)
fun edgeMap graph sources cond update =
let
  val unflatNbrs = Seq.map (appToNbrsOfSource graph cond update) sources
in
  flatten unflatNbrs
end

val graph = G.parseFile "g0.txt"
val sources = Seq.tabulate (fn x => x) (G.numVertices graph)
fun printTarget _ t =
  print ((Int.toString t) ^ " ")

val _ = edgeMap graph sources (fn _ => true) printTarget

fun bfs graph =
let 
  val parents = Seq.tabulate (fn _ => -1) (G.numVertices graph)
  fun update s t =
    (* this needs to return whether the update was performed or not *)
    (* TODO according to the type definitions, arrayCompareAndSwap returns
    * something of the type of the array. What does it return on failure, and
    * what does it return on success? I will assume that it returns the new
    * value on success
    *)
    if arrayCompareAndSwap (parents, t) (-1, s) == s then
      true
    else
      false

  fun loop front =
  let
    val newFront = edgeMap graph front  (fn t => Seq.nth parents t == -1) update;
  in
    if Seq.length newFront == 0 then () else loop newFront
  end
  
  val frontier = Seq.fromList [0]
in
  loop frontier; parents
end


(*
* Notes. So write another algorithm. what's another algo that can have this
* be done?
*
*
*
*
* }

