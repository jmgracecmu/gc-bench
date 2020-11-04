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
  val targets = map (ifCondApp cond update source) (G.neighbors graph source)
  val suc = filter (fn x => case x of Some x' => true | None => false) targets
in
  map (fn Some x => x) suc
end

(*
* assumes that graph is in the offset * degrees * nbrs format
* edgeMap should return all the targets that we applied the function update to
*)
fun edgeMap graph sources cond update =
let
  val unflatNbrs = map (appToNbrsOfSource graph cond update) sources
in
  flatten unflatNbrs
end

val graph = G.parseFile "g0.txt"
val sources = Seq.tabulate (fn x => x) (G.numVertices graph)
fun printTarget _ t =
  print ((Int.toString t) ^ " ")

val _ = edgeMap graph sources (fn _ => true) printTarget

(* assume graph is in the format of offsets * degrees * nbrs *)
fun betweenCentral graph =
let
  val numPaths = Seq.tabulate (fn _ => 0) (G.numVertices graph)
  val visited = Seq.tabulate (fn _ => 0) (G.numVertices graph)
  val root = 0
  val _ = SeqBasis.upd numPaths root 1
  val _ = SeqBasis.upd visited root 1
  (* TODO make ref so it can be updated *)
  val currLevel = 0
  val levels = Seq.tabulate (fn _ => Seq.tabulate (fn _ => 0) 0) (G.numVertices graph)
  val dependencies = Seq.tabulate (fn _ => 0.0) (G.numVertices graph)
  fun visit vertex =
    SeqBasis.upd visited vertex 1; 1

  fun pathsUpdate src tgt =
  let
    val oldNumPaths = Seq.nth numPaths tgt
    val newNumPaths = oldNumPaths + Seq.nth numPaths src
  in
    if arrayCompareAndSwap (numPaths, tgt) (oldNumPaths, newNumPaths) == oldNumPaths then
      (* need to retry *)
      pathsUpdate src tgt
    else
      oldNumPaths == 0
  end

  fun dependenciesUpdate src tgt =
  let
    val oldDep = Seq.nth dependencies tgt
    val ratio = (Real.fromInt (Seq.nth numPaths tgt)) / (Real.fromInt (Seq.nth numPaths src))
    val newDep = oldDep + ratio * (1.0 + Seq.nth dependencies src)
  in
    if arrayCompareAndSwap (dependencies, tgt) (oldDep, newDep) == oldDep then
      (* need to retry *)
      dependenciesUpdate src tgt
    else
      oldDep == 0.0
  end

  fun cond vertex = Seq.nth visited vertex == 0

  fun dependencies =
  let
    val frontier = Seq.fromList [root]
    val _ = makeNumPaths
    val _ = Seq.foreach (fn _ => 0) visited
  in


in

    


(*
* Notes. So write another algorithm. what's another algo that can have this
* be done?
*
*
*
*
* }

