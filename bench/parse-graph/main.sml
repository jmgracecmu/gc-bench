structure CLA = CommandLineArgs
structure G = AdjacencyGraph(Int)

val filename =
  case CLA.positional () of
    [x] => x
  | _ => Util.die "missing filename"

val (chars, tm) = Util.getTime (fn _ => ReadFile.contentsSeq filename)
val _ = print ("read file in " ^ Time.fmt 4 tm ^ "s\n")

val (graph, tm) = Util.getTime (fn _ => G.parse chars)

val _ = print ("parsed graph in " ^ Time.fmt 4 tm ^ "s\n")
val _ = print ("num vertices: " ^ Int.toString (G.numVertices graph) ^ "\n")
val _ = print ("num edges: " ^ Int.toString (G.numEdges graph) ^ "\n")
