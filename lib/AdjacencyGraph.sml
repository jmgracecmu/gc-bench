functor AdjacencyGraph (Vertex: INTEGER) =
struct

  structure A = Array
  structure AS = ArraySlice

  structure Vertex =
  struct
    type t = Vertex.int
    open Vertex
    val maxVal = toInt (valOf maxInt)
  end

  type vertex = Vertex.t
  fun vertexNth s v = Seq.nth s (Vertex.toInt v)
  fun vToWord v = Word64.fromInt (Vertex.toInt v)

  (* offsets, degrees, compact neighbors *)
  type graph = (int Seq.t) * (int Seq.t) * (vertex Seq.t)

  fun degree G v =
    let val (offsets, degrees, _) = G
    in (vertexNth degrees v)
    end

  fun neighbors G v =
    let
      val (offsets, _, nbrs) = G
    in
      Seq.subseq nbrs (vertexNth offsets v, degree G v)
    end

  fun numVertices G =
    let val (_, degrees, _) = G
    in Seq.length degrees
    end

  fun numEdges G =
    let val (_, _, nbrs) = G
    in Seq.length nbrs
    end

  fun parse chars =
    let
      fun isNewline i = (Seq.nth chars i = #"\n")

      (* Computing newline positions takes up about half the time of parsing...
       * Can we do this faster? *)
      val nlPos =
        AS.full (SeqBasis.filter 10000 (0, Seq.length chars) (fn i => i) isNewline)
      val numLines = Seq.length nlPos + 1
      fun lineStart i =
        if i = 0 then 0 else 1 + Seq.nth nlPos (i-1)
      fun lineEnd i =
        if i = Seq.length nlPos then Seq.length chars else Seq.nth nlPos i
      fun line i = Seq.subseq chars (lineStart i, lineEnd i - lineStart i)

      val _ =
        if numLines >= 3 then ()
        else raise Fail ("AdjacencyGraph: missing or incomplete header")

      val _ =
        if Parse.parseString (line 0) = "AdjacencyGraph" then ()
        else raise Fail ("expected AdjacencyGraph header")

      fun tryParse thing lineNum =
        let
          fun whoops () =
            raise Fail ("AdjacencyGraph: line "
                        ^ Int.toString (lineNum+1)
                        ^ ": error while parsing " ^ thing)
        in
          case (Parse.parseInt (line lineNum) handle _ => whoops ()) of
            SOME x => if x >= 0 then x else whoops ()
          | NONE => whoops ()
        end

      val numVertices = tryParse "num vertices" 1
      val numEdges = tryParse "num edges" 2

      val _ =
        if numLines >= numVertices + numEdges + 3 then ()
        else raise Fail ("AdjacencyGraph: not enough offsets and/or edges to parse")

      val offsets = SeqBasis.tabulate 1000 (0, numVertices)
        (fn i => tryParse "edge offset" (3+i))

      val neighbors = SeqBasis.tabulate 1000 (0, numEdges)
        (fn i => tryParse "neighbor" (3+numVertices+i))

      val degrees = SeqBasis.tabulate 10000 (0, numVertices) (fn i =>
        let
          val off = A.sub (offsets, i)
          val nextOff =
            if i+1 < numVertices then A.sub (offsets, i+1) else numEdges
          val deg = nextOff - off
        in
          if deg < 0 then
            raise Fail ("AdjacencyGraph: vertex " ^ Int.toString i
                        ^ " has negative degree")
          else
            deg
        end)
    in
      (AS.full offsets, AS.full degrees, AS.full neighbors)
    end

  (* Useful as a sanity check for symmetrized graphs --
   * (every symmetrized graph has edge parity 0, but not all graphs with
   * edge parity 0 are symmetrized!) *)
  fun parityCheck g =
    let
      val (offsets, _, _) = g
      val n = numVertices g

      fun canonical (u, v) =
        if Vertex.< (u, v) then (u, v) else (v, u)
      fun xorEdges ((u1, v1), (u2, v2)) =
        (Word64.xorb (u1, u2), Word64.xorb (v1, v2))
      fun packEdge (u, v) = (vToWord u, vToWord v)

      val (p1, p2) = SeqBasis.reduce 100 xorEdges (0w0, 0w0) (0, n) (fn i =>
        let
          val u = Vertex.fromInt i
          val offset = Seq.nth offsets i
        in
          SeqBasis.reduce 1000 xorEdges (0w0, 0w0) (0, degree g u) (fn j =>
            packEdge (canonical (u, Seq.nth (neighbors g u) j)))
        end)

    in
      p1 = 0w0 andalso p2 = 0w0
    end

  fun fromSortedEdges sorted =
    let
      fun edgeInts (u, v) = (Vertex.toInt u, Vertex.toInt v)
      val m = Seq.length sorted
      val n =
        1 + SeqBasis.reduce 10000 Int.max ~1 (0, m)
            (Int.max o edgeInts o Seq.nth sorted)

      fun k i = Vertex.toInt (#1 (Seq.nth sorted i))

      val ends = Seq.tabulate (fn i => if i = n then m else 0) (n+1)
      val _ = ForkJoin.parfor 10000 (0, m) (fn i =>
        if i = m-1 then
          AS.update (ends, k i, m)
        else if k i <> k (i+1) then
          AS.update (ends, k i, i+1)
        else ())
      val (offsets, _) = Seq.scan Int.max 0 ends

      fun off i = Seq.nth offsets (i+1) - Seq.nth offsets i
      val degrees = Seq.tabulate off n

      val nbrs = Seq.map #2 sorted
    in
      (offsets, degrees, nbrs)
    end

  fun dedupEdges edges =
    let
      val sorted =
        Mergesort.sort (fn ((u1,v1), (u2,v2)) =>
          case Vertex.compare (u1, u2) of
            EQUAL => Vertex.compare (v1, v2)
          | other => other) edges
    in
      AS.full (SeqBasis.filter 5000 (0, Seq.length sorted) (Seq.nth sorted)
        (fn i => i = 0 orelse Seq.nth sorted (i-1) <> Seq.nth sorted i))
    end

  fun randSymmGraph n d =
    let
      val m = Real.ceil (Real.fromInt n * Real.fromInt d / 2.0)

      fun makeEdge i =
        let
          val u = (2 * i) div d
          val v = Util.hash i mod (n-1)
        in
          (Vertex.fromInt u, Vertex.fromInt (if v < u then v else v+1))
        end

      val bothWays = ForkJoin.alloc (2*m)
      val _ = ForkJoin.parfor 1000 (0, m) (fn i =>
        let
          val (u, v) = makeEdge i
        in
          A.update (bothWays, 2*i, (u,v));
          A.update (bothWays, 2*i+1, (v,u))
        end)
    in
      fromSortedEdges (dedupEdges (AS.full bothWays))
    end

end
