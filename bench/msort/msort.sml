structure CLA = CommandLineArgs

val n = CLA.parseInt "N" (100*1000*1000)
val rep = case (Int.fromString (CLA.parseString "repeat" "1")) of
               SOME(a) => a
             | NONE => 1

val _ = print ("generating " ^ Int.toString n ^ " random integers\n")

fun elem i =
  Word64.toInt (Word64.mod (Util.hash64 (Word64.fromInt i), Word64.fromInt n))
val input = ArraySlice.full (SeqBasis.tabulate 10000 (0, n) elem)

val _ = print ("sorting\n")

fun msortEx() =
	let
		val (result, tm) = Util.getTime (fn _ => Mergesort.sort Int.compare input)
	in
		(result, tm)
	end


val (result, tm) = Util.repeat (rep, (fn _ => msortEx()))


val _ = print ("finished in " ^ Time.fmt 4 tm ^ "s\n")

val _ = print ("result " ^ Util.summarizeArraySlice 8 Int.toString result ^ "\n")
