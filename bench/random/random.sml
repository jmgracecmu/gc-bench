structure  CLA = CommandLineArgs
(* build an array in parallel with elements f(i) for each 0 <= i < n *)
fun tabulate (n, f) =
  let
    val arr = ForkJoin.alloc n
  in
    ForkJoin.parfor 10000 (0, n) (fn i => Array.update (arr, i, f i));
    arr
  end

(* generate the ith element with a hash function *)
fun gen seed i = Util.hash64 (Word64.xorb (Word64.fromInt i, seed))

(* ==========================================================================
 * parse command-line arguments and run
 *)

val n = CLA.parseInt "N" (1000 * 1000 * 1000)
val seed = CLA.parseInt "seed" 0
val rep = case (Int.fromString (CLA.parseString "repeat" "1")) of
               SOME(a) => a
             | NONE => 1

val _ = print ("tabulate " ^ Int.toString n ^ " pseudo-random 64-bit words\n")
val _ = print ("seed " ^ Int.toString seed ^ "\n")

val seed' = Util.hash64 (Word64.fromInt seed)


fun randomEx() =
	let
		val (result, tm) = Util.getTime(fn _ => tabulate (n, gen seed'))
	in
		(result, tm)
	end
val (result, tm) = Util.repeat (rep, (fn _ => randomEx()))

val _ = print ("finished in " ^ Time.fmt 4 tm ^ "s\n")

fun str x = Word64.fmt StringCvt.HEX x
val _ = print ("result " ^ Util.summarizeArray 3 str result ^ "\n")
