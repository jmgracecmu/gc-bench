structure CLA = CommandLineArgs


fun dmmEx n =
	let
		val _ = print ("generating matrices of sidelength " ^ Int.toString n ^ "\n")

		val input = TreeMatrix.tabulate n (fn (i, j) => 1.0)

		val _ = print ("multiplying\n")

		val (result, tm) = Util.getTime (fn _ => TreeMatrix.multiply (input, input))

		val _ = print ("finished in " ^ Time.fmt 4 tm ^ "s\n")
	in
		(result, tm)
	end



val n = CLA.parseInt "N" 1024
val rep = case (Int.fromString (CLA.parseString "repeat" "1")) of
               SOME(a) => a
             | NONE => 1

val (result, tm) = Util.repeat (rep, (fn _ => dmmEx n))
val _ =
  if Util.boundPow2 n = n then ()
  else raise Fail "sidelength N must be a power of two"

