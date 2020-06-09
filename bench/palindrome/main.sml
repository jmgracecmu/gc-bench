structure CLA = CommandLineArgs
structure P = Pal

val n = CLA.parseInt "N" (1000 * 1000)
val rep = case (Int.fromString (CLA.parseString "repeat" "1")) of
               SOME(a) => a
             | NONE => 1


(* makes the sequence `ababab...` *)
fun gen i = if i mod 2 = 0 then #"a" else #"b"
val (input, tm) = Util.getTime (fn _ => Seq.tabulate gen n)
val _ = print ("generated input in " ^ Time.fmt 4 tm ^ "s\n")

fun palindromeEx() =
	let
		val (result, tm) = Util.getTime (fn _ => Pal.longest input)
	in
		(result, tm)
	end
val (result, tm) = Util.repeat (rep, (fn _ => palindromeEx()))


val _ = print ("found longest palindrome in " ^ Time.fmt 4 tm ^ "s\n")

val correct =
  if n mod 2 = 0
  then result = (1, n-1)
  else result = (0, n)

val _ =
  if correct then
    print ("correct? yes\n")
  else
    print ("correct? no\n")
