structure CLA = CommandLineArgs

val (pat, file) =
  case CLA.positional () of
    [pat, file] => (pat, file)
  | _ => Util.die ("[ERR] usage: grep PATTERN FILE")

val benchmark = CLA.parseFlag "benchmark"
val rep = case (Int.fromString (CLA.parseString "repeat" "1")) of
               SOME(a) => a
             | NONE => 1

fun bprint str =
  if not benchmark then ()
  else print str

val (source, tm) = Util.getTime (fn _ => ReadFile.contentsSeq file)
val _ = bprint ("read file in " ^ Time.fmt 4 tm ^ "s\n")

fun grepEx() =
	let
		val pattern = Seq.tabulate (fn i => String.sub (pat, i)) (String.size pat)
		val ((matches, output), tm) = Util.getTime (fn _ => Grep.grep pattern source)
	in
		((matches, output), tm)
	end

val ((matches, output), tm) = Util.repeat (rep, (fn _ => grepEx()))

val _ = bprint ("grep completed in " ^ Time.fmt 4 tm ^ "s\n")
val _ = bprint ("number of matched lines: " ^ Int.toString matches ^ "\n")
val _ = bprint ("length of output: " ^ Int.toString (Seq.length output) ^ "\n")

val _ =
  if benchmark then ()
  else
    ArraySlice.app (fn c => TextIO.output1 (TextIO.stdOut, c)) output
