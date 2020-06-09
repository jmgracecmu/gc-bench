structure CLA = CommandLineArgs

val filename =
  case CLA.positional () of
    [x] => x
  | _ => Util.die "missing filename"

val numSeams = CLA.parseInt "num-seams" 100
val _ = print ("num-seams " ^ Int.toString numSeams ^ "\n")

val (image, tm) = Util.getTime (fn _ => PPM.read filename)
val _ = print ("read image in " ^ Time.fmt 4 tm ^ "s\n")
val rep = case (Int.fromString (CLA.parseString "repeat" "1")) of
               SOME(a) => a
             | NONE => 1

fun seamCurveEx() =
  let
    val (carved, tm) = Util.getTime (fn _ => SC.removeSeams numSeams image)
  in
    (carved, tm)
  end

val (carved, tm) = Util.repeat (rep, (fn _ => seamCurveEx()))

val _ = print ("carved seams in " ^ Time.fmt 4 tm ^ "s\n")

val outfile = CLA.parseString "output" ""
val _ =
  if outfile = "" then
    print ("use -output XXX to see result\n")
  else
    let
      (* val red = {red=0w255, green=0w0, blue=0w0}
      val (_, tm) = Util.getTime (fn _ =>
        PPM.write outfile (SC.paintSeam image seam red)) *)
      val (_, tm) = Util.getTime (fn _ => PPM.write outfile carved)
    in
      print ("wrote output in " ^ Time.fmt 4 tm ^ "s\n")
    end
