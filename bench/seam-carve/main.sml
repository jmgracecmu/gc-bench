structure CLA = CommandLineArgs

val filename =
  case CLA.positional () of
    [x] => x
  | _ => Util.die "missing filename"

val (image, tm) = Util.getTime (fn _ => PPM.read filename)
val _ = print ("read image in " ^ Time.fmt 4 tm ^ "s\n")

val (seam, tm) = Util.getTime (fn _ => SC.minSeam image)
val _ = print ("found seam in " ^ Time.fmt 4 tm ^ "s\n")

val outfile = CLA.parseString "output" ""
val _ =
  if outfile = "" then
    print ("use -output XXX to see result\n")
  else
    let
      val outputImage = SC.paintSeam image seam {red=0w255, green=0w0, blue=0w0}
    in
      PPM.write outfile outputImage
    end
