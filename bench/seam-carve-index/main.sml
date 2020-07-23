structure CLA = CommandLineArgs

val filename =
  case CLA.positional () of
    [x] => x
  | _ => Util.die "missing filename"

val numSeams = CLA.parseInt "num-seams" 100
val _ = print ("num-seams " ^ Int.toString numSeams ^ "\n")

val (image, tm) = Util.getTime (fn _ => PPM.read filename)
val _ = print ("read image in " ^ Time.fmt 4 tm ^ "s\n")

val w = #width image
val h = #height image

val X = Benchmark.run "seam carving"
  (fn _ => SCI.makeSeamCarveIndex numSeams image)

val outfile = CLA.parseString "output" ""
val _ =
  if outfile = "" then
    print ("use -output XXX to see result\n")
  else
    let
      val carved =
        { width = w
        , height = h
        , data = ArraySlice.full (SeqBasis.tabulate 4000 (0, w * h) (fn k =>
            let
              val i = k div w
              val j = k mod w
            in
              if Seq.nth X k < 0 then
                PPM.elem image (i, j)
              else
                Color.red
            end))
        }
      val (_, tm) = Util.getTime (fn _ => PPM.write outfile carved)
    in
      print ("wrote output in " ^ Time.fmt 4 tm ^ "s\n")
    end

val _ = GCStats.report ()
