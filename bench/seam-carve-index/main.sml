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

val _ = print ("height " ^ Int.toString h ^ "\n")
val _ = print ("width " ^ Int.toString w ^ "\n")

val _ =
  if numSeams >= 0 andalso numSeams <= w then ()
  else
    Util.die ("cannot remove " ^ Int.toString numSeams
    ^ " seams from image of width " ^ Int.toString w ^ "\n")

val X = Benchmark.run "seam carving"
  (fn _ => SCI.makeSeamCarveIndex numSeams image)

val outfile = CLA.parseString "output" ""
val _ =
  if outfile = "" then
    print ("use -output XXX to see result\n")
  else
    let
      fun colorSeam i =
        Color.hsv { h = 100.0 * (Real.fromInt i / Real.fromInt numSeams)
                  , s = 1.0
                  , v = 1.0
                  }

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
                colorSeam (Seq.nth X k)
            end))
        }
      val (_, tm) = Util.getTime (fn _ => PPM.write outfile carved)
    in
      print ("wrote output in " ^ Time.fmt 4 tm ^ "s\n")
    end

val _ = GCStats.report ()
