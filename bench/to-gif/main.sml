structure CLA = CommandLineArgs

val (input, output) =
  case CLA.positional () of
    [input, output] => (input, output)
  | _ => Util.die "missing filename"

val (image, tm) = Util.getTime (fn _ => PPM.read input)
val _ = print ("read image in " ^ Time.fmt 4 tm ^ "s\n")

(* val ({colors, remap}, tm) = Util.getTime (fn _ => GIF.Palette.quantized (7,7,5)) *)
val ({colors, remap}, tm) = Util.getTime (fn _ => GIF.Palette.summarize image)
val _ = print ("picked colors in " ^ Time.fmt 4 tm ^ "s\n")

val _ = print ("num colors: " ^ Int.toString (Seq.length colors) ^ "\n")

val paletteImage =
  { width = 256
  , height = 256
  , data = Seq.tabulate (fn k =>
      let
        val i = k div 256
        val j = k mod 256

        val ci = i div 16
        val cj = j div 16

        val ck = ci * 16 + cj
      in
        if ck < Seq.length colors then Seq.nth colors ck else Color.black
      end) (256 * 256)
  }

val _ = PPM.write "palette.ppm" paletteImage

val _ = print ("wrote chosen colors to palette.ppm\n")

fun make () =
  { width = #width image
  , height = #height image
  , data = Seq.map (Seq.nth colors) (remap image)
  }

val (newImage, tm) = Util.getTime make
val _ = print ("remapped image in " ^ Time.fmt 4 tm ^ "s\n")

val avgDist =
  SeqBasis.reduce 500 op+ 0.0 (0, #width image * #height image) (fn k =>
    Color.approxHumanPerceptionDistance
      (Seq.nth (#data image) k,
       Seq.nth (#data newImage) k))
  / Real.fromInt (#width image * #height image)

val _ = print ("avg color distance: " ^ Real.toString avgDist ^ "\n")

val (_, tm) = Util.getTime (fn _ => PPM.write output newImage)
val _ = print ("wrote to " ^ output ^ " in " ^ Time.fmt 4 tm ^ "s\n")

