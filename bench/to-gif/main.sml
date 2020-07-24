structure CLA = CommandLineArgs

val (input, output) =
  case CLA.positional () of
    [input, output] => (input, output)
  | _ => Util.die "missing filename"

val (image, tm) = Util.getTime (fn _ => PPM.read input)
val _ = print ("read image in " ^ Time.fmt 4 tm ^ "s\n")

val ({colors, indices}, tm) = Util.getTime (fn _ => GIF.LZW.chooseColors image)
val _ = print ("picked colors in " ^ Time.fmt 4 tm ^ "s\n")

val _ = print ("num colors: " ^ Int.toString (Seq.length colors) ^ "\n")

val newImage =
  { width = #width image
  , height = #height image
  , data = Seq.map (Seq.nth colors) indices
  }

val (_, tm) = Util.getTime (fn _ => PPM.write output newImage)
val _ = print ("wrote to " ^ output ^ " in " ^ Time.fmt 4 tm ^ "s\n")

