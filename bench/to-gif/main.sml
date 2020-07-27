structure CLA = CommandLineArgs

val (input, output) =
  case CLA.positional () of
    [input, output] => (input, output)
  | _ => Util.die "missing filename"

val (image, tm) = Util.getTime (fn _ => PPM.read input)
val _ = print ("read image in " ^ Time.fmt 4 tm ^ "s\n")
(*
val colors =
  Seq.fromList [Color.white, Color.red, Color.blue, Color.black]

fun remap (image: PPM.image) =
  Seq.map (fn px =>
    if Color.equal (px, Color.white) then 0
    else if Color.equal (px, Color.red) then 1
    else if Color.equal (px, Color.blue) then 2
    else 3) (#data image)
*)

(*
fun t (n, x) = List.tabulate (n, fn _ => x)
val firstRow = t (5, Color.red) @ t (5, Color.blue)
val middleRow = t (3, Color.red) @ t (4, Color.white) @ t (3, Color.blue)

val stuff =
Seq.fromList (
  firstRow @
  firstRow @
  firstRow @
  middleRow @
  middleRow @
  List.rev middleRow @
  List.rev middleRow @
  List.rev firstRow @
  List.rev firstRow @
  List.rev firstRow)

val image =
  { width = 10
  , height = 10
  , data = stuff
  }


val stuff = Seq.append (stuff, stuff)

val image =
  { width = 200
  , height = 1000
  , data =
      SeqBasis.reduce 1000 Seq.append (Seq.empty ()) (0, 1000) (fn _ => stuff)
  }
*)
val (_, tm) = Util.getTime (fn _ => GIF.write output image)
val _ = print ("wrote " ^ output ^ " in " ^ Time.fmt 4 tm ^ "s\n")

(* val codes = GIF.LZW.codeStream image {colors=colors, remap=remap}

val _ = print ("codes: " ^ Seq.toString Int.toString codes ^ "\n")

val bytes = GIF.LZW.packCodeStream (Seq.length colors) codes

fun wtos w =
  let val s = Word8.toString w
  in if String.size s = 1 then "0" ^ s else s
  end

val _ = print ("expected:  02 16 8C 2D 99 87 2A 1C DC 33 A0 02 75 EC 95 FA A8 DE 60 8C 04 91 4C 01 00\n")
val _ = print ("generated: " ^ String.translate (fn #"," => " " | c => Char.toString c) (Seq.toString wtos bytes) ^ "\n") *)

(* val ({colors, remap}, tm) = Util.getTime (fn _ => GIF.Palette.quantized (7,7,5)) *)
(* val ({colors, remap}, tm) = Util.getTime (fn _ => GIF.Palette.summarize image) *)
(* val _ = print ("picked colors in " ^ Time.fmt 4 tm ^ "s\n") *)

(*
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
*)

(*
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
*)
