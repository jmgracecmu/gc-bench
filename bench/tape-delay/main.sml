structure CLA = CommandLineArgs

val infile =
  case CLA.positional () of
    [x] => x
  | _ => Util.die ("[ERR] usage: echo INPUT_FILE [-output OUTPUT_FILE]\n")

val outfile = CLA.parseString "output" ""

val delayTime = CLA.parseReal "delay" 0.5
val decayFactor = CLA.parseReal "decay" 0.2

val delaySamples = Real.round (44100.0 * delayTime)
val decaydB = Real.round (20.0 * Math.log10 decayFactor)

val _ = print ("delay " ^ Real.toString delayTime ^ "s ("
               ^ Int.toString delaySamples ^ " samples)\n")
val _ = print ("decay " ^ Real.toString decayFactor ^ " ("
               ^ Int.toString decaydB ^ "dB)\n")

val (snd, tm) = Util.getTime (fn _ => NewWaveIO.readSound infile)
val _ = print ("read sound in " ^ Time.fmt 4 tm ^ "s\n")

val (esnd, tm) =
  Util.getTime (fn _ => Signal.delay delaySamples decayFactor snd)
val _ = print ("echo sound in " ^ Time.fmt 4 tm ^ "s\n")

val _ =
  if outfile = "" then
    print ("use -output file.wav to hear results\n")
  else
    let
      val (_, tm) = Util.getTime (fn _ => NewWaveIO.writeSound esnd outfile)
    in
      print ("wrote output in " ^ Time.fmt 4 tm ^ "s\n")
    end
