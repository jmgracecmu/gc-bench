structure Color =
struct
  type channel = Word8.word
  type pixel = {red: channel, green: channel, blue: channel}

  val white: pixel = {red=0w255, green=0w255, blue=0w255}
  val black: pixel = {red=0w0, green=0w0, blue=0w0}
  val red: pixel = {red=0w255, green=0w0, blue=0w0}
  val blue: pixel = {red=0w0, green=0w0, blue=0w255}

  (* hue in range [0,360)
   * saturation in range [0,1]
   * value in range [0,1]
   *)
  fun hsv {h: real, s: real, v: real}: pixel =
    let
      val H = h
      val S = s
      val V = v

      (* from https://en.wikipedia.org/wiki/HSL_and_HSV#HSV_to_RGB *)
      val C = V * S
      val H' = H / 60.0
      val X = C * (1.0 - Real.abs (Real.rem (H', 2.0) - 1.0))

      val (R1, G1, B1) =
        if H' < 1.0 then      (C,   X,   0.0)
        else if H' < 2.0 then (X,   C,   0.0)
        else if H' < 3.0 then (0.0, C,   X)
        else if H' < 4.0 then (0.0, X,   C)
        else if H' < 5.0 then (X,   0.0, C)
        else                  (C,   0.0, X)

      val m = V - C

      fun to256 channel =
        Word8.fromInt (Real.ceil (channel * 255.0))
    in
      {red = to256 (R1 + m), green = to256 (G1 + m), blue = to256 (B1 + m)}
    end
end
