structure Benchmark =
struct

  fun getTimes n f =
    let
      fun loop tms n =
        let
          val (result, tm) = Util.getTime f
        in
          print ("time " ^ Time.fmt 4 tm ^ "s\n");

          if n <= 1 then
            (result, List.rev (tm :: tms))
          else
            loop (tm :: tms) (n-1)
        end
    in
      loop [] n
    end

  fun run msg f =
    let
      val warmup = CommandLineArgs.parseInt "warmup" 0
      val _ =
        if warmup >= 0 then ()
        else Util.die "-warmup N must be at least 0"
      val rep = CommandLineArgs.parseInt "repeat" 1
      val _ =
        if rep >= 1 then ()
        else Util.die "-repeat N must be at least 1"

      val _ = print ("warmup " ^ Int.toString warmup ^ "\n")
      val _ = print ("repeat " ^ Int.toString rep ^ "\n")

      val _ =
        if warmup <= 0 then ()
        else ( print ("====== WARMUP ======\n" ^ msg ^ "\n")
             ; ignore (getTimes warmup f)
             ; print ("==== END WARMUP ====\n")
             )

      val _ = print (msg ^ "\n")
      val t0 = Time.now ()
      val (result, tms) = getTimes rep f
      val t1 = Time.now ()
      val endToEnd = Time.- (t1, t0)

      val total = List.foldl Time.+ Time.zeroTime tms
      val avg = Time.toReal total / (Real.fromInt rep)
    in
      print ("average " ^ Real.fmt (StringCvt.FIX (SOME 4)) avg ^ "s\n");
      print ("total   " ^ Time.fmt 4 total ^ "s\n");
      print ("end-to-end " ^ Time.fmt 4 endToEnd ^ "s\n");
      result
    end

end
