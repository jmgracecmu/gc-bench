structure Signal:
sig
  type sound = NewWaveIO.sound
  val delay: real -> real -> sound -> sound
  val allPass: real -> real -> sound -> sound
  val reverb: sound -> sound
end =
struct

  type sound = NewWaveIO.sound

  structure A = Array
  structure AS = ArraySlice

  (* ds: the delay distance, in seconds
   * a: the decay parameter in range [0,1]
   * snd: sequence of samples in range [-1,1]
   *)
  fun delay ds a (snd as {sr, data}: sound) =
    let
      val N = Seq.length data

      (* convert to samples *)
      val ds = Real.round (ds * Real.fromInt sr)

      (* i: skip distance
       * s: current sound
       * powa: a^i
       *
       * Each loop iteration adds `a^i * s[k]` to `s[k+i*ds]` for each k.
       * we compute this by "pulling" `a^i * s[j-i*ds]` into each s[j].
       * This can either be done in two ways:
       *   1. Copy the entire sound and recompute each s[j]
       *   2. Modify only the s[j]'s which need to be recomputed. Note that
       *   all samples before index i*ds do not change.
       *
       * Approach 1 performs exactly N writes, whereas approach 2 performs
       * approximately 2*(N-i*ds) writes (because it writes intermediate results
       * into a temporary array and then copies back into s). So, for earlier
       * iterations (when i*ds is small) it makes sense to prefer the former.
       * Then when i*ds is large enough, we switch to the latter approach.
       * This guarantees O(N) work in total, because i increases exponentially.
       *
       * We're done when the skip distance i*ds exceeds the number of samples,
       * or when a^i is sufficiently small (at which point further contributions
       * would not be audible).
       *)

      fun next s powa i j =
        let val k = j - (i*ds)
        in (Seq.nth s j) + (if k < 0 then 0.0 else powa * Seq.nth s k)
        end

      fun loop s powa i =
        if i*ds >= N orelse powa < 0.0001 then
          s
        else if i*ds < N div 10 then
          loop (Seq.tabulate (next s powa i) N) (powa*powa) (2*i)
        else
          let
            val tmp = SeqBasis.tabulate 10000 (i*ds, N) (next s powa i)
          in
            ForkJoin.parfor 10000 (i*ds, N) (fn j =>
              AS.update (s, j, A.sub (tmp, j - i*ds)));

            loop s (powa*powa) (2*i)
          end

      (* It's not correct to just call `loop data a 1`, because this could
       * modify the input `data`. We need to make sure that we are operating
       * on a copy. So, we'll open up the first iteration of the loop.
       *)
      val result =
        if ds >= N orelse a < 0.0001 then
          data
        else
          loop (Seq.tabulate (next data a 1) N) (a*a) 2
    in
      {sr = sr, data = result}
    end

  fun delaySequential D a data =
    let
      val n = Seq.length data
      val output = ForkJoin.alloc n
    in
      Util.for (0, n) (fn i =>
        if i < D then
          Array.update (output, i, Seq.nth data i)
        else
          Array.update (output, i, Seq.nth data i + a * Array.sub (output, i - D))
      );

      AS.full output
    end

  fun pow (a: real) n =
    if n <= 1 then
      a
    else if n mod 2 = 0 then
      pow (a*a) (n div 2)
    else
      a * pow (a*a) (n div 2)

  (* Granularity parameters *)
  val blockWidth = CommandLineArgs.parseInt "comb-width" 600
  val blockHeight = CommandLineArgs.parseInt "comb-height" 50
  val _ = print ("comb-width " ^ Int.toString blockWidth ^ "\n")
  val _ = print ("comb-height " ^ Int.toString blockHeight ^ "\n")

  (* Imagine laying out the data as a matrix, where sample s[i*D + j] is
   * at row i, column j.
   *)
  fun delay' D alpha data =
    if alpha < 0.0001 then
      data
    else if Seq.length data <= 10000 then
      delaySequential D alpha data
    else
    let
      val n = Seq.length data
      val output = ForkJoin.alloc n

      val numCols = D
      val numRows = Util.ceilDiv n D

      fun getOutput i j =
        Array.sub (output, i*numCols + j)

      fun setOutput i j x =
        let val idx = i*numCols + j
        in if idx < n then Array.update (output, idx, x) else ()
        end

      fun input i j =
        let val idx = i*numCols + j
        in if idx >= n then 0.0 else Seq.nth data idx
        end

      val powAlpha = pow alpha blockHeight

      val numColumnStrips = Util.ceilDiv numCols blockWidth
      val numRowStrips = Util.ceilDiv numRows blockHeight

      fun doColumnStrip c =
        let
          val jlo = blockWidth * c
          val jhi = Int.min (numCols, jlo + blockWidth)
          val width = jhi - jlo
          val summaries =
            AS.full (ForkJoin.alloc (width * numRowStrips))

          fun doBlock b =
            let
              val ilo = blockHeight * b
              val ihi = Int.min (numRows, ilo + blockHeight)
              val ss = Seq.subseq summaries (width * b, width)
            in
              Util.for (0, width) (fn j => AS.update (ss, j, input ilo (jlo+j)));

              Util.for (ilo+1, ihi) (fn i =>
                Util.for (0, width) (fn j =>
                  AS.update (ss, j, input i (jlo+j) + alpha * Seq.nth ss j)
                )
              )
            end

          val _ = ForkJoin.parfor 1 (0, numRowStrips) doBlock
          val summaries' = delay' width powAlpha summaries

          fun fillOutputBlock b =
            let
              val ilo = blockHeight * b
              val ihi = Int.min (numRows, ilo + blockHeight)
            in
              if b = 0 then
                Util.for (jlo, jhi) (fn j => setOutput 0 j (input 0 j))
              else
                let
                  val ss = Seq.subseq summaries' (width * (b-1), width)
                in
                  Util.for (0, width) (fn j =>
                    setOutput ilo (jlo+j) (input ilo (jlo+j) + alpha * Seq.nth ss j))
                end;

              Util.for (ilo+1, ihi) (fn i =>
                Util.for (jlo, jhi) (fn j =>
                  setOutput i j (input i j + alpha * getOutput (i-1) j)
                )
              )
            end
        in
          ForkJoin.parfor 1 (0, numRowStrips) fillOutputBlock
        end
    in
      ForkJoin.parfor 1 (0, numColumnStrips) doColumnStrip;

      AS.full output
    end

  fun delay ds alpha ({sr, data}: sound) =
    let
      val ds = Real.round (ds * Real.fromInt sr)
    in
      {sr = sr, data = delay' ds alpha data}
    end

  fun allPass ds a (snd as {sr, data}: sound) =
    let
      val {data=combed, ...} = delay ds a snd

      (* convert to samples *)
      val ds = Real.round (ds * Real.fromInt sr)

      fun output j =
        let
          val k = j - ds
        in
          (1.0 - a*a) * (if k < 0 then 0.0 else Seq.nth combed k)
          - (a * Seq.nth combed j)
        end
    in
      { sr = sr
      , data = Seq.tabulate output (Seq.length data)
      }
    end

  val par = ForkJoin.par

  fun par4 (a, b, c, d) =
    let
      val ((ar, br), (cr, dr)) =
        par (fn _ => par (a, b), fn _ => par (c, d))
    in
      (ar, br, cr, dr)
    end

  fun shiftBy n s i =
    if i < n then
      0.0
    else if i < Seq.length s + n then
      Seq.nth s (i-n)
    else
      0.0

  fun reverb (dry: sound) =
    let
      val N = Seq.length (#data dry)

      val sr = #sr dry
      val srr = Real.fromInt sr
      fun secondsToSamples sec = Real.round (sec * srr)

      fun secondsAt441 samples = Real.fromInt samples / 44100.0

      (* ==========================================
       * Fused reflections near 50ms
       * (at 44.1kHz, 50ms is about 2200 samples)
       *
       * The basic design is taken from
       *   The Computer Music Tutorial (1996), page 481
       *   Author: Curtis Roads
       *
       * The basic design is 4 comb filters (in parallel)
       * which are then fed into two allpass filters, in series.
       *
       * Originally, I tuned the comb and allPass parameters
       * based on numbers of samples at 44.1 kHz, which I chose
       * to be relatively prime to one another. But now, to
       * handle any sample rate, I switched to parameters
       * expressed in seconds. Does it really matter if the
       * sample delays are relatively prime? I'm not sure. For
       * sample rates other than 44.1 kHz, they almost certainly
       * won't be now.
       *)

      val (c1, c2, c3, c4) =
        par4 (fn _ => delay (secondsAt441 1931) 0.7 dry,
              fn _ => delay (secondsAt441 2213) 0.7 dry,
              fn _ => delay (secondsAt441 1747) 0.7 dry,
              fn _ => delay (secondsAt441 1559) 0.7 dry)

      fun combs i =
        (Seq.nth (#data c1) i +
         Seq.nth (#data c2) i +
         Seq.nth (#data c3) i +
         Seq.nth (#data c4) i)

      val fused = {sr = sr, data = Seq.tabulate combs N}
      val fused = allPass (secondsAt441 167) 0.6 fused
      val fused = allPass (secondsAt441 191) 0.6 fused

      (* ==========================================
       * early reflections are single echos of
       * the dry sound that occur after around
       * 25ms delay
       *)
      val ed1 = secondsToSamples (secondsAt441 1013)
      val ed2 = secondsToSamples (secondsAt441 1102)
      val ed3 = secondsToSamples (secondsAt441 1300)

      (* ==========================================
       * wet signal = dry + early + fused
       * the fused reflections start emerging after
       * approximately 35ms
       *)
      val fusedDelay = secondsToSamples (secondsAt441 1500)

      val wet = Seq.tabulate (fn i =>
          shiftBy 0 (#data dry) i
          + 0.6 * (shiftBy ed1 (#data dry) i)
          + 0.5 * (shiftBy ed2 (#data dry) i)
          + 0.4 * (shiftBy ed3 (#data dry) i)
          + 0.75 * (shiftBy fusedDelay (#data fused) i))
        (N + fusedDelay)

    in
      (* wet *)
      NewWaveIO.compress 2.0 {sr=sr, data=wet}
    end

end
