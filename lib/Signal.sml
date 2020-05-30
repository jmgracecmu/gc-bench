structure Signal =
struct

  structure A = Array
  structure AS = ArraySlice

  (* ds: the delay distance, in number of samples
   * a: the decay parameter in range [0,1]
   * snd: sequence of samples in range [-1,1]
   *)
  fun delay ds a snd =
    let
      val N = Seq.length snd

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
    in
      (* It's not correct to just call `loop snd a 1`, because this could
       * modify the input `snd`. We need to make sure that we are operating
       * on a copy. So, we'll open up the first iteration of the loop.
       *)
      if ds >= N orelse a < 0.0001 then
        snd
      else
        loop (Seq.tabulate (next snd a 1) N) (a*a) 2
    end

  fun allPass ds a snd =
    let
      val combed = delay ds a snd
      fun output j =
        let
          val k = j - ds
        in
          (1.0 - a*a) * (if k < 0 then 0.0 else Seq.nth combed k)
          - (a * Seq.nth combed j)
        end
    in
      Seq.tabulate output (Seq.length snd)
    end

  val par = ForkJoin.par

  fun par4 (a, b, c, d) =
    let
      val ((ar, br), (cr, dr)) =
        par (fn _ => par (a, b), fn _ => par (c, d))
    in
      (ar, br, cr, dr)
    end

  (* Essentially mu-law compression. Normalizes to [-1,+1] and compresses
   * the dynamic range slightly. The boost parameter should be >= 1. *)
  fun compress boost snd =
    if boost < 1.0 then
      raise Fail ("Compression boost parameter must be at least 1")
    else
      let
        (* maximum amplitude *)
        val maxA =
          SeqBasis.reduce 10000 Real.max 1.0 (0, Seq.length snd)
            (fn i => Real.abs (Seq.nth snd i))

        (* a little buffer of intensity to avoid distortion *)
        val maxA' = 1.05 * maxA

        val scale = Math.ln (1.0 + boost)

        fun transfer x =
          let
            (* normalized *)
            val x' = Real.abs (x / maxA')
          in
            (* compressed *)
            Real.copySign (Math.ln (1.0 + boost * x') / scale, x)
          end
      in
        Seq.map transfer snd
      end

  fun shiftBy n s i =
    if i < n then
      0.0
    else if i < Seq.length s + n then
      Seq.nth s (i-n)
    else
      0.0

  fun reverb dry =
    let
      val N = Seq.length dry

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
       *)
      val (c1, c2, c3, c4) =
        par4 (fn _ => delay 1931 0.7 dry,
              fn _ => delay 2213 0.7 dry,
              fn _ => delay 1747 0.7 dry,
              fn _ => delay 1559 0.7 dry)

      fun combs i =
        (Seq.nth c1 i +
         Seq.nth c2 i +
         Seq.nth c3 i +
         Seq.nth c4 i)

      val fused = Seq.tabulate combs N
      val fused = allPass 167 0.6 fused
      val fused = allPass 191 0.6 fused

      (* ==========================================
       * early reflections are single echos of
       * the dry sound that occur after around
       * 25ms delay
       *)
      val ed1 = 1013
      val ed2 = 1102
      val ed3 = 1300

      (* ==========================================
       * wet signal = dry + early + fused
       * the fused reflections start emerging after
       * approximately 35ms
       *)
      val fusedDelay = 1500

      val wet = Seq.tabulate (fn i =>
          shiftBy 0 dry i
          + 0.6 * (shiftBy ed1 dry i)
          + 0.5 * (shiftBy ed2 dry i)
          + 0.4 * (shiftBy ed3 dry i)
          + 0.75 * (shiftBy fusedDelay fused i))
        (N + fusedDelay)

    in
      (* wet *)
      compress 2.0 wet
    end

end
