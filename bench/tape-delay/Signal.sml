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

end
