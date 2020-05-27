(* already provided by the compiler *)
structure ForkJoin = ForkJoin

structure Concurrency =
struct
  val numberOfProcessors = MLton.Parallel.numberOfProcessors
  val cas = MLton.Parallel.compareAndSwap
  val casArray = MLton.Parallel.arrayCompareAndSwap
end

structure ReadFile =
struct

  fun contentsSeq filename =
    let
      val file = MPL.File.openFile filename
      val n = MPL.File.size file
      val arr = ForkJoin.alloc n
      val k = 10000
      val m = 1 + (n-1) div k
    in
      ForkJoin.parfor 1 (0, m) (fn i =>
        let
          val lo = i*k
          val hi = Int.min ((i+1)*k, n)
        in
          MPL.File.readChars file lo (ArraySlice.slice (arr, lo, SOME (hi-lo)))
        end);
      MPL.File.closeFile file;
      ArraySlice.full arr
    end

  fun contents filename =
    let
      val chars = contentsSeq filename
    in
      CharVector.tabulate (ArraySlice.length chars,
        fn i => ArraySlice.sub (chars, i))
    end

end
