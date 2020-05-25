(* already provided by the compiler *)
structure ForkJoin = ForkJoin

structure Concurrency =
struct
  val numberOfProcessors = MLton.Parallel.numberOfProcessors

  (* val compareAndSwap = ... *)
end
