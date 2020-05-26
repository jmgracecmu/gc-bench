structure ForkJoin:
sig
  val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
  val parfor: int -> int * int -> (int -> unit) -> unit
  val alloc: int -> 'a array
end =
struct
  fun par (f, g) = (f (), g ())
  fun parfor (g:int) (lo, hi) (f: int -> unit) =
    if lo >= hi then () else (f lo; parfor g (lo+1, hi) f)
  fun alloc n = ArrayExtra.alloc n
end

structure Concurrency =
struct
  val numberOfProcessors = 1

  fun cas r (x, y) =
    let
      val current = !r
    in
      if MLton.eq (x, current) then r := y else ();
      current
    end

  fun casArray (a, i) (x, y) =
    let
      val current = Array.sub (a, i)
    in
      if MLton.eq (x, current) then Array.update (a, i, y) else ();
      current
    end
end

structure ReadFile =
struct

  fun contentsSeq path =
    let
      val (file, length) =
        let
          open Posix.FileSys
          val file = openf (path, O_RDONLY, O.fromWord 0w0)
        in
          (file, Position.toInt (ST.size (fstat file)))
        end

      open Posix.IO

      val bufferSize = 100000
      val buffer = Word8ArrayExtra.alloc length
      val result = ArrayExtra.alloc length
      (* val result = Word8ArrayExtra.alloc length *)

      (* fun copyToResult i n =
        Word8ArraySlice.copy
          { src = Word8ArraySlice.slice (buffer, 0, SOME n)
          , dst = result
          , di = i
          } *)

      fun copyToResult i n =
        Word8ArraySlice.appi (fn (j, b) =>
          Unsafe.Array.update (result, i+j, Char.chr (Word8.toInt b)))
          (Word8ArraySlice.slice (buffer, 0, SOME n))

      fun dumpFrom i =
        if i >= length then () else
        let
          val bytesRead = readArr (file, Word8ArraySlice.full buffer)
        in
          copyToResult i bytesRead;
          dumpFrom (i + bytesRead)
        end
    in
      dumpFrom 0;
      close file;
      ArraySlice.full result
    end

  fun contents filename =
    let
      val chars = contentsSeq filename
    in
      CharVector.tabulate (ArraySlice.length chars,
        fn i => ArraySlice.sub (chars, i))
    end
end
