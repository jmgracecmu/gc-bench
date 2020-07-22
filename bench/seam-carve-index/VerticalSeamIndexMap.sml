structure VerticalSeamIndexMap :>
sig
  type t
  type seam = int Seq.t

  (* `new (height, width)` *)
  val new: (int * int) -> t

  (* Remaps from some (H, W) into (H', W'). For vertical seams, we always
   * have H = H'. This signature could be reused for horizontal seams, though.
   *)
  val domain: t -> (int * int)
  val range: t -> (int * int)

  (* Remap an index (i, j) to some (i', j'), where i is a row index and j
   * is a column index. With vertical seams, i = i'.
   *)
  val remap: t -> (int * int) -> (int * int)

  (* Remove the given seam.
   * Causes all (i, j) on the right of the seam to be remapped to (i, j+1).
   *)
  val carve: t -> seam -> t
end =
struct

  structure AS = ArraySlice

  type t = {displacement: int Seq.t, domain: int * int, range: int * int}
  type seam = int Seq.t

  fun new (height, width) =
    { displacement = AS.full (SeqBasis.tabulate 4000 (0, width*height) (fn _ => 0))
    , domain = (height, width)
    , range = (height, width)
    }

  fun domain ({domain=d, ...}: t) = d
  fun range ({range=r, ...}: t) = r

  fun remap ({displacement, domain=(h, w), ...}: t) (i, j) =
    (i, j + Seq.nth displacement (i*w + j))

  fun carve ({displacement, domain=(h, w), range=r}: t) seam =
    { domain = (h, w-1)
    , range = r
    , displacement = AS.full (SeqBasis.tabulate 1000 (0, (w-1)*h) (fn k =>
        let
          val i = k div (w-1)
          val j = k mod (w-1)
          val s = Seq.nth seam i
        in
          if j < s then
            Seq.nth displacement (i*w + j)
          else
            1 + Seq.nth displacement (i*w + j)
        end))
    }

end
