structure GIF:
sig
  type pixel = Color.pixel
  type image = {height: int, width: int, data: pixel Seq.t}

  (* LZW compression is one of the substeps of generating a GIF.
   * I've broken it up here into three parts:
   *   1. choosing a color palette (and renaming each pixel with color indices)
   *   2. generating a code stream from the color indices
   *   3. packing the code stream into a bit stream.
   *)
  structure LZW:
  sig
    (* Simplify colors: choose up to 256 colors, and replace each pixel
     * with its color index. The color table will be used as the GIF
     * color table. *)
    val chooseColors: image -> {colors: pixel Seq.t, indices: int Seq.t}

    (* Using the color indices given, generate the code stream. Note that
     * this step inserts clear- and EOI codes. The clear-code is
     * #numColors, and the EOI code is #(numColors+1). *)
    val codeStream: {numColors: int, indices: int Seq.t} -> int Seq.t

    (* Final step of LZW compression: pack the code stream into bits with
     * flexible bit-lengths. This step also inserts sub-block sizes. *)
    val packCodeStream: int Seq.t -> Word8.word Seq.t
  end

  val write: string -> image -> unit
end =
struct

  structure AS = ArraySlice

  type pixel = Color.pixel
  type image = {height: int, width: int, data: pixel Seq.t}

  structure LZW =
  struct

    (* GIFs allow at most 256 colors. Here's a simple algorithm to choose 256
     * colors based on sampling.
     *
     * First, we blindly choose 216 = 6*6*6 quantized colors. This leaves
     * 40 colors which can be chosen to better represent the image. (But of
     * course, we can always fall back on the quantized colors to get an
     * "okay" result...)
     *
     * To pick the 40 representative colors, we sample a bunch of pixels,
     * deduplicate them and then choose (up to) 40 of the most frequently
     * occuring within the sample.
     *
     * Not exactly the best algorithm, but oh well.
     *)
    fun chooseColors ({data, width, height}: image) =
      let
        val n = Seq.length data

        val sampleSize = 512
        val sample =
          if n <= sampleSize then
            data
          else
            Seq.tabulate (fn i => Seq.nth data ((Util.hash i) mod n)) sampleSize

        val sampleSize = Seq.length sample
        val sample = Mergesort.sort Color.compare sample

        val offsets =
          AS.full (SeqBasis.filter 2000 (0, sampleSize+1)
            (fn i => i)
            (fn i => i = 0 orelse i = sampleSize orelse
              Seq.nth sample (i-1) <> Seq.nth sample i))
        fun off i = Seq.nth offsets i
        val numUniq = Seq.length offsets - 1

        (* Take first 40 of most frequent colors. Note that the comparison
         * is swapped to put most frequent at front. *)
        val histogram =
          Seq.take
            (Mergesort.sort (fn ((_, n1), (_, n2)) => Int.compare (n2, n1))
            (Seq.tabulate (fn i => (Seq.nth sample (off i), off (i+1) - off i)) numUniq))
          (Int.min (40, numUniq))

        (* the actual palette consists of
         *   + 216 quantized colors
         *   + 40 most frequent colors
         * which totals to 256 colors.
         *)

        (* fun q c = Word8.fromInt (43 * (Word8.toInt c div 43) + 20) *)
        fun qi c = Word8.toInt c div 43
        fun qc i = 0w20 + Word8.fromInt (43*i)
        (* fun quantize {red, green, blue} =
          {red = q red, green = q green, blue = q blue} *)
        fun quantizeIdx {red, green, blue} =
          36 * qi red + 6 * qi green + qi blue
        fun colorOfQuantizeIdx i =
          let
            val b = i mod 6
            val g = (i div 6) mod 6
            val r = (i div 36) mod 6
          in
            {red = qc r, green = qc g, blue = qc b}
          end
        val numQuantized = 216

        val dist = Color.approxHumanPerceptionDistance

        fun remap color =
          let
            val qIdx = quantizeIdx color
            val bestQuantized =
              (dist (color, colorOfQuantizeIdx qIdx), qIdx)
            val (_, bestIdx) =
              Util.loop (0, Seq.length histogram) bestQuantized
              (fn ((bestDist, bestIdx), i) =>
                let
                  val d = dist (color, #1 (Seq.nth histogram i))
                in
                  if d < bestDist then (d, i+numQuantized) else (bestDist, bestIdx)
                end)
          in
            bestIdx
          end

        val palette =
          Seq.tabulate (fn i =>
            if i < numQuantized then
              colorOfQuantizeIdx i
            else
              #1 (Seq.nth histogram (i - numQuantized)))
          256
      in
        { colors = palette
        , indices = AS.full (SeqBasis.tabulate 100 (0, n) (remap o Seq.nth data))
        }
      end

    (* TODO *)
    fun codeStream {numColors, indices} =
      let
      in
        Seq.empty ()
      end

    (* TODO *)
    fun packCodeStream codes =
      let
      in
        Seq.empty ()
      end
  end

  fun err msg =
    raise Fail ("GIF: " ^ msg)

  fun checkToWord16 thing x =
    if x >= 0 andalso x <= 65535 then
      Word16.fromInt x
    else
      err (thing ^ " must be non-negative and less than 2^16");

  fun packScreenDescriptorByte
        { colorTableFlag: bool
        , colorResolution: int
        , sortFlag: bool
        , colorTableSize: int
        } =
    let
      open Word8
      infix 2 << orb andb
    in
      ((if colorTableFlag then 0w1 else 0w0) << 0w7)
      orb
      ((fromInt colorResolution andb 0wx7) << 0w4)
      orb
      ((if sortFlag then 0w1 else 0w0) << 0w3)
      orb
      (fromInt colorTableSize andb 0wx7)
    end

  fun ceilLog2 n =
    if n <= 0 then
      err "ceilLog2: expected input at least 1"
    else
      (* Util.log2(x) computes 1 + floor(log_2(x)) *)
      Util.log2 (n-1)

  fun write path {height, width, data} =
    let
      val file = BinIO.openOut path

      val width16 = checkToWord16 "width" width
      val height16 = checkToWord16 "height" height

      val w8 = ExtraBinIO.w8 file
      val w32b = ExtraBinIO.w32b file
      val w32l = ExtraBinIO.w32l file
      val w16l = ExtraBinIO.w16l file
      val wrgb = ExtraBinIO.wrgb file

      (* some sample data
       * TODO: actually generate this from input... *)
      val width16 = 0w10;
      val height16 = 0w10;
      val numberOfColors = 4
    in
      (* ==========================
       * "GIF89a" header: 6 bytes
       *)

      List.app (w8 o Word8.fromInt) [0x47, 0x49, 0x46, 0x38, 0x39, 0x61];

      (* ===================================
       * logical screen descriptor: 7 bytes
       *)

      w16l width16;
      w16l height16;

      w8 (packScreenDescriptorByte
        { colorTableFlag  = true
        , colorResolution = 1
        , sortFlag        = false
        , colorTableSize  = (ceilLog2 numberOfColors) - 1
        });

      w8 0w0; (* background color index. just use 0 for now. *)

      w8 0w0; (* pixel aspect ratio ?? *)

      (* ===================================
       * global color table
       *)

      wrgb Color.white; (* just some sample data. TODO generate from input! *)
      wrgb Color.red;
      wrgb Color.blue;
      wrgb Color.black;

      (* ==================================
       * graphics control extension.
       * OPTIONAL, so for now, skip it.
       *)


      (* ==================================
       * image descriptor
       * each image has one!
       *)

      w8 0wx2C; (* image separator *)

      w16l 0w0;  (* image left *)
      w16l 0w0;  (* image top *)

      w16l width16;  (* image width *)
      w16l height16; (* image height *)

      w8 0w0;   (* packed local color table descriptor (NONE FOR NOW) *)

      (* =================================
       * compressed image data
       *)

      (* some sample data *)
      List.app (w8 o Word8.fromInt)
      [ 0x02, 0x16, 0x8C, 0x2D, 0x99, 0x87, 0x2A, 0x1C, 0xDC, 0x33, 0xA0, 0x02
      , 0x75, 0xEC, 0x95, 0xFA, 0xA8, 0xDE, 0x60, 0x8C, 0x04, 0x91, 0x4C, 0x01
      , 0x00
      ];

      (* ================================
       * trailer
       *)

      w8 0wx3B;

      BinIO.closeOut file
    end
end
