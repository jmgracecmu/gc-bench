structure GIF:
sig
  type pixel = Color.pixel
  type image = {height: int, width: int, data: pixel Seq.t}

  (* A GIF color palette is a table of up to 256 colors, and
   * function for remapping the colors of an image. *)
  structure Palette:
  sig
    type t = {colors: pixel Seq.t, remap: image -> int Seq.t}
    val summarize: image -> t
    val quantized: (int * int * int) -> t
  end

  structure LZW:
  sig
    (* First step of compression. Remap an image with the given color
     * palette, and then generate the LZW-compressed code stream.
     * This inserts clear- and EOI codes. *)
    val codeStream: image -> Palette.t -> int Seq.t

    (* Second step of compression: pack the code stream into bits with
     * flexible bit-lengths. This step also inserts sub-block sizes.
     * The first argument is the number of colors. *)
    val packCodeStream: int -> int Seq.t -> Word8.word Seq.t
  end

  (* Write many images as an animation. All images must be the same dimension. *)
  val writeMany: string  (* output path *)
              -> int     (* microsecond delay between images *)
              -> {width: int, height: int, numImages: int, getImage: int -> image}
              -> unit

  val write: string -> image -> unit
end =
struct

  structure AS = ArraySlice

  type pixel = Color.pixel
  type image = {height: int, width: int, data: pixel Seq.t}

  fun err msg =
    raise Fail ("GIF: " ^ msg)

  fun stderr msg =
    (TextIO.output (TextIO.stdErr, msg); TextIO.output (TextIO.stdErr, "\n"))

  fun ceilLog2 n =
    if n <= 0 then
      err "ceilLog2: expected input at least 1"
    else
      (* Util.log2(x) computes 1 + floor(log_2(x)) *)
      Util.log2 (n-1)

  structure Palette =
  struct

    type t = {colors: pixel Seq.t, remap: image -> int Seq.t}

    fun makeQuantized (rqq, gqq, bqq) =
      let
        fun bucketSize numBuckets =
          Real.floor (1.0 + 255.0 / Real.fromInt numBuckets)
        fun bucketShift numBuckets =
          Word8.fromInt ((255 - (numBuckets-1)*(bucketSize numBuckets)) div 2)

        fun qi nb = fn c => Word8.toInt c div (bucketSize nb)
        fun qc nb = fn i => bucketShift nb + Word8.fromInt (i * bucketSize nb)

        fun makeQ nb =
          { numBuckets = nb
          , channelToIdx = qi nb
          , channelFromIdx = qc nb
          }

        val R = makeQ rqq
        val G = makeQ gqq
        val B = makeQ bqq
        val numQuantized = (* this should be at most 256 *)
          List.foldl op* 1 (List.map #numBuckets [R, G, B])

        fun quantizeIdx {red, green, blue} =
          (#channelToIdx B blue) +
          (#channelToIdx G green) * (#numBuckets B) +
          (#channelToIdx R red) * (#numBuckets B) * (#numBuckets G)

        fun colorOfQuantizeIdx i =
          let
            val b = i mod (#numBuckets B)
            val g = (i div (#numBuckets B)) mod (#numBuckets G)
            val r = (i div (#numBuckets B) div (#numBuckets G)) mod (#numBuckets R)
          in
            { red = #channelFromIdx R r
            , green = #channelFromIdx G g
            , blue = #channelFromIdx B b
            }
          end
      in
        (numQuantized, quantizeIdx, colorOfQuantizeIdx)
      end

    fun quantized qpackage =
      let
        val (numQuantized, quantizeIdx, colorOfQuantizeIdx) =
          makeQuantized qpackage
      in
        { colors = Seq.tabulate colorOfQuantizeIdx numQuantized
        , remap = fn ({data, ...}: image) =>
            AS.full (SeqBasis.tabulate 1000 (0, Seq.length data) (fn i =>
              quantizeIdx (Seq.nth data i)))
        }
      end

    (* Here's a simple algorithm to choose 256 colors based on sampling.
     * First, we blindly choose 216 = 6*6*6 quantized colors. This leaves
     * 40 colors which can be chosen to better represent the image. (But of
     * course, we can always fall back on the quantized colors to get an
     * "okay" result...) To pick the 40 representative colors, we sample a
     * bunch of pixels, deduplicate them and then choose (up to) 40 of the
     * most frequently occuring within the sample. Not exactly the best
     * algorithm, but oh well.
     *)
    fun summarize ({data, width, height}: image) =
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

        val (numQuantized, quantizeIdx, colorOfQuantizeIdx) =
          makeQuantized (6, 6, 6)

        val dist = Color.approxHumanPerceptionDistance

        fun remapOne color =
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

        fun remap {width, height, data} =
          AS.full (SeqBasis.tabulate 100 (0, Seq.length data)
                    (remapOne o Seq.nth data))

        val palette =
          Seq.tabulate (fn i =>
            if i < numQuantized then
              colorOfQuantizeIdx i
            else
              #1 (Seq.nth histogram (i - numQuantized)))
          256
      in
        {colors = palette, remap = remap}
      end

  end

  structure LZW =
  struct

    (* ===================================================================
     * The code table maps index sequences to codes *)
    structure CodeTable:
    sig
      type t
      type idx = int
      type code = int

      val new: int -> t (* `new numColors` *)
      val clear: t -> unit
      val insert: (code * idx) -> t -> bool (* returns false when full *)
      val maybeLookup: (code * idx) -> t -> code option
    end =
    struct
      type idx = int
      type code = int

      exception Invalid

      type t =
        { nextCode: int ref
        , numColors: int
        , table: code array
        }

      fun new numColors =
        { nextCode = ref (Util.boundPow2 numColors + 2)
        , numColors = numColors
        , table = Array.array (4096 * numColors, ~1)
        }

      fun clear {nextCode, numColors, table} =
        ( Util.for (0, Array.length table) (fn i => Array.update (table, i, ~1))
        ; nextCode := Util.boundPow2 numColors + 2
        )

      fun insert (code, idx) ({nextCode, numColors, table}: t) =
        if !nextCode = 4095 then
          false (* GIF limits the maximum code number to 4095 *)
        else
          ( Array.update (table, code*numColors + idx, !nextCode)
          ; nextCode := !nextCode + 1
          ; true
          )

      fun maybeLookup (code, idx) ({table, numColors, ...}: t) =
        case Array.sub (table, code*numColors + idx) of
          ~1 => NONE
        | c => SOME c

    end
    (* =================================================================== *)

    structure T = CodeTable

    fun codeStream image (palette as {colors, remap}) =
      let
        val colorIndices = remap image
        fun colorIdx i = Seq.nth colorIndices i

        val numColors = Seq.length colors
        val clear = Util.boundPow2 numColors
        val eoi = clear + 1

        val table = T.new numColors

        fun finish stream =
          Seq.rev (Seq.fromList (eoi :: stream))

        (* The buffer is implicit, represented instead by currentCode
         * i is the next index into `colorIndices` *)
        fun loop stream currentCode i =
          if i >= Seq.length colorIndices then
            finish (currentCode :: stream)
          else
            case T.maybeLookup (currentCode, colorIdx i) table of
              SOME code => loop stream code (i+1)
            | NONE =>
                if T.insert (currentCode, colorIdx i) table then
                  loop (currentCode :: stream) (colorIdx i) (i+1)
                else
                  ( T.clear table
                  ; loop (clear :: currentCode :: stream) (colorIdx i) (i+1)
                  )
      in
        if Seq.length colorIndices = 0 then
          err "empty color index sequence"
        else
          loop [clear] (colorIdx 0) 1
      end

    fun packCodeStream numColors codes =
      let
        val n = Seq.length codes
        fun code i = Seq.nth codes i
        val clear = Util.boundPow2 numColors
        val eoi = clear+1
        val minCodeSize = ceilLog2 numColors
        val firstCodeWidth = minCodeSize+1

        (* Begin by calculating the bit width of each code. Since we know bit
         * widths are reset at each clear code, we can parallelize by splitting
         * the codestream into segments delimited by clear codes and processing
         * the segments in parallel.
         *
         * Within a segment, the width is increased every time we generated
         * a new code with power-of-two width. Every symbol in the code stream
         * corresponds to a newly generated code.
         *)

        val clears =
          AS.full (SeqBasis.filter 2000 (0, n) (fn i => i) (fn i => code i = clear))
        val numClears = Seq.length clears

        val widths = ForkJoin.alloc n
        val _ = Array.update (widths, 0, firstCodeWidth)
        val _ = ForkJoin.parfor 1 (0, numClears) (fn c =>
          let
            val i = 1 + Seq.nth clears c
            val j = if c = numClears-1 then n else 1 + Seq.nth clears (c+1)

            (* max code in table, up to (but not including) index k *)
            fun currentMaxCode k =
              k - i  (* num outputs since the table was cleared *)
              + eoi  (* the max code immediately after clearing the table *)
          in
            Util.loop (i, j) firstCodeWidth (fn (currWidth, k) =>
              ( Array.update (widths, k, currWidth)
              ; if currentMaxCode (k+1) = Util.pow2 currWidth then
                  currWidth+1
                else
                  currWidth
              ));
            ()
          end)
        val widths = AS.full widths

        val totalBitWidth = Seq.reduce op+ 0 widths
        val packedSize = Util.ceilDiv totalBitWidth 8

        val packed = ForkJoin.alloc packedSize

        fun flushBuffer (oi, buffer, used) =
          if used < 8 then
            (oi, buffer, used)
          else
            ( Array.update (packed, oi, Word8.fromLarge buffer)
            ; flushBuffer (oi+1, LargeWord.>> (buffer, 0w8), used-8)
            )

        (* Input index range [ci,cj)
         * Output index range [oi, oj)
         * `buffer` is a partially filled byte that has not yet been written
         * to the packed. `used` (0 to 7) is how much of that byte is
         * used. *)
        fun pack (oi, oj) (ci, cj) (buffer: LargeWord.word) (used: int) =
          if ci >= cj then
            (if oi >= oj then
              ()
            else if oi = oj-1 then
              Array.update (packed, oi, Word8.fromLarge buffer)
            else
              err "cannot fill rest of packed region")
          else
            let
              val thisCode = code ci
              val thisWidth = Seq.nth widths ci
              val buffer' =
                LargeWord.orb (buffer,
                  LargeWord.<< (LargeWord.fromInt thisCode, Word.fromInt used))
              val used' = used + thisWidth
              val (oi', buffer'', used'') = flushBuffer (oi, buffer', used')
            in
              pack (oi', oj) (ci+1, cj) buffer'' used''
            end

        val _ = pack (0, packedSize) (0, n) 0w0 0
        val packed = AS.full packed
        val numBlocks = Util.ceilDiv packedSize 255
        val output = ForkJoin.alloc (packedSize + numBlocks + 1)
      in
        ForkJoin.parfor 10 (0, numBlocks) (fn i =>
          let
            val size = if i < numBlocks-1 then 255 else packedSize - 255*i
          in
            Array.update (output, 256*i, Word8.fromInt size);
            Util.for (0, size) (fn j =>
              Array.update (output, 256*i + 1 + j, Seq.nth packed (255*i + j)))
          end);

        Array.update (output, packedSize + numBlocks, 0w0);

        AS.full output
      end
  end

  (* ====================================================================== *)

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

  fun writeMany path delay {width, height, numImages, getImage} =
    if numImages <= 0 then
      err "Must be at least one image"
    else
    let
      val width16 = checkToWord16 "width" width
      val height16 = checkToWord16 "height" height

      val palette = Palette.quantized (6,7,6)
      val numberOfColors = Seq.length (#colors palette)

      val imageData =
        AS.full (SeqBasis.tabulate 1 (0, numImages) (fn i =>
          let
            val img = getImage i
          in
            if #width img <> width orelse #height img <> height then
              err "Not all images the same dimensions"
            else
              LZW.packCodeStream numberOfColors (LZW.codeStream img palette)
          end))

      val file = BinIO.openOut path
      val w8 = ExtraBinIO.w8 file
      val w32b = ExtraBinIO.w32b file
      val w32l = ExtraBinIO.w32l file
      val w16l = ExtraBinIO.w16l file
      val wrgb = ExtraBinIO.wrgb file
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

      Util.for (0, numberOfColors) (fn i =>
        wrgb (Seq.nth (#colors palette) i));

      Util.for (numberOfColors, Util.boundPow2 numberOfColors) (fn i =>
        wrgb Color.black);

      (* ==================================
       * application extension, for looping
       * OPTIONAL. skip it if there is only one image.
       *)

      if numImages = 1 then () else
      List.app (w8 o Word8.fromInt)
      [ 0x21, 0xFF, 0x0B, 0x4E, 0x45, 0x54, 0x53, 0x43, 0x41, 0x50, 0x45, 0x32
      , 0x2E, 0x30, 0x03, 0x01, 0x00, 0x00, 0x00
      ];

      (* ==================================
       * IMAGE DATA
       *)

      Util.for (0, numImages) (fn i =>
        let
          val bytes = Seq.nth imageData i
        in
          (* ==========================
           * graphics control extension.
           * OPTIONAL. only needed if
           * doing animation.
           *)

          if numImages = 1 then () else
          ( List.app (w8 o Word8.fromInt) [ 0x21, 0xF9, 0x04, 0x04 ]
          ; w16l (Word16.fromInt delay)
          ; w8 0w0
          ; w8 0w0
          );

          (* ==========================
           * image descriptor
           *)

          w8 0wx2C; (* image separator *)

          w16l 0w0;  (* image left *)
          w16l 0w0;  (* image top *)

          w16l width16;  (* image width *)
          w16l height16; (* image height *)

          w8 0w0;   (* packed local color table descriptor (NONE FOR NOW) *)

          (* ===========================
           * compressed image data
           *)

          w8 (Word8.fromInt (ceilLog2 numberOfColors));
          Util.for (0, Seq.length bytes) (fn i =>
            w8 (Seq.nth bytes i))
        end);

      (* ================================
       * trailer
       *)

      w8 0wx3B;

      BinIO.closeOut file
    end

  fun write path img =
    writeMany path 0
      { width = #width img
      , height = #height img
      , numImages = 1
      , getImage = (fn _ => img)
      }
end
