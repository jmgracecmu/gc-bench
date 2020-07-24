structure GIF:
sig
  type pixel = Color.pixel
  type image = {height: int, width: int, data: pixel Seq.t}

  val write: string -> image -> unit
end =
struct

  type pixel = Color.pixel
  type image = {height: int, width: int, data: pixel Seq.t}

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
