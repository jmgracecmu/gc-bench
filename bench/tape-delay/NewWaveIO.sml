structure NewWaveIO:
sig
  (* A sound is a sequence of samples at 44.1K Hz.
   * Each sample is in range [-1.0, +1.0]. *)
  val readSound : string -> real Seq.t
  val writeSound : real Seq.t -> string -> unit
end =
struct

  structure AS = ArraySlice

  fun err msg =
    raise Fail ("NewWaveIO: " ^ msg)

  fun readSound path =
    let
      val bytes = ReadFile.contentsBinSeq path

      (* =======================================================
       * RIFF header, 12 bytes
       *)

      val _ =
        if Seq.length bytes >= 12 then ()
        else err "not enough bytes for RIFF header"

      val offset = 0
      val riff = 0wx52494646 (* ascii "RIFF", big endian *)
      val _ =
        if Parse.r32b bytes offset = riff then ()
        else err "expected 'RIFF' chunk ID"

      (* the chunkSize should be the size of the "rest of the file" *)
      val offset = 4
      val chunkSize = Word32.toInt (Parse.r32l bytes offset)

      val totalFileSize = 8 + chunkSize
      val _ =
        if Seq.length bytes >= totalFileSize then ()
        else err ("expected " ^ Int.toString totalFileSize ^
                  " bytes but the file is only " ^
                  Int.toString (Seq.length bytes))

      val offset = 8
      val wave = 0wx57415645 (* ascii "WAVE" big endian *)
      val _ =
        if Parse.r32b bytes offset = wave then ()
        else err "expected 'WAVE' format"

      (* =======================================================
       * fmt subchunk, should be 24 bytes total for PCM
       *)

      val _ =
        if Seq.length bytes >= 12+24 then ()
        else err "not enough bytes for fmt subchunk"

      val offset = 12
      val fmtId = 0wx666d7420 (* ascii "fmt " big endian *)
      val _ =
        if Parse.r32b bytes offset = fmtId then ()
        else err "expected 'fmt ' chunk ID"

      val offset = 16
      val chunk1Size = Word32.toInt (Parse.r32l bytes offset)
      val _ =
        if chunk1Size = 16 then ()
        else err "expected 'fmt ' chunk to be 16 bytes"

      val offset = 20
      val audioFormat = Word16.toInt (Parse.r16l bytes offset)
      val _ =
        if audioFormat = 1 then ()
        else err "expected PCM audio format"

      val offset = 22
      val numChannels = Word16.toInt (Parse.r16l bytes offset)

      val offset = 24
      val sampleRate = Word32.toInt (Parse.r32l bytes offset)

      val offset = 28
      val byteRate = Word32.toInt (Parse.r32l bytes offset)

      val offset = 32
      val blockAlign = Word16.toInt (Parse.r16l bytes offset)

      val offset = 34
      val bitsPerSample = Word16.toInt (Parse.r16l bytes offset)
      val bytesPerSample = bitsPerSample div 8

      (* =======================================================
       * data subchunk, should be the rest of the file
       *)

      val _ =
        if Seq.length bytes >= 12+24+8 then ()
        else err "not enough bytes for data subchunk"

      val offset = 36
      val dataId = 0wx64617461 (* ascii "data" big endian *)
      val _ =
        if Parse.r32b bytes offset = dataId then ()
        else err "expected 'data' chunk ID"

      val offset = 40
      val dataSize = Word32.toInt (Parse.r32l bytes offset)
      val _ =
        if 12 + 24 + 8 + dataSize = totalFileSize then ()
        else err ("expected data chunk of size " ^
                  Int.toString (totalFileSize - 12 - 24 - 8) ^
                  " but found " ^
                  Int.toString dataSize)

      val dataStart = 44

      val numSamples = (dataSize div numChannels) div bytesPerSample

      fun readSample8 pos =
        Real.fromInt (Word8.toInt (Seq.nth bytes pos) - 128) / 256.0
      fun readSample16 pos =
        Real.fromInt (Word16.toIntX (Parse.r16l bytes pos)) / 32768.0

      val readSample =
        case bytesPerSample of
          1 => readSample8
        | 2 => readSample16
        | _ => err "only 8-bit and 16-bit samples supported at the moment"

      (* read the ith channel *)
      fun readChannel i =
        AS.full (SeqBasis.tabulate 1000 (0, numSamples) (fn j =>
          readSample (dataStart + j * (numChannels * bytesPerSample) + i)))
    in
      if numChannels = 1 then ()
      else err "only mono (1 channel) files are supported at the moment";

      if sampleRate = 44100 then ()
      else err "only 44100 kHz samplerate is supported at the moment";

      readChannel 0
    end

  (* =======================================================================
   * some utilities for writing
   *)

  fun w8 file (w: Word8.word) = BinIO.output1 (file, w)

  fun w64b file (w: Word64.word) =
    let
      val w8 = w8 file
      open Word64
      infix 2 >> andb
    in
      w8 (Word8.fromLarge (w >> 0w56));
      w8 (Word8.fromLarge (w >> 0w48));
      w8 (Word8.fromLarge (w >> 0w40));
      w8 (Word8.fromLarge (w >> 0w32));
      w8 (Word8.fromLarge (w >> 0w24));
      w8 (Word8.fromLarge (w >> 0w16));
      w8 (Word8.fromLarge (w >> 0w8));
      w8 (Word8.fromLarge w)
    end

  fun w32b file (w: Word32.word) =
    let
      val w8 = w8 file
      val w = Word32.toLarge w
      open Word64
      infix 2 >> andb
    in
      w8 (Word8.fromLarge (w >> 0w24));
      w8 (Word8.fromLarge (w >> 0w16));
      w8 (Word8.fromLarge (w >> 0w8));
      w8 (Word8.fromLarge w)
    end

  fun w32l file (w: Word32.word) =
    let
      val w8 = w8 file
      val w = Word32.toLarge w
      open Word64
      infix 2 >> andb
    in
      w8 (Word8.fromLarge w);
      w8 (Word8.fromLarge (w >> 0w8));
      w8 (Word8.fromLarge (w >> 0w16));
      w8 (Word8.fromLarge (w >> 0w24))
    end

  fun w16b file (w: Word16.word) =
    let
      val w8 = w8 file
      val w = Word16.toLarge w
      open Word64
      infix 2 >> andb
    in
      w8 (Word8.fromLarge (w >> 0w8));
      w8 (Word8.fromLarge w)
    end

  fun w16l file (w: Word16.word) =
    let
      val w8 = w8 file
      val w = Word16.toLarge w
      open Word64
      infix 2 >> andb
    in
      w8 (Word8.fromLarge w);
      w8 (Word8.fromLarge (w >> 0w8))
    end

  (* ====================================================================== *)

  fun writeSound snd path =
    let
      val file = BinIO.openOut path

      val w32b = w32b file
      val w32l = w32l file
      val w16l = w16l file

      val totalBytes =
        44 + (Seq.length snd * 2)

      val riffId = 0wx52494646 (* ascii "RIFF", big endian *)
      val fmtId = 0wx666d7420 (* ascii "fmt " big endian *)
      val wave = 0wx57415645 (* ascii "WAVE" big endian *)
      val dataId = 0wx64617461 (* ascii "data" big endian *)
    in
      (* ============================
       * RIFF header, 12 bytes *)
      w32b riffId;
      w32l (Word32.fromInt (totalBytes - 8));
      w32b wave;

      (* ============================
       * fmt subchunk, 24 bytes *)
      w32b fmtId;
      w32l 0w16;    (* 16 remaining bytes in subchunk *)
      w16l 0w1;     (* audio format PCM = 1 *)
      w16l 0w1;     (* 1 channel (mono) *)
      w32l 0w44100; (* 44.1 kHz sample rate *)
      w32l (0w44100 * 0w2); (* "byte rate" = sampleRate * numChannels * bytesPerSample *)
      w16l 0w2;     (* "block align" = numChannels * bytesPerSample *)
      w16l 0w16;    (* bits per sample *)

      (* ============================
       * data subchunk: rest of file *)
      w32b dataId;
      w32l (Word32.fromInt (2 * Seq.length snd)); (* number of data bytes *)

      Util.for (0, Seq.length snd) (fn i =>
        let
          val s = Seq.nth snd i
          val s =
            if s < ~1.0 then ~1.0
            else if s > 1.0 then 1.0
            else s
          val s = Real.round (s * 32767.0)
          val s = if s < 0 then s + 65536 else s
        in
          w16l (Word16.fromInt s)
        end);

      BinIO.closeOut file
    end
end
