fun binarySearch seq value =
let
  fun binarySearchRec start fin =
  let
    val middle = (fin - start) div 2 + start
    (*
    val _ = print ("middle " ^ (Int.toString middle) ^ "\n")
    *)
  in
    if start = fin then start
    else if value < Seq.nth seq middle then
      binarySearchRec start middle
    else if value = Seq.nth seq middle then
      middle
    else
      binarySearchRec (middle + 1) fin
  end
in
  binarySearchRec 0 (Seq.length seq)
end

fun bisplitRec seq1 seq2 k offset1 offset2 =
let
  fun min i j = if i < j then i else j
  val lenReal1 = Real.fromInt(Seq.length seq1)
  val lenReal2 = Real.fromInt(Seq.length seq2)
  val kReal = Real.fromInt (k - offset1 - offset2)
  (* splitGuess1 is the index of the first thing of the right hand sequence
  val _ = print "\nseq1\n"
  val _ = Seq.map (fn x => print ((Int.toString x) ^ " ")) seq1
  val _ = print "\nseq2\n"
  val _ = Seq.map (fn x => print ((Int.toString x) ^ " ")) seq2
  val _ = print "\n"
  val _ = print ("off " ^ (Int.toString offset1) ^ (Int.toString offset2) ^ "\n")
  *)
  val splitGuess1 = Real.floor(kReal / (lenReal1 + lenReal2) * lenReal1)
  val maxSplitGuess1 = min splitGuess1 ((Seq.length seq1) - 1)
  val splitGuess2 = binarySearch seq2 (Seq.nth seq1 maxSplitGuess1)
in
  if k < splitGuess1 + splitGuess2 + offset1 + offset2 then
  (* what percent of the total number of elements is k? Use this percentage
  * to guess where to start in seq1
  *)
  let
    val seq1' = Seq.take seq1 splitGuess1
    val seq2' = Seq.take seq2 splitGuess2
    val (seq2i, seq1i) = bisplitRec seq2' seq1' k offset2 offset1
  in
    (seq1i, seq2i)
  end
  else if k = splitGuess1 + splitGuess2 + offset1 + offset2 then
    (splitGuess1 + offset1, splitGuess2 + offset2)
  else if k = splitGuess1 + splitGuess2 + offset1 + offset2 + 1 then
    (splitGuess1 + offset1 + 1, splitGuess2 + offset2)
  else
  let
    val seq1' = Seq.drop seq1 splitGuess1
    val seq2' = Seq.drop seq2 splitGuess2
    val (seq2i, seq1i) = bisplitRec seq2' seq1' k (splitGuess2 + offset2) (splitGuess1 + offset1)  
  in
    (seq1i, seq2i)
  end
end

fun bisplit seq1 seq2 k = bisplitRec seq1 seq2 k 0 0

fun seqUpdate seq n x = ArraySlice.update (seq, n, x)

(* the original sequence should be updated when we update these subsequences *)
fun mergeLinearRec seq1 seq2 dest i1 i2 offset =
  if i1 = Seq.length seq1 andalso i2 = Seq.length seq2 then
      ()
  else if i1 = Seq.length seq1 then
    (* copy the rest of seq2 to dest then return *)
    (seqUpdate dest offset (Seq.nth seq2 i2);
    mergeLinearRec seq1 seq2 dest i1 (i2 + 1) (offset + 1))
  else if i2 = Seq.length seq2 then
    (* copy the rest of seq2 to dest then return *)
    (seqUpdate dest offset (Seq.nth seq1 i1);
    mergeLinearRec seq1 seq2 dest (i1 + 1) i2 (offset + 1))
  else if Seq.nth seq1 i1 < Seq.nth seq2 i2 then
    (seqUpdate dest offset (Seq.nth seq1 i1);
    mergeLinearRec seq1 seq2 dest (i1 + 1) i2 (offset + 1))
  else
    (seqUpdate dest offset (Seq.nth seq2 i2);
    mergeLinearRec seq1 seq2 dest i1 (i2 + 1) (offset + 1))

fun mergeLinear seq1 seq2 dest = mergeLinearRec seq1 seq2 dest 0 0 0

fun mergeLog seq1 seq2 dest =
let
  val gran = 10000
  (*
  val _ = print "things to merge\n"
  val _ = print (Seq.toString (fn x => Int.toString x) seq1);
  val _ = print "\n";
  val _ = print (Seq.toString (fn x => Int.toString x) seq2);
  val _ = print "\n";
  val _ = print "\n";
  *)
  fun smallMerge startIndex endIndex =
  let
    val (lowBoundSeq1, lowBoundSeq2) = bisplit seq1 seq2 startIndex
    val (hiBoundSeq1, hiBoundSeq2) = bisplit seq1 seq2 endIndex
    (*
    val _ = print "label 1\n"
    val _ = print ((Int.toString startIndex) ^ " " ^ (Int.toString endIndex))
    val _ = print "\n"
    val _ = print "label 2\n"
    val _ = print ((Int.toString startIndex) ^ " " ^ (Int.toString endIndex))
    val _ = print "\n"
    val _ = print "label 3\n"
    val _ = print ((Int.toString startIndex) ^ " " ^ (Int.toString endIndex))
    val _ = print "\n"
    *)
    val subseq1 = Seq.subseq seq1 (lowBoundSeq1, hiBoundSeq1 - lowBoundSeq1)
    val subseq2 = Seq.subseq seq2 (lowBoundSeq2, hiBoundSeq2 - lowBoundSeq2)
    val subdest = Seq.subseq dest (startIndex, endIndex - startIndex)
  in
    mergeLinear subseq1 subseq2 subdest
  end
  val chunkSize = Real.floor (Math.ln (Real.fromInt (Seq.length dest)))
  val numberProcs = (Seq.length dest) div chunkSize
  (*
  val _ = print ("chunksize" ^ (Int.toString chunkSize) ^ "\n")
  val _ = print ("numberProcs" ^ (Int.toString numberProcs) ^ "\n")
  *)
  fun forEachProc i = smallMerge (i * chunkSize) ((i + 1) * chunkSize)
in
  ForkJoin.parfor gran (0, numberProcs) forEachProc;
  smallMerge (numberProcs * chunkSize) (Seq.length dest)
end

fun mergeSortRec seq resultUp resultDown =
  if Seq.length seq = 1 then
    (seqUpdate resultDown 0 (Seq.nth seq 0); ())
  else
  let
    val firstHalfSize = (Seq.length seq) div 2
    val secondHalfSize = (Seq.length seq) - firstHalfSize
    val seqLeft = Seq.subseq seq (0, firstHalfSize)
    val seqRight = Seq.subseq seq (firstHalfSize, secondHalfSize)
    val resultUpLeft = Seq.subseq resultUp (0, firstHalfSize)
    val resultUpRight = Seq.subseq resultUp (firstHalfSize, secondHalfSize)
    val resultDownLeft = Seq.subseq resultDown (0, firstHalfSize)
    val resultDownRight = Seq.subseq resultDown (firstHalfSize, secondHalfSize)
    val _ = mergeSortRec seqLeft resultDownLeft resultUpLeft
    val _ = mergeSortRec seqRight resultDownRight resultUpRight
  in
    if Seq.length resultDown < 100 then
      mergeLinear resultUpLeft resultUpRight resultDown
    else
      mergeLog resultUpLeft resultUpRight resultDown
  end

fun mergeSort seq =
let
  val resultUp = ArraySlice.full (ForkJoin.alloc (Seq.length seq))
  val resultDown = ArraySlice.full (ForkJoin.alloc (Seq.length seq))
  val _ = mergeSortRec seq resultUp resultDown
in
  resultDown
end
