
fun testMergeSort1 _ =
let
  val seq = Seq.fromList [9,8,7,6,5,4,3,2,1,0]
  val answer = Seq.fromList [0,1,2,3,4,5,6,7,8,9]
  val sorted = mergeSort seq
in
  if Seq.equal (fn (x, y) => x = y) (sorted, answer) then
    (print "pass mergeSort seq\n";
    ())
  else
    (print "fail mergeSort seq\n";
    print (Seq.toString (fn x => Int.toString x) sorted);
    print "\n";
    ())
end

fun testMergeSort2 _ =
let
  val size = 1000009
  val seq = Seq.tabulate (fn i => size - i - 1) size
  val answer = Seq.tabulate (fn i => i) size
  val sorted = mergeSort seq
in
  if Seq.equal (fn (x, y) => x = y) (sorted, answer) then
    (print "pass mergeSort2 seq\n";
    ())
  else
    (print "fail mergeSort2 seq\n";
    print (Seq.toString (fn x => Int.toString x) sorted);
    print "\n";
    ())
end

fun testMergeLinear1 _ =
let
  val dest = Seq.tabulate (fn _ => 0) 10
  val seq1 = Seq.tabulate (fn x => 2 * x) 5
  val seq2 = Seq.tabulate (fn x => 2 * x + 1) 5
  val answer = Seq.tabulate (fn x => x) 10
in
  mergeLinear seq1 seq2 dest;
  if Seq.equal (fn (x, y) => x = y) (dest, answer) then
    (print "pass mergeLinearTest seq1 seq2\n";
    ())
  else
    (print "fail mergeLinearTest seq1 seq2\n";
    print (Seq.toString (fn x => Int.toString x) dest);
    print "\n";
    ())
end

fun testMergeLinear2 _ =
let
  val dest = Seq.tabulate (fn _ => 0) 10
  val seq1 = Seq.fromList [2, 8, 9, 10]
  val seq2 = Seq.fromList [0,3,4,5,6,12]
  val answer = Seq.fromList [0,2,3,4,5,6,8,9,10,12]
in
  mergeLinear seq1 seq2 dest;
  if Seq.equal (fn (x, y) => x = y) (dest, answer) then
    (print "pass mergeLinearTest2 seq1 seq2\n";
    ())
  else
    (print "fail mergeLinearTest2 seq1 seq2\n";
    print (Seq.toString (fn x => Int.toString x) dest);
    print "\n";
    ())
end

fun testBisplit _ =
let
  val numbers = Seq.fromList [3,6,1,9,23,44,2,88]
  val seq1 = Seq.fromList [10,20,30,40,50,60,70,80,90, 100]
  val seq2 = Seq.fromList [5,15,25,35,45,55,65,75,85,95]
  val seq3 = Seq.fromList [1,2,3,4,6,7,8,9,11,12,13,14,16,17,18,19,21]
  val seq4 = Seq.fromList [201,202,203,204,210,211,212,213,220,221,222,223,224]
  val seq5 = Seq.fromList [205,206,207,208,214,215,216,217,218,225,226,227,228]
  val seq6 = Seq.fromList [0,1,2,3,4,5,6,7,8,9]
  val seq7 = Seq.fromList [10,11,12,13,14,15,16,17,18,19]
in
  if binarySearch seq1 4 = 0 then
    print "pass binarySearch seq1 4\n"
  else
    print "fail binarySearch seq1 4\n";
  if binarySearch seq1 14 = 1 then
    print "pass binarySearch seq1 14\n"
  else
    print "fail binarySearch seq1 14\n";
  if binarySearch seq1 101 = 10 then
    print "pass binarySearch seq1 101\n"
  else
    print "fail binarySearch seq1 101\n";
  if bisplit seq1 seq2 20 = (10,10) then
    print "pass bisplit seq1 seq2 20\n"
  else
    print "fail bisplit seq1 seq2 20\n";
  if bisplit seq1 seq2 4 = (2,2) then
    print "pass bisplit seq1 seq2 4\n"
  else
    print "fail bisplit seq1 seq2 4\n";
  if bisplit seq1 seq2 8 = (4,4) then
    print "pass bisplit seq1 seq2 8\n"
  else
    print "fail bisplit seq1 seq2 8\n";
  if bisplit seq1 seq2 13 = (6,7) then
    print "pass bisplit seq1 seq2 13\n"
  else
    print "fail bisplit seq1 seq2 13\n";
  if bisplit seq2 seq3 5 = (1,4) then
    print "pass bisplit seq2 seq3 5\n"
  else
    print "fail bisplit seq2 seq3 5\n";
  if bisplit seq2 seq3 17 = (2,15) then
    print "pass bisplit seq2 seq3 17\n"
  else
    print "fail bisplit seq2 seq3 17\n";
  if bisplit seq4 seq5 6 = (4,2) then
    print "pass bisplit seq4 seq5 6\n"
  else
    print "fail bisplit seq4 seq5 6\n";
  if bisplit seq4 seq5 10 = (6,4) then
    print "pass bisplit seq4 seq5 10\n"
  else
    print "fail bisplit seq4 seq5 10\n";
  if bisplit seq6 seq7 10 = (10,0) then
    print "pass bisplit seq6 seq7 10\n"
  else
    print "fail bisplit seq6 seq7 10\n";
    ()
end

val _ = testBisplit ()
val _ = testMergeLinear1 ()
val _ = testMergeLinear2 ()

val _ = testMergeSort1 ()

val _ = testMergeSort2 ()
