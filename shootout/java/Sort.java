import java.util.*;
import java.util.concurrent.*;
import java.util.stream.*;

class Sort {

  private static void compute(int[] l) {
    int[] res = new int[l.length];
    IntStream.range(0, l.length).parallel().forEach(i -> res[i] = l[i]);
    java.util.Arrays.parallelSort(res);
    
    System.out.print("result [");
    for (int i = 0; i < 7; i++) {
      System.out.print("" + res[i] + ", ");
    }
    System.out.print("..., " + res[res.length-1] + "]\n");

  }

  public static void main (String args[]) throws Exception {
    int n = 100000000;
    int reps = 1;
    int sreps = 1;
    try {
      if (args.length > 0)
        n = Integer.parseInt(args[0]);
      if (args.length > 1)
        reps = Integer.parseInt(args[1]);
      if (args.length > 2)
        sreps = Integer.parseInt(args[2]);
    } catch (Exception e) {
      System.out.println("Usage: java Sort <size> <rounds> <warmup rounds>");
      return;
    }

    System.out.println("hashes " + hash(0) + " " + hash(1) + " " + hash(2));

    int[] l = new int[n];
    IntStream.range(0, n).parallel().forEach(i -> l[i] = gen(i));

    System.out.print("input [");
    for (int i = 0; i < 7; i++) {
      System.out.print("" + l[i] + ", ");
    }
    System.out.print("..., " + l[n-1] + "]\n");

    final int n2 = n;

    Runner.run(
      (Void v) -> { compute(l); return null; },
         reps, sreps);
  }

  static long hash(long i) {
    long v = i * 3935559000370003845L + 2691343689449507681L;
    v = v ^ (v >>> 21);
    v = v ^ (v << 37);
    v = v ^ (v >>> 4);
    v = v * 4768777513237032717L;
    v = v ^ (v << 20);
    v = v ^ (v >>> 41);
    v = v ^ (v <<  5);
    return v;
  }

  static int gen(int i) {
    long v = hash((long)i);
    return (int)Long.remainderUnsigned(v, 0x7fffffffL);
  }

}
