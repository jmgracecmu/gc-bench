package main

import (
  "os"
  "fmt"
  "time"
	"runtime"
)

type ParamStruct struct {
  n, threads, rounds, warmup_rounds int
  n_buckets, oversample_stride, n_countblocks int
}

// func read_cmdline_input(args []string) (int, int, int) {
//   const expected_args int = 3
//   if num_args := len(args) ; num_args != expected_args + 1 {
//     fmt.Printf("Usage: %s <n> <threads> <rounds\n", args[0])
//     os.Exit(1);
//   }
//   return string_to_int(args[1]), string_to_int(args[2]), string_to_int(args[3])
// }

func read_cmdline_input_struct(args []string) ParamStruct {
  const expected_args int = 4
  if num_args := len(args) ; num_args != expected_args + 1 {
    fmt.Printf("Usage: %s <n> <threads> <rounds> <warmup rounds>\n", args[0])
    os.Exit(1);
  }

  n := string_to_int(args[1])
  threads := string_to_int(args[2])
  rounds := string_to_int(args[3])
	warmup_rounds := string_to_int(args[4])

	runtime.GOMAXPROCS(threads)

  return ParamStruct{
    n,
    threads * 4,
    rounds,
		warmup_rounds,
    threads * 16, // # of buckets
    16,           // oversample stride (was 4)
    threads * 16, // # of countblocks (was *4)
  }
}

func main() {
// Read cmdline input
  ps := read_cmdline_input_struct(os.Args)
  n, threads, rounds, warmup_rounds := ps.n, ps.threads, ps.rounds, ps.warmup_rounds
  // n, threads, rounds := read_cmdline_input(os.Args)
  fmt.Printf("%d elements, %d threads, %d rounds, %d warmup rounds\n", n, threads, rounds, warmup_rounds)
  if rounds == 0 { os.Exit(0) } // quit now if 0 rounds requested

// Allocate slices
  input := make(ElementSlice, n)
  runtimes := make([]float64, rounds)

// Generate input sequence
  time_generate := time.Now()

  done := make(chan bool, threads)
  for i:=0;i<threads;i++ {
    go func (i int) {
      blk, base := block(input, i, threads)
      for j := range blk {
        // blk[j].Generate(j + base)
				blk[j] = hash32(int32(j + base))
      }
      done <- true
    }(i)
  }
  barrier(done, threads)

  elapsed_generate := time.Since(time_generate)
  fmt.Printf("Generating input: %s\n", elapsed_generate)
  fmt.Printf("input [");
  for i:=0; i<7; i++ {
    fmt.Printf("%d, ", input[i]);
  }
  fmt.Printf("..., %d]\n", input[n-1]);

  if warmup_rounds > 0 {
	  fmt.Println("\n------- Doing a few warm-up sorts -------")

	  for r:=0;r<warmup_rounds;r++ {
      output := make(ElementSlice, n)
	    parallel_sample_sort(input, output, ps)
	  }
	}

// Sort #rounds times
  fmt.Println("\n------- Actual sorting begins -------")

  for r:=0;r<rounds;r++ {
    fmt.Printf("Round %v: \n", r)

    // This is where our sort function should be called from!
    time_sort := time.Now()
    // sequential_sort_copy(input, output)
    output := make(ElementSlice, n)
    parallel_sample_sort(input, output, ps)
    // old_sample_sort(input, output, ps)
    elapsed_sort := time.Since(time_sort).Seconds()

    // Do some simple book-keeping
    fmt.Printf("\n==== run %d ====\nwall %d\n", r, int(elapsed_sort * 1000.0))
    runtimes[r] = elapsed_sort

    fmt.Printf("result [");
    for i:=0; i<7; i++ {
      fmt.Printf("%d, ", output[i]);
    }
    fmt.Printf("..., %d]\n", output[n-1]);

    // Verify that the output produced was correct
    // verify(output)
    fmt.Println()
  }

  total_time := 0.0
  for _,t := range(runtimes) {
    total_time += t
  }

// Print the best running-time
  best_time := runtimes[0]
  for _,t := range(runtimes) {
    if t < best_time { best_time = t }
  }

  fmt.Printf("best time:    %.3fs\n", best_time)
  fmt.Printf("average time: %.3fs\n", total_time/float64(rounds))
}
