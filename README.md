# gc-bench
Benchmarks for evaluating MPL performance.

## Setup

You need `mpl` and `mlton` in your path.

## Run

Run all experiments:
```
$ ./run
```
This produces a file `results/XXX` where `XXX` is the current date/time.

Print a summary of the results:
```
$ ./report results/XXX
```

## Huh?

Here's how the repo is organized:

  * `bench/` has benchmark codes. Each benchmark has its own subdirectory and
  `.mlb` for compilation.
  * `lib/` is a shared library.
  * `config/` defines how to use different compilers to make a benchmark.
  * `inputs/` contains files which are used as input by some benchmarks.
  * `scripts/` has some utilities for building and running benchmarks.
  * `exp.json` defines the experiments to be performed. It's fairly
  self-explanatory.

Individual benchmark binaries are named `BENCH.CONFIG.bin`, where `BENCH` is a
benchmark name (one of the subdirectories of `bench/`) and `CONFIG` is a
compiler configuration (one of the files in `config/`).

The top-level makefile takes a binary name, compiles it and puts the resulting
binary in `bin/`. You can run a single benchmark this way, for example:
```
$ make fib.mpl.bin
$ bin/fib.mpl.bin @mpl procs 4 -- -N 42
```
