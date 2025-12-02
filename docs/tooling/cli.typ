#import "/lib.typ": *

= Using the CLI

== Building <building>

You can build a Vine program with `vine build`,
  which outputs an @ivy[Ivy] file.

`vine build` takes a path to the main Vine program to run.
You can supply additional library paths with `--lib <path>`.

You can enable @debug[debug mode] with `--debug`.

== Running

You can build and run a Vine program with `vine run`.
All of the @building[build options] apply.

Passing `--workers <n>` or `-w <n>` will
  execute the program in parallel on `n` threads.

Passing `--breadth-first` will run the program in breadth-first mode,
  where at each step, every possible interaction is performed in parallel.
This allowing measuring the
  the number of parallel steps needed,
  which corresponds to the
  #link("https://en.wikipedia.org/wiki/Analysis_of_parallel_algorithms#Definitions")[_depth_]
  of the computation.
This gives an upper bound on the speedup that could be seen from a parallel machine.
The flags `--depth` and `-d` can thus be used as aliases.

By default, the runtime is limited to 4 GB of memory.
You can configure the size of the runtime's heap with `--heap`.
If your program runs out of memory,
  consider specifying a larger heap size such as `--heap 16G`.

== REPL

Vine's CLI also features a @repl[REPL].
