# Superg runtime
Superg is an evaluator for lazy functional languages based on Combinatory Logic. 
It provides:
* Translation from an enriched lambda calculus into combinators using Kiselyov's semantic translation.
* A graph reduction engine based on David Turner's [Miranda](https://www.cs.kent.ac.uk/people/staff/dat/miranda/).
* A graph reduction engine based on Phil Koopman's [TIGRE](https://users.ece.cmu.edu/~koopman/tigre/index.html).
* A simple LISP-like frontend.

I hope to expand this README at some point. 
For now please refer to the report under `doc/paper.tex`. 

## Building
To build this crate, [install a Rust toolchain](https://rustup.rs) and run:

```shell
cargo build
```

## Tests
To run the unit and integration tests, use the standard cargo command:

```shell
cargo test
```

## Benchmarking
Our benchmark suite includes a comparison with Turner's Miranda implementation.
When running the benchmarks, you must make sure to define an environment variable `MIRANDA_PATH`.
It should store the path to a `miranda` directory containing the sources for Miranda release 2066.
The Miranda source must be patched to output performance metrics.
Refer to `mirabench/README.md` for a guide on setting this up.

The easiest way to run the benchmarks is to use cargo:

```shell
MIRANDA_PATH=<path to miranda/> cargo bench
```

If you need more advanced options (e.g. JSON output), install the cargo-criterion tool:

```shell 
cargo install cargo-criterion
MIRANDA_PATH=<path to miranda/> cargo criterion
```

## Finding performance bottlenecks
We have used Cachegrind, Callgrind and Flame Graphs to study the performance of the evaluator.
Below we detail the steps needed to run the analysis yourself.

## Analyzing TIGRE performance
### Cachegrind
Build the `tigre_fib` binary, and profile it using `cachegrind`:

```shell
CARGO_PROFILE_RELEASE_DEBUG=true cargo build --release --bin tigre_fib
valgrind --tool=cachegrind target/release/tigre_fib
```

Analyze the result with `kcachegrind cachegrind.out.*`.

### Callgrind
Build the `tigre_fib` binary, and profile it using `callgrind`:

```shell
CARGO_PROFILE_RELEASE_DEBUG=true cargo build --release --bin tigre_fib
valgrind --tool=callgrind target/release/tigre_fib
```

Analyze the result with `kcachegrind callgrind.out.*`.

### Flamegraph
Install the necessary dependencies:

```shell
cargo install flamegraph
sudo apt install -y linux-perf
```

Enable profiling:

```shell
echo -1 | sudo tee /proc/sys/kernel/perf_event_paranoid
export CARGO_PROFILE_RELEASE_DEBUG=true
```

Generate the flamegraph:

```shell
cargo flamegraph --bin tigre_fib
```

Now open `flamegraph.svg` in a browser.