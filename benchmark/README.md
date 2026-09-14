# HerbCore Benchmarks

This directory contains benchmarks for functions within `HerbCore`--`RuleNode`
and friends, for the most part. `benchmarks.jl` contains the code for creating
the benchmark suite (a
[`BenchmarkTools.BenchmarkGroup`](https://juliaci.github.io/BenchmarkTools.jl/stable/manual/#The-BenchmarkGroup-type)),
but does not run anything by itself.

Note that you don't have to run the benchmarks manually if you don't want to.
Each PR will run the benchmarks for you and comment the results on the PR.
The rest of these instructions are for those who would like to benchmark their
changes locally.

To run the suite, there are plenty of tools built on top of `BenchmarkTools`.
We mention two here: the [barebones, REPL-based](#Running-in-the-REPL)
approach, and the CLI-based [AirspeedVelocity](#Running-with-AirspeedVelocity)


## Running in the REPL
The most basic approach is to load the script in the REPL, and `tune!`/`run` it.


```sh
julia --project=benchmark
```

```julia
julia> include("benchmark/benchmarks.jl")

julia> using BenchmarkTools

julia> tune!(SUITE);

julia> result = run(SUITE)
2-element BenchmarkTools.BenchmarkGroup:
  tags: []
  "deepcopy" => 5-element BenchmarkTools.BenchmarkGroup:
          tags: ["rulenodes"]
          "Small uniform tree" => Trial(1.529 μs)
          "Tree with hole left side" => Trial(1.454 μs)
          "Small tree" => Trial(954.875 ns)
          "Tree with hole middle" => Trial(1.929 μs)
          "Big tree" => Trial(4.381 μs)
  "get_node_at_location" => 5-element BenchmarkTools.BenchmarkGroup:
          tags: ["rulenodes"]
          "Small uniform tree" => Trial(48.667 ns)
          "Tree with hole left side" => Trial(38.810 ns)
          "Small tree" => Trial(48.576 ns)
          "Tree with hole middle" => Trial(38.852 ns)
          "Big tree" => Trial(98.032 ns)

julia> 
```

If you swap out `include` for `includet` (from
[`Revise.jl`](https://timholy.github.io/Revise.jl/stable/user_reference/#Revise.includet)),
you can then make changes and re-run benchmarks quickly without restarting your
REPL session.

## Running with AirspeedVelocity

Make sure the benchmark environment is instantiated. This will install
[`AirspeedVelocity.jl`](https://astroautomata.com/AirspeedVelocity.jl/stable/).

```sh
julia --project=benchmark
```

```julia
pkg> instantiate
```

You can then benchmark from your terminal directly using the command `benchpkg`
(assuming your `./julia/bin` directory is on your path). See [the
documentation](https://astroautomata.com/AirspeedVelocity.jl/stable/)
for examples and options. There is also a Julia API if you'd rather use the
tool from within the REPL.
