# Hashing Modulo Alpha-Equivalence

This repository contains supporting code for the "Hashing Modulo Alpha-Equivalence" paper.

## Dependencies

You need a reasonably modern installation of GHC and cabal on your `PATH`.
If you don't already have one you might like to follow the instructions for
[installing the Haskell Platform](https://www.haskell.org/downloads/#platform).

Additionally, to generate plots from the raw output data, you will either need gnuplot or Python 3.6.

## Building

In the top level of the repository run

```
cabal v2-update
cabal v2-install --ghc-options="-Wwarn" --installdir=. --overwrite-policy=always --install-method=copy
```

## Running

Most commands have two variants:
* `full`, which was unsed to produce all the plots and measurements in the paper, and
* `fast`, which is faster, but gives results that are either more noisy, less accurate, or smaller scale

### Checking that everything works

* Run the tests

    `./hash test`

* Run a very quick benchmark

    `./hash random fast`

### Running the full benchmarks

* Run the full benchmarks on random expressions

    `./hash random full +RTS -I0 -A90G -G1 -m1 -RTS`

    (The RTS options configure the garbage collector to run less)

* Run the full benchmarks on real-life expressions

    `./hash manual full`

* Run the full analysis of hash collisions

    `./hash collisions full`

## Repository structure

* `exe/` defines the entry point to the code.
* `src/` contains several variants of our hashing algorithm.
   * While only the final `AlphaHashOptimizedHash.hs` is being benchmarked, we also release `AlphaHashInefficient.hs`, `AlphaHashEfficient.hs` and `AlphaHashEfficientHash.hs`, which are stepping stones that match the flow of the paper.
   * `AlphaHashOptimizedHash16Bit.hs` is a 16-bit version of our algorithm, used only for benchmarking the empirical frequency of hash collisions.
* `exprs/` contains real-life expressions extracted from complex machine learning models such as BERT.

## Authors

* [Tom Ellis](mailto:tom.ellis@microsoft.com)
* [Krzysztof Maziarz](mailto:krzysztof.maziarz@microsoft.com)
* [Alan Lawrence](mailto:allawr@microsoft.com)
* [Andrew Fitzgibbon](mailto:awf@microsoft.com)
* [Simon Peyton Jones](mailto:simonpj@microsoft.com)

## Contributing

This project welcomes contributions and suggestions.  Most contributions require you to agree to a
Contributor License Agreement (CLA) declaring that you have the right to, and actually do, grant us
the rights to use your contribution. For details, visit https://cla.opensource.microsoft.com.

When you submit a pull request, a CLA bot will automatically determine whether you need to provide
a CLA and decorate the PR appropriately (e.g., status check, comment). Simply follow the instructions
provided by the bot. You will only need to do this once across all repos using our CLA.

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/).
For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or
contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.
