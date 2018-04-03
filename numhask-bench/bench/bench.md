numhask-bench
===

[![Build Status](https://travis-ci.org/tonyday567/numhask-bench.png)](https://travis-ci.org/tonyday567/numhask-bench)

early results
---

1 cycle = 0.4 nanoseconds. Run tables show the time performance in cycles for the first 5 runs, and then the 40th percentile of the total runs.  See [perf](https://hackage.haskell.org/package/perf) for what's going on under the hood.

Comparing matrix multiplication for NumHask.Array, hmatrix and matrix 

```include
other/array.md
```

NumHask.Array operations

```include
other/ops.md
```

I use the reults below to line up with the perf baseline stats, to check if everything is humming at roughly a good speed.  Summing 1 to 1000 should take about 700 cycles.

```include
other/test.md
```


recipe
---

```
stack build --exec "$(stack path --local-install-root)/bin/numhask-bench" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/header.md bench/bench.md other/footer.md -t html -o index.html --filter pandoc-include --mathjax" --exec "$(stack path --local-bin)/pandoc -f markdown -i bench/bench.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch
```

reference
---

- [ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
- [pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
- [libraries](https://www.stackage.org/)
- [protolude](https://www.stackage.org/package/protolude)
- [optparse-generic](https://www.stackage.org/package/optparse-generic)
- [hoogle](https://www.stackage.org/package/hoogle)
- [perf](https://hackage.haskell.org/package/perf)
- [numhask](https://hackage.haskell.org/package/numhask)
- [numhask-array](https://hackage.haskell.org/package/numhask-array)
