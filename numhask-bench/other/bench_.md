numhask-bench
===

[![Build Status](https://travis-ci.org/tonyday567/numhask-bench.png)](https://travis-ci.org/tonyday567/numhask-bench)

array performance
---

The first runs compares creation of a 10x10 matrix and matrix multiplication for:

- [hmatrix](http://hackage.haskell.org/package/hmatrix)
- [matrix](https://hackage.haskell.org/package/matrix)
- A NumHask.Array instance with list as a container
- A NumHask.Array instance with a boxed vector instance

```include
other/array.md
```

All measurements are in cycles. See [perf](https://hackage.haskell.org/package/perf) for what this is.  The runs show the first three measurements, then the median and average of many runs.

NumHask.Array operations
---

```include
other/ops.md
```

recipe
---

```
stack build --exec "$(stack path --local-install-root)/bin/numhask-bench" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/bench_.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch
```

reference
---

- [perf](https://hackage.haskell.org/package/perf)
- [numhask](https://hackage.haskell.org/package/numhask)
