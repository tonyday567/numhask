numhask-bench
=============

[![Build
Status](https://travis-ci.org/tonyday567/numhask-bench.png)](https://travis-ci.org/tonyday567/numhask-bench)

array performance
-----------------

The first runs compares creation of a 10x10 matrix and matrix
multiplication for:

-   [hmatrix](http://hackage.haskell.org/package/hmatrix)
-   [matrix](https://hackage.haskell.org/package/matrix)
-   A NumHask.Array instance with list as a container
-   A NumHask.Array instance with a boxed vector instance

<!-- -->

    square matrix size: 10

    creation
    hmatrix:                  4.70e4
    matrix:                   2.77e3
    Array []:                 2.37e4
    Array Vector(Boxed):      5.41e4

    mmult
    run                        first      2nd      3rd   median      av.

    hmatrix                   2.03e4   6.94e3   2.49e3   2.20e3   3.36e3
    matrix                    4.09e4   1.96e4   1.92e4   1.59e4   2.08e4
    []                        1.77e4   3.24e2   1.92e2   1.33e2   7.74e2
    Boxed                     1.89e4   7.33e3   6.68e3   6.42e3   9.40e3

All measurements are in cycles. See
[perf](https://hackage.haskell.org/package/perf) for what this is. The
runs show the first three measurements, then the median and average of
many runs.

NumHask.Array operations
------------------------

    square matrix size: 10
    run                        first      2nd      3rd   median      av.

    row                       4.95e3   6.68e2   4.50e2   3.27e2   6.56e2
    col                       1.01e3   5.18e2   1.52e2   1.20e2   1.30e2
    unsafeRow                 1.21e3   1.34e2   3.80e1   3.45e1   3.68e1
    unsafeCol                 5.22e2   1.36e2   1.32e2   1.09e2   8.11e2
    unsafeIndex               4.65e3   4.18e2   2.86e2   1.84e2   1.99e2
    concat                    2.31e4   8.87e3   7.77e3   8.16e3   1.38e4
    transpose                 4.06e2   6.80e1   2.20e1   2.19e1   2.30e1

recipe
------

    stack build --exec "$(stack path --local-install-root)/bin/numhask-bench" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/bench_.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch

reference
---------

-   [perf](https://hackage.haskell.org/package/perf)
-   [numhask](https://hackage.haskell.org/package/numhask)

