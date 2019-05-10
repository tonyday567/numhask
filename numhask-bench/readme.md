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
    hmatrix:                  5.16e4
    matrix:                   2.41e3
    Array []:                 2.36e4
    Array Vector(Boxed):      5.19e4

    mmult
    run                        first      2nd      3rd   median      av.

    hmatrix                   2.06e4   2.69e3   2.21e3   2.18e3   3.57e3
    matrix                    2.86e4   1.67e4   1.60e4   1.58e4   2.00e4
    []                        1.09e4   1.90e2   1.10e2   8.01e1   5.28e2
    Boxed                     1.45e4   5.88e3   5.18e3   4.99e3   9.83e3

All measurements are in cycles. See
[perf](https://hackage.haskell.org/package/perf) for what this is. The
runs show the first three measurements, then the median and average of
many runs.

NumHask.Array operations
------------------------

    square matrix size: 10
    run                        first      2nd      3rd   median      av.

    row                       4.65e3   6.64e2   4.04e2   3.45e2   3.64e2
    col                       1.77e3   1.74e2   7.00e1   4.48e1   5.01e1
    unsafeRow                 8.96e2   1.04e2   7.60e1   5.04e1   5.17e1
    unsafeCol                 4.60e2   9.40e1   7.80e1   4.83e1   8.69e2
    unsafeIndex               4.75e3   8.14e2   4.08e2   2.04e2   2.27e2
    concat                    1.72e4   9.68e3   9.27e3   8.01e3   1.49e4
    transpose                 3.44e2   2.40e1   2.60e1   2.19e1   2.26e1

recipe
------

    stack build --exec "$(stack path --local-install-root)/bin/numhask-bench" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/bench_.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch

reference
---------

-   [perf](https://hackage.haskell.org/package/perf)
-   [numhask](https://hackage.haskell.org/package/numhask)
