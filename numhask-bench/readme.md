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
    hmatrix:                  4.06e4
    matrix:                   9.82e3
    numhask []:               2.81e4
    numhask Boxed:            4.15e4
    DLA:                      5.05e4

    mmult
    run                        first      2nd      3rd   median      av.

    hmatrix                   1.71e4   2.30e3   1.89e3   2.01e3   3.12e3
    matrix                    4.92e4   3.39e4   3.31e4   2.33e4   3.05e4
    numhask []                8.78e3   4.36e3   7.20e1   6.88e1   5.30e2
    numhask Boxed             3.24e4   6.46e3   5.35e3   5.26e3   1.82e4
    DLA                       5.76e4   5.46e4   5.37e4   5.36e4   8.06e4

    square matrix size: 20

    creation
    hmatrix:                  3.01e4
    matrix:                   5.75e3
    numhask []:               1.60e3
    numhask Boxed:            2.66e4
    DLA:                      2.33e4

    mmult
    run                        first      2nd      3rd   median      av.

    hmatrix                   1.40e4   1.17e4   1.16e4   1.16e4   1.16e4
    matrix                    1.16e5   1.08e5   6.22e5   1.07e5   1.24e5
    numhask []                1.79e3   1.24e2   9.60e1   6.88e1   1.07e2
    numhask Boxed             2.64e4   1.95e4   1.85e4   1.88e4   3.52e4
    DLA                       5.76e5   4.08e5   5.78e5   5.65e5   5.57e5

    square matrix size: 100

    creation
    hmatrix:                  1.85e6
    matrix:                   2.19e5
    numhask []:               1.93e3
    numhask Boxed:            2.35e6
    DLA:                      7.65e6

    mmult
    run                        first      2nd      3rd   median      av.

    hmatrix                   1.22e6   1.22e6   1.38e6   1.32e6   1.41e6
    matrix                    1.26e7   1.20e7   1.61e7   1.21e7   1.22e7
    numhask []                2.48e3   1.56e2   9.60e1   6.28e1   1.11e2
    numhask Boxed             1.41e6   1.51e6   3.66e6   1.51e6   2.01e6
    DLA                       6.64e7   7.01e7   7.21e7   6.44e7   6.55e7

All measurements are in cycles. See
[perf](https://hackage.haskell.org/package/perf) for what this is. The
runs show the first three measurements, then the median and average of
many runs.

NumHask.Array operations
------------------------

    square matrix size: 100
    run                        first      2nd      3rd   median      av.

    row                       1.26e4   3.31e3   2.97e3   2.88e3   2.99e3
    col                       1.04e3   1.32e2   6.80e1   4.00e1   6.01e1
    unsafeRow                 6.90e2   8.20e1   4.20e1   4.12e1   4.93e1
    unsafeCol                 5.38e2   9.20e1   6.40e1   4.15e1   5.10e1
    unsafeIndex               5.48e3   1.35e3   1.21e3   1.19e3   1.25e3
    concat                    7.95e6   1.12e7   1.24e7   9.97e6   1.00e7
    transpose                 3.02e2   5.00e1   2.20e1   2.15e1   2.64e1

recipe
------

    stack build --exec "$(stack path --local-install-root)/bin/numhask-bench" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/bench_.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch

reference
---------

-   [perf](https://hackage.haskell.org/package/perf)
-   [numhask](https://hackage.haskell.org/package/numhask)
