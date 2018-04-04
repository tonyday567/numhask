numhask-bench
=============

[![Build
Status](https://travis-ci.org/tonyday567/numhask-bench.png)](https://travis-ci.org/tonyday567/numhask-bench)

early results
-------------

1 cycle = 0.4 nanoseconds. Run tables show the time performance in
cycles for the first 5 runs, and then the 40th percentile of the total
runs. See [perf](https://hackage.haskell.org/package/perf) for what's
going on under the hood.

Comparing matrix multiplication for NumHask.Array, hmatrix and matrix

    square matrix size: 10

    creation array:   1.684e4 cycles
    creation hmatrix: 1.546e4 cycles
    creation matrix:  2.754e3 cycles

    run                       first     2nd     3rd     4th     5th  40th %

    mmult (sz,sz)           1.632e4 3.440e2 1.720e2 1.220e2 1.260e21.190e2 cycles
    hmatrix mmult (sz,sz)   6.062e3 2.366e3 2.290e3 2.244e3 2.264e32.201e3 cycles
    matrix mmult (sz,sz)    2.132e4 1.664e4 1.635e4 1.614e4 1.610e41.594e4 cycles

NumHask.Array operations

    square matrix size: 10
    run                       first     2nd     3rd     4th     5th  40th %

    row                     6.068e3 8.040e2 5.400e2 3.660e2 3.660e23.584e2 cycles
    col                     1.274e3 2.820e2 1.440e2 1.380e2 1.420e21.216e2 cycles
    unsafeRow               9.120e2 1.780e2 5.400e1 5.000e1 4.600e14.392e1 cycles
    unsafeCol               2.050e3 1.880e2 1.560e2 1.300e2 1.540e21.100e2 cycles
    unsafeIndex             2.778e3 4.040e2 2.800e2 2.480e2 1.940e21.899e2 cycles
    concatenate             1.641e4 8.352e3 9.828e5 1.646e4 1.202e47.271e3 cycles
    transpose               3.620e2 4.800e1 2.400e1 2.600e1 2.400e12.170e1 cycles

I use the reults below to line up with the perf baseline stats, to check
if everything is humming at roughly a good speed. Summing 1 to 1000
should take about 700 cycles.

    sum to 1000
    first measure: 740 cycles
    fInt version: 16378 cycles
    sum                     9.942e4 2.112e3 2.042e3 1.986e3 2.234e31.400e3 cycles

recipe
------

```
stack build --exec "$(stack path --local-install-root)/bin/numhask-bench"  --exec "$(stack path --local-bin)/pandoc -f markdown -i bench/bench.md -t markdown -o readme.md --filter pandoc-include --mathjax"
```
    

