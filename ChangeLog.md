0.7.0
=====

* GHC 8.10.2 support
* Modules `NumHask.Algebra.Abstract.*` renamed to `NumHask.Algebra.*`
* Renamed `Normed` to `Norm` and added `basis`
* Removed `Metric` and added `distance`
* Added `Direction`, `Polar`, `polar`, `coord`; streamlined `Complex`
* Removed `NumHask.Data.Pair`
* Fixed `FromIntegral` and `FromRational` to work in well with rebindable syntax.
* Added fundeps to `Norm`, `Direction`
* Integrated `NumHask.Algebra.Action` into `NumHask.Algebra.Module`
* Added `atan2`
* Added doctests and laws
* Improved haddocks
* Made (^) a monomorphic `a -> Int -> a` and accept negative Ints



0.6.0
=====

* GHC 8.10.1 support
