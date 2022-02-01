0.9.0
===
* Removed bounded classes.
* Moved operators outside of class definitions where possible. 

0.8.0
=====

* GHC 9.0.1 support
* Removed protolude and replaced it with prelude
* Removed NumHask.Data.Positive, NumHask.Data.LogFloat, NumHask.Data.Wrapper
* modified project build to cabal
* removed NegativeLiterals recommendation.

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
