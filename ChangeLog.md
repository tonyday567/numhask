0.12
===

- added SemiField, and bumped QuotientField to default for Subtraction.

- moved infinity & nqn to SemiField, from Field.

- introduced NumHask.Data.Positive

- introduced NumHask.Data.Wrapped

- Monus & Addus

- hiding Prelude.Rational

0.11.1.0
===
* Added Sum (..)
* Added Product (..)

0.11.0.0
===

* TypeFamilies introduced replacing FunDep usage for QuotientField, AdditiveAction, MultiplicativeAction, Basis. Classes go from Multi-parameter to single.
* EuclideanPair introduced as an intended DerivingVia support for 2 dimensional Basis & Direction instances. 
* Complex modified to use EuclideanPair. Underlying representation changed to tuple and (+:) constructor as a top-level function.
* Action class operators changed from (.\*) to (|\*), and (\*.) to (\*|) etc.
* Ring, Field, Distributive & Module become type synonyms (were classes).
* Added Basis class replacing Norm & Signed
* extra type synonyms added for Basis specialisations: Absolute, Sign, EndoBased.
* abs becomes top-level function (previously method of Norm).
* sign removed and replaced with signum, mirroring Num.
* aboutEqual & nearZero moved outside Epsilon class definition.
* rationalised Language pragmas around GHC2021
* introduced QuotientField instance for Complex & EuclideanPair without Ord constraint.

0.10.0
===
* Moved operators back in.
* added doctests and properties
* added accsum & accproduct
* fixed Ratio Eq instance

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
