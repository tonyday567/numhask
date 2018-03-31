numhask
===

[![Build Status](https://travis-ci.org/tonyday567/numhask.svg)](https://travis-ci.org/tonyday567/numhask) [![Hackage](https://img.shields.io/hackage/v/numhask.svg)](https://hackage.haskell.org/package/numhask) [![lts](https://www.stackage.org/package/numhask/badge/lts)](http://stackage.org/lts/package/numhask) [![nightly](https://www.stackage.org/package/numhask/badge/nightly)](http://stackage.org/nightly/package/numhask) 

A numeric prelude, providing a clean structure for numbers and operations that combine them.

Field heirarchy
---

[![Field Hierarchy](https://tonyday567.github.io/other/field.svg)](https://tonyday567.github.io/other/field.svg)

Numbers with structure
---

[![Tensor Hierarchy](https://tonyday567.github.io/other/tensor_product.svg)](https://tonyday567.github.io/other/tensor_product.svg)


This particular shed has been painted:

- by providing separately named magma-derived classes for addition and multiplication, and then being symetrical in the treatment of the two heirarchies.  A short magma structure is provided with the intention of supplying appropriate classes fro operators that are no addition nor multiplication, but this structure is not hooked up to the main classes.
- to be as compatible as practical with the existing haskell ecosystem.  Ints, Integers, Floats, Doubles and Complex are taken from base and given numhask instances, so they are also Num instances.  Monoid and Semigroup are not used in numhask to maintain compatability.
- as a replacement for anything in base that has a Num, Fractional or Real constraint.
- includes QuickCheck tests of the numeric laws implicit in the classes.  This also includes tracking where laws are approximate or fail for non-exact numbers.
- the usual operators (+) and (*) operators are reserved for commutative relationships, with plus and times being used for non-commutative ones.

Alternative color-schemes, stylistic flourishes and opines are welcome.

In summary, the library doesn't do anything fancy. But if having to define `(*)` when you just want a `(+)` offends your sensibilities, it may bring some sanity.


Usage
---

``` {.sourceCode .literate .haskell}
{-# LANGUAGE NoImplicitPrelude #-}
import Numhask.Prelude
```

'Numhask.Prelude' is designed as a drop-in replacement for Prelude and 'NoImplicitPrelude' is obligatory. Behind the scenes, the module wraps [protolude](https://www.stackage.org/package/protolude).

See [Examples](src/NumHask/Examples.hs) for basic examples, [numhask-array](https://www.stackage.org/package/numhask-array) for numbers with structure, and [numhask-range](https://www.stackage.org/package/numhask-range) for slightly heavier number crunching.

