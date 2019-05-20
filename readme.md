numhask
===

[![Build Status](https://travis-ci.org/tonyday567/numhask.svg)](https://travis-ci.org/tonyday567/numhask) [![Gitter chat](https://badges.gitter.im/numhask/Lobby.png)](https://gitter.im/numhask/Lobby)

A numeric class hierarchy, providing a structure for numbers and functions that combine them.

Ring hierarchy
---

For example, the `Num` class in prelude is approximately a `Ring`. In numhask, the class laws and operators flow as follows:

[![Ring Hierarchy](numhask/other/ring.svg)](numhask/other/ring.svg)

Libraries
---

- `numhask` the core class hierarchy with just base as a dependency
- `numhask-prelude` numhask + [protolude](https://hackage.haskell.org/package/protolude)
- `numhask-array` n-dimensional arrays with type-level size

There is also some performance analytics in `numhask-bench` and `numhask` produces the above chart.

numhask
---

[![Hackage](https://img.shields.io/hackage/v/numhask.svg)](https://hackage.haskell.org/package/numhask) [![lts](https://www.stackage.org/package/numhask/badge/lts)](http://stackage.org/lts/package/numhask) [![nightly](https://www.stackage.org/package/numhask/badge/nightly)](http://stackage.org/nightly/package/numhask)

`numhask` begins with separately named magma-derived classes for addition and multiplication, and then being symetrical in the treatment of the two heirarchies.  A short magma structure is provided with the intention of supplying appropriate classes for operators that are neither addition nor multiplication, but this structure is not hooked up to the main classes.

To be as compatible as practical with the existing haskell ecosystem.  Ints, Integers, Floats, Doubles and Complex are taken from base and given numhask class instances, so they are also Num instances.  Monoid and Semigroup are not used in numhask to maintain compatability.

`numhask` replaces all the relevant numeric operators in Prelude, so you're going to get clashes.

QuickCheck tests of numeric laws are included.  This also includes tracking where laws are approximate or fail for non-exact numbers.

The usual operators (+) and (*) operators are reserved for commutative relationships, with plus and times being used for non-commutative ones.

In summary, the library doesn't do anything fancy. But if having to define `(*)` when you just want a `(+)` offends your sensibilities, it may bring some sanity.

numhask-prelude
---

[![Hackage](https://img.shields.io/hackage/v/numhask-prelude.svg)](https://hackage.haskell.org/package/numhask-prelude) [![lts](https://www.stackage.org/package/numhask-prelude/badge/lts)](http://stackage.org/lts/package/numhask-prelude) [![nightly](https://www.stackage.org/package/numhask-prelude/badge/nightly)](http://stackage.org/nightly/package/numhask-prelude)

``` {.sourceCode .literate .haskell}
{-# LANGUAGE NoImplicitPrelude #-}
import NumHask.Prelude
```

'Numhask.Prelude' is designed as a drop-in replacement for Prelude and 'NoImplicitPrelude' is obligatory. Behind the scenes, the module wraps [protolude](https://www.stackage.org/package/protolude).

See [Examples](numhask-prelude/src/NumHask/Examples.hs) for basic examples, or [numhask-range](https://www.stackage.org/package/numhask-range) for slightly heavier usage.


numhask-hedgehog
---

[![Hackage](https://img.shields.io/hackage/v/numhask-hedgehog.svg)](https://hackage.haskell.org/package/numhask-hedgehog) [![lts](https://www.stackage.org/package/numhask-test/badge/lts)](http://stackage.org/lts/package/numhask-hedgehog) [![nightly](https://www.stackage.org/package/numhask-hedgehog/badge/nightly)](http://stackage.org/nightly/package/numhask-hedgehog)

Testing framework for numhask, based around hedgehog.


numhask-array
---

[![Hackage](https://img.shields.io/hackage/v/numhask-array.svg)](https://hackage.haskell.org/package/numhask-array) [![lts](https://www.stackage.org/package/numhask-array/badge/lts)](http://stackage.org/lts/package/numhask-array) [![nightly](https://www.stackage.org/package/numhask-array/badge/nightly)](http://stackage.org/nightly/package/numhask-array)

An experimental array with:

- a polymorphic container
- shape specified at the type level
- Representable instances

See [Examples](src/NumHask/Array/Example.hs) for the emergent API.

To try out in ghci:

```
stack ghci
> :set -XDataKinds
> :set -XOverloadedLists
> import NumHask.Prelude
> import NumHask.Array
> let a = [0..5] :: Array [] '[2,3] Int
> a + a
[[0, 2, 4],
 [6, 8, 10]]
```
