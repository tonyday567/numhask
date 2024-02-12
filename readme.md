numhask
===

[![Hackage](https://img.shields.io/hackage/v/numhask.svg)](https://hackage.haskell.org/package/numhask)
[![Build Status](https://github.com/tonyday567/numhask/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/numhask/actions?query=workflow%3Ahaskell-ci)

![](other/nh12.svg)


SemiField
---

Compared to previous library versions, Ring and Field have been removed as super classes of QuotientField, and SemiField introduced as the new constraint.

Old version:

![](other/nh11.svg)

```
type SemiField a = (Distributive a, Divisive a)

class (SemiField a) => QuotientField a where
  type Whole a :: Type
  properFraction :: a -> (Whole a, a)
```

The notion of a quotient is now that which distributes and divides.

Subtractive originally slipped in as a super class due to the notion of rounding down (or, specifically, towards zero). By using DefaultSignatures, a default for Subtractive-type numbers can be provided and still allow non-Subtractive (SemiField) things to be quotient fields.

Infinity and nan move from a Field to a SemiField constraint - subtraction is not needed to come up with an infinity or silly compute.

Positive
---

A motivation for SemiField was to introduce NumHask.Data.Positive into the library. Positive has no sane Subtractive instance (but should be able to be rounded).

Out of the many approaches that can be taken in defining a positive number, the definition relies on a notion of truncated subtraction; that subtraction can be performed on positive numbers but, for answers outside the typed range, the lower bound should be returned.

Specifically, the positive constructor needs to be supplied with a number that has a MeetSemiLattice instance, so that complex numbers and other geometries are correctly handled:

``` haskell
ghci> 2 +: (-2)
Complex {complexPair = (2,-2)}
ghci> positive (2 +: (-2))
UnsafePositive {unPositive = Complex {complexPair = (2,0)}}
```

Truncated Arithmetic
---

Truncated subtraction can be generalised to a notion of truncated arithmetic on a number with a typed range. This may be a direction explored further in the library including:

- [epsilon, +infinity): A positive number type which is a safe divisor.
- /= zero, non-zero arithmetic (x - x returns epsilon, say)
- [0,1]: probability and weight arithmetic
- [-1,1]: correlation math

magnitudes are positive
---

The current Basis instance of Double:

``` haskell
instance Basis Double where
  type Mag Double = Double
  type Base Double = Double
  magnitude = P.abs
  basis = P.signum
```

is probably more correctly written as:

``` haskell
instance Basis Double where
  type Mag Double = Positive Double
  type Base Double = Sign Double
  magnitude = Positive . P.abs
  basis = Sign . P.signum
```

where Sign is a future-imagined type representing {-1,0,1} or {-1,1}

In Haskell, there is a basic choice between using multiple parameters for a type or embedding types using type families. Using multiple parameters would, in practice, force users to have to chose and write 'Basis Double Double Double' or 'Basis Positive Sign Double'.

On balance, a computational chain involving magnitude is likely to be a single, underlying type, so that providing a Basis instance returning a Positive would result in a lot of unwrapping.

``` haskell
-- endo-based
x == basis x * magnitude x

-- if hetero-typed ...
x == (unSign $ basis x) * (unPositive $ magnitude x)
```

The library awaits real-world feedback on safety versus ergonomics.

Monus
---

Truncated subtraction is encapsulated within the Monus class and supplied operator:

``` haskell
ghci> 4 ∸ 7 :: Positive Int
UnsafePositive {unPositive = 0}
ghci> unPositive (4 ∸ 7 :: Positive Int)
0
ghci> unPositive (7 ∸ 4 :: Positive Int)
3
```





- introduced NumHask.Data.Positive

- introduced NumHask.Data.Wrapped


This package provides numeric classes alternate to the prelude as specified in haskell98.

The numeric class constellation looks somewhat like:

![nh](other/nh.svg)

Usage
===

``` haskell
{-# LANGUAGE RebindableSyntax #-}
import NumHask.Prelude
```
See the documentation in the NumHask module for a detailed overview.

