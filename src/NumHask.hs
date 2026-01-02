{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_HADDOCK prune #-}

-- | Numeric classes.
module NumHask
  ( -- * Usage
    -- $setup

    -- * Overview
    -- $overview
    -- $pictures

    -- * Prelude Mappings
    -- $mapping

    -- * Extensions
    -- $extensions

    -- * Additive
    Additive (..),
    sum,
    accsum,
    Subtractive (..),

    -- * Multiplicative
    Multiplicative (..),
    product,
    accproduct,
    Divisive (..),

    -- * Ring
    Distributive,
    Ring,
    StarSemiring (..),
    KleeneAlgebra,
    InvolutiveRing (..),
    two,

    -- * Field
    Field,
    ExpField (..),
    QuotientField (..),
    TrigField (..),
    infinity,
    negInfinity,
    nan,
    half,
    modF,
    divF,
    divModF,

    -- * Lattice
    JoinSemiLattice (..),
    joinLeq,
    (<\),
    MeetSemiLattice (..),
    meetLeq,
    (</),
    LowerBounded (..),
    UpperBounded (..),

    -- * Action
    AdditiveAction (..),
    (+|),
    SubtractiveAction (..),
    (-|),
    MultiplicativeAction (..),
    (*|),
    DivisiveAction (..),
    (/|),
    Module,
    TrivialAction (..),

    -- * Metric
    Basis (..),
    Absolute,
    Sign,
    EndoBased,
    abs,
    signum,
    distance,
    Direction (..),
    Polar (..),
    polar,
    coord,
    Epsilon (..),
    aboutEqual,
    nearZero,
    (~=),

    -- * Complex
    Complex (..),
    (+:),
    realPart,
    imagPart,

    -- * Integral
    Integral (..),
    ToIntegral (..),
    ToInt,
    FromIntegral (..),
    FromInteger (..),
    FromInt,
    even,
    odd,
    (^^),
    (^),

    -- * Rational
    Ratio (..),
    Rational,
    ToRatio (..),
    FromRatio (..),
    FromRational (..),
    reduce,
    gcd,
    numerator,
    denominator,

    -- * Exceptions
    NumHaskException (..),
    throw,
  )
where

import NumHask.Algebra.Action
  ( AdditiveAction (..),
    DivisiveAction (..),
    Module,
    MultiplicativeAction (..),
    SubtractiveAction (..),
    (*|),
    (+|),
    (-|),
    (/|),
    TrivialAction (..),
  )
import NumHask.Algebra.Additive
  ( Additive (..),
    Subtractive (..),
    accsum,
    sum,
  )
import NumHask.Algebra.Field
  ( ExpField (..),
    Field,
    QuotientField (..),
    TrigField (..),
    divF,
    divModF,
    half,
    infinity,
    modF,
    nan,
    negInfinity,
  )
import NumHask.Algebra.Lattice
  ( JoinSemiLattice (..),
    LowerBounded (..),
    MeetSemiLattice (..),
    UpperBounded (..),
    joinLeq,
    meetLeq,
    (</),
    (<\),
  )
import NumHask.Algebra.Metric
  ( Absolute,
    Basis (..),
    Direction (..),
    EndoBased,
    Epsilon (..),
    Polar (..),
    Sign,
    aboutEqual,
    abs,
    coord,
    distance,
    nearZero,
    polar,
    signum,
    (~=),
  )
import NumHask.Algebra.Multiplicative
  ( Divisive (..),
    Multiplicative (..),
    accproduct,
    product,
  )
import NumHask.Algebra.Ring
  ( Distributive,
    InvolutiveRing (..),
    KleeneAlgebra,
    Ring,
    StarSemiring (..),
    two,
  )
import NumHask.Data.Complex (Complex (..), imagPart, realPart, (+:))
import NumHask.Data.Integral
  ( FromInt,
    FromInteger (..),
    FromIntegral (..),
    Integral (..),
    ToInt,
    ToIntegral (..),
    even,
    odd,
    (^),
    (^^),
  )
import NumHask.Data.Rational
  ( FromRatio (..),
    FromRational (..),
    Ratio (..),
    Rational,
    ToRatio (..),
    gcd,
    reduce,
    numerator,
    denominator,
  )
import NumHask.Exception (NumHaskException (..), throw)

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> 1+1
-- 2

-- $extensions
--
-- [RebindableSyntax](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rebindable_syntax.html)
-- is recommended for use with numhask.
--
-- As a replacement for the numerical classes, numhask clashes significantly with an
-- unqualified import of the @Prelude@. Either numhask modules should be qualified,
-- or prelude turned off with the NoImplicitPrelude extension, or with RebindableSyntax,
-- which implies NoImplicitPrelude.
--
-- == defaulting
--
-- Without RebindableSyntax, numeric literals default as follows:
--
-- >>> :set -XNoRebindableSyntax
-- >>> :t 1
-- 1 :: Num a => a
--
-- >>> :t 1.0
-- 1.0 :: Fractional a => a
--
-- With RebindableSyntax (which also switches NoImplicitPrelude on) literal numbers change to the numhask types, 'FromInteger' and 'FromRational':
--
-- >>> :set -XRebindableSyntax
-- >>> :t 1
-- 1 :: FromInteger a => a
--
-- >>> :t 1.0
-- 1.0 :: FromRational a => a
--
-- >>> 1
-- 1
--
-- >>> 1.0
-- 1.0
--
-- RebindableSyntax is a tradeoff, however, and usage comes attached with other non-numeric changes
-- that "NumHask.Prelude" attempts to counteract.
--
-- See [haskell2010 Section 4.3.4](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-750004.3) for the nuts and bolts to defaulting.
--
-- The effect of [ExtendedDefaultRules](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/ghci.html#extension-ExtendedDefaultRules)
-- in ghci or switched on as an extension also need to be understood.
-- It can lead to unusual interactions with numerics and strange error messages at times because
-- it adds @()@ and @[]@ to the start of the type defaulting list.

-- $overview
-- numhask is largely a set of classes that can replace the 'GHC.Num.Num' class and it's descendents.
-- Principles that have guided design include:
--
-- - __/balanced class density/__. The numeric hierarchy begins with addition and multiplication,
--   choosing not to build from a Magma base. Whilst not being as principled as other approaches, this circumvents the instance explosion problems of Haskell whilst maintaining clarity of class purpose.
--
-- - __/operator-first/__. In all cases, a class exists to define useful operators.
--   Major class groupings, such as 'Distributive', 'Ring' and 'Field' are type synonyms.
--
-- - __/lawful/__. All classes have laws associated with them that serve to relate class operators together in a meaningful way.
--
-- - __/low-impact/__. The library attempts to fit in with the rest of the Haskell ecosystem.
--   It provides instances for common numbers: 'GHC.Num.Int', 'GHC.Num.Integer', 'GHC.Float.Double',
--   'GHC.Float.Float', 'GHC.Natural.Natural', and the Word classes. It avoids name (or idea) clashes with other popular libraries
--   and adopts conventions in the <https://hackage.haskell.org/package/base/docs/Prelude.html current prelude>
--   where they make sense.
--
-- - __/proof-of-concept/__. The library may be below industrial-strength depending on a definition
--   of this term. At the same time, correspondence around improving the library is most welcome.

-- $pictures
--
-- The class heirarchy looks somewhat like this:
--
-- ![classes](other/nh.svg)

-- $mapping
--
-- 'GHC.Num' is a very old part of haskell, and is virtually unchanged since its specification in
-- [haskell98](https://www.haskell.org/onlinereport/standard-prelude.html).
--
-- A deconstruction of 'GHC.Num.Num' and mapping to numhask.
--
-- > -- | Basic numeric class.
-- > class  Num a  where
-- >    {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
-- >
-- >    (+), (-), (*)       :: a -> a -> a
-- >    -- | Unary negation.
-- >    negate              :: a -> a
--
-- '(+)' is an operator of the 'Additive' class
--
-- '(-)' & 'negate' are functions in the 'Subtractive' class, and
--
-- '(*)' is an operator of the 'Multiplicative' class.
--
-- 'zero' and 'one' are also introduced to the numeric hierarchy.
--
-- >    -- | Absolute value.
-- >    abs                 :: a -> a
-- >    -- | Sign of a number.
-- >    -- The functions 'abs' and 'signum' should satisfy the law:
-- >    --
-- >    -- > abs x * signum x == x
-- >    --
-- >    -- For real numbers, the 'signum' is either @-1@ (negative), @0@ (zero)
-- >    -- or @1@ (positive).
-- >    signum              :: a -> a
--
-- The concept of an absolute value and the sign of a number can include situations where the domain type is different to the absolute and sign codomain types.
--
-- A new class, 'Basis' is supplied to handle these situations:
--
-- - the 'magnitude' method is a generalisation of 'abs'
--
-- - the 'basis' method is a generalisation of 'signum'
--
-- 'NumHask.Algebra.Metric.abs' and 'NumHask.Algebra.Metric.signum' are specialisations of these methods.
--
-- >    -- | Conversion from an 'Integer'.
-- >    -- An integer literal represents the application of the function
-- >    -- 'fromInteger' to the appropriate value of type 'Integer',
-- >    -- so such literals have type @('Num' a) => a@.
-- >    fromInteger         :: Integer -> a
--
-- 'FromInteger' becomes its own class and 'FromIntegral' is introduced to polymorphise the covariant.
--
-- Mappings from other areas of prelude include:
--
-- - 'GHC.Real.Integral' becomes 'Integral' and a polymorphic 'ToIntegral' is introduced.
--
-- - 'GHC.Real.Fractional' is roughly synonymous to 'Field' together with a polymorphic 'FromRatio'.
--
-- - 'GHC.Real.RealFrac' becomes 'QuotientField' with a polymorphic 'Whole' type using Type Families.
--
-- - 'GHC.Float.Floating' is split into 'ExpField' and 'TrigField'
--
-- - 'GHC.Float.RealFloat' is not attempted. Life is too short.
--
-- - Complex is resupplied in 'NumHask.Data.Complex' but with some functionality deriving via 'NumHask.Algebra.Metric.EuclideanPair'. The underlying representation has also been switched to a newtype-wrapped tuple.
--
-- In addition to base changes, alternatives to 'sum' and 'product' from 'Data.Foldable' are also supplied.
