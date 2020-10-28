{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK prune #-}

-- | Numeric classes.
module NumHask
  ( -- * Overview
    -- $overview

    -- * Mappings
    -- $mapping

    -- * NumHask
    -- $instances
    module NumHask.Algebra.Abstract.Additive,
    module NumHask.Algebra.Abstract.Field,
    module NumHask.Algebra.Abstract.Group,
    module NumHask.Algebra.Abstract.Lattice,
    module NumHask.Algebra.Abstract.Module,
    module NumHask.Algebra.Abstract.Multiplicative,
    module NumHask.Algebra.Abstract.Ring,
    module NumHask.Analysis.Metric,
    module NumHask.Data.Complex,
    module NumHask.Data.Integral,
    module NumHask.Data.LogField,
    module NumHask.Data.Rational,
    module NumHask.Data.Pair,
    module NumHask.Data.Positive,
    module NumHask.Exception,

    -- * Extensions
    -- $extensions

  )
where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Lattice
import NumHask.Algebra.Abstract.Module
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Analysis.Metric
import NumHask.Data.Complex
import NumHask.Data.Integral
import NumHask.Data.LogField
import NumHask.Data.Pair
import NumHask.Data.Positive
import NumHask.Data.Rational
import NumHask.Exception

-- $setup
--
-- >>> :set -XRebindableSyntax
-- >>> :set -XNegativeLiterals
-- >>> import NumHask.Prelude
-- >>> 1+1
-- 2

-- | $defaulting
--
-- Without RebindableSyntax, numeric literals default as follows:
--
-- > :t 1
-- 1 :: Num p => p
--
-- > :t 1.0
-- 1.0 :: Fractional p => p
--
-- With RebindableSyntax:
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




-- > :t Pair 1 -2
-- Pair 1 -2
--   :: (Subtractive a, FromInteger a, FromInteger (a -> Pair a)) =>
--     a -> Pair a
-- > :set -XNegativeLiterals
-- > :t Pair 1 -2
-- > Pair 1 -2 :: FromInteger a => Pair a
-- > Pair 1 -2
-- Pair 1 -2



-- $overview
-- <https://www.haskell.org/onlinereport/standard-prelude.html haskell98 prelude>
-- <https://hackage.haskell.org/package/base/docs/Prelude.html current prelude>
-- $mapping
--
-- 'GHC.Num' is a very old part of haskell, and a lot of different numeric concepts are tossed in there. The closest analogue in numhask is the 'Ring' class:
--
-- ![ring example](other/ring.svg)
--
-- No attempt is made, however, to reconstruct the particular constellation of classes that represent the old 'Num'.  A rough mapping of to numhask classes follows:
--
-- > -- | Basic numeric class.
-- > class  Num a  where
-- >    {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
-- >
-- >    (+), (-), (*)       :: a -> a -> a
-- >    -- | Unary negation.
-- >    negate              :: a -> a
--
-- + is a function of the 'Additive' class,
-- - is a function of the 'Subtractive' class, and
-- * is a function of the 'Multiplicative' class.
-- negate is a unary function in the 'Subtractive' class.
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
-- abs is a function in the 'NumHask.Analysis.Metric.Signed' class.  The concept of an absolute value of a number can include situations where the domain and codomain are different, and norm as a function in the 'NumHask.Analysis.Metric.Normed' class is supplied for these cases.
--
--  'NumHask.Analysis.Metric.sign' replaces 'signum', because signum is a heinous name.
--
-- >    -- | Conversion from an 'Integer'.
-- >    -- An integer literal represents the application of the function
-- >    -- 'fromInteger' to the appropriate value of type 'Integer',
-- >    -- so such literals have type @('Num' a) => a@.
-- >    fromInteger         :: Integer -> a
--

-- $backend
-- NumHask imports Protolude as a starting prelude.
--
-- In addition, 'id' is imported (protolude uses 'identity')

-- $instances
-- NumHask replaces much of the 'Num' and 'Real' heirarchies in protolude & base.
--
-- Instances for 'Int', 'Integer', 'Float', 'Double', 'Bool', 'Complex' and 'Natural'are supplied.

-- $extensions
--
-- RebindableSyntax
-- Awaiting LexicalNegation
-- NegativeLiterals
-- > :set -XRebindableSyntax
-- > \x -> x - 1
-- > \x -> x - 1 :: (Subtractive a, FromIntegral a Integer) => a -> a
--
-- > \x -> x-1
-- \x -> x-1 :: FromIntegral t1 Integer => (t1 -> t2) -> t2
--
-- > -1 `mod` 2
-- 1
--
-- Default system gets turned off by RebindableSyntax
