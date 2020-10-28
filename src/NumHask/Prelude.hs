{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK prune #-}

-- | A numeric prelude.
module NumHask.Prelude
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
    Natural (..),
    module NumHask.Exception,

    -- * Extensions
    -- $extensions

    -- * Backend
    -- $backend
    Category (..),
    module Protolude,
    module Data.Biapplicative,
    module Control.Monad.Morph,
    module Data.Functor.Constant,
    pack,
    unpack,
    -- | Using different types for numbers requires RebindableSyntax.  This then removes all sorts of base-level stuff that has to be put back in.
    fromString,
    fail,
    ifThenElse,
    fromList,
    fromListN,
  )
where

import Control.Category (Category (..))
import Control.Monad (fail)
import Control.Monad.Morph
import Data.Biapplicative
import Data.Functor.Constant
import Data.String
import Data.Text (pack, unpack)
import GHC.Exts
import GHC.Natural (Natural (..))
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
import Protolude hiding ((*), (**), (+), (-), (.), (/), (<<$>>), (<<*>>), Complex (..), Integral (..), Product (..), Ratio, Rep, Semiring (..), Sum (..), (^), (^^), abs, acos, acosh, asin, asinh, atan, atan2, atanh, ceiling, cis, cos, cosh, even, exp, floor, fromInteger, fromIntegral, fromRational, gcd, imagPart, infinity, log, logBase, magnitude, mkPolar, negate, odd, phase, pi, polar, product, properFraction, realPart, recip, reduce, round, sin, sinh, sqrt, subtract, sum, tan, tanh, toInteger, toRational, trans, truncate, zero)

-- $usage
--
-- >>> :set -XRebindableSyntax
-- >>> :set -XNegativeLiterals
-- >>> import NumHask.Prelude
-- >>> print $ 1-(1::Int)
-- 0

-- $overview
-- TODO:

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


-- | rebindable syntax splats this, and I'm not sure where it exists in GHC land
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y
