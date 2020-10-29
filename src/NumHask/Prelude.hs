{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK prune #-}

-- | A numeric prelude.
--
-- 'NumHask.Prelude' is composed of 'NumHask' modules and "protolude" with a few monir changes.
module NumHask.Prelude
  ( module NumHask.Algebra.Abstract.Additive,
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

    Category (..),
    module Protolude,
    module Data.Biapplicative,
    module Control.Monad.Morph,
    module Data.Functor.Constant,
    pack,
    unpack,

    -- * rebindables
    -- Using different types for numbers requires RebindableSyntax.  This then removes base-level stuff that has to be put back in.
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
-- >>> 1+1
-- 2

-- | FIXME: rebindable syntax splats this, and I'm not sure where it exists in GHC land
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y
