{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK prune #-}

-- | A prelude composed by overlaying numhask on Prelude, together with a few minor tweaks needed for RebindableSyntax.
module NumHask.Prelude
  ( -- * numhask exports
    module NumHask.Algebra.Additive,
    module NumHask.Algebra.Field,
    module NumHask.Algebra.Group,
    module NumHask.Algebra.Lattice,
    module NumHask.Algebra.Module,
    module NumHask.Algebra.Multiplicative,
    module NumHask.Algebra.Ring,
    module NumHask.Algebra.Metric,
    module NumHask.Data.Complex,
    module NumHask.Data.Integral,
    module NumHask.Data.Rational,
    module NumHask.Exception,

    -- * rebindables
    -- $rebindables
    fromString,
    ifThenElse,
    fromList,
    fromListN,
    Natural (..),

    -- * Modules you can't live without
    module Data.Bool,
    module Data.Kind,
    module GHC.Generics,
    module Prelude,
    module Data.Foldable,
    module Data.Traversable,
    module Data.Semigroup,
    module Data.Maybe,
  )
where

import Data.Bool
import Data.Foldable hiding (product, sum)
import Data.Kind
import Data.Maybe
import Data.Semigroup hiding (Sum (..), Product (..))
import Data.Traversable
import GHC.Exts
import GHC.Generics
import GHC.Natural (Natural (..))
import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Group
import NumHask.Algebra.Lattice
import NumHask.Algebra.Metric
import NumHask.Algebra.Module
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import NumHask.Data.Complex
import NumHask.Data.Integral
import NumHask.Data.Rational
import NumHask.Exception
import Prelude hiding (Integral (..), abs, acos, acosh, asin, asinh, atan, atan2, atanh, ceiling, cos, cosh, even, exp, floor, fromInteger, fromIntegral, fromRational, gcd, log, logBase, negate, odd, pi, product, properFraction, recip, round, sin, sinh, sqrt, subtract, sum, tan, tanh, toInteger, toRational, truncate, (*), (**), (+), (-), (/), (^), (^^))

-- $usage
--
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> 1+1
-- 2

-- $rebindables
--
-- Using different types for numbers requires RebindableSyntax.  This then removes base-level stuff that has to be put back in.

-- | RebindableSyntax splats this, and I'm not sure where it exists in GHC land
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y
