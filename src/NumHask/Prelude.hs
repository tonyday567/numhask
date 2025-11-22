{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK prune #-}

-- | A prelude composed by overlaying numhask on Prelude, together with a few minor tweaks needed for RebindableSyntax.
module NumHask.Prelude
  ( -- * numhask exports
    module NumHask.Algebra.Additive,
    module NumHask.Algebra.Field,
    module NumHask.Algebra.Lattice,
    module NumHask.Algebra.Action,
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
    #if defined(__GLASGOW_HASKELL__)
    Natural (..),
    #endif
    module GHC.OverloadedLabels,

    -- * Modules you can't live without
    module Data.Bool,
    module Data.Kind,
    module GHC.Generics,
    module Control.Applicative,
    module Data.Traversable,
    module Data.Semigroup,
    module Data.Maybe,

    -- * Data.Function
    module Data.Function,

    -- * Control.Category

    -- Putting id back.
    module Control.Category,

    -- * Data.Foldable
    module Data.Foldable,

    -- * The Prelude
    module Prelude,
  )
where

import Control.Applicative
import Control.Category
import Data.Bool
import Data.Foldable hiding (product, sum)
import Data.Function hiding (id, (.))
import Data.Kind
import Data.Maybe
import Data.Semigroup hiding (Product (..), Sum (..))
import Data.Traversable
import GHC.Exts
import GHC.Generics
#if defined(__GLASGOW_HASKELL__)
import GHC.Natural (Natural (..))
#endif
import GHC.OverloadedLabels
import NumHask.Algebra.Action
import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Lattice
import NumHask.Algebra.Metric
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import NumHask.Data.Complex
import NumHask.Data.Integral
import NumHask.Data.Rational
import NumHask.Exception
import Prelude hiding (Integral (..), Rational, abs, acos, acosh, asin, asinh, atan, atan2, atanh, ceiling, cos, cosh, even, exp, floor, fromInteger, fromIntegral, fromRational, gcd, id, log, logBase, negate, odd, pi, product, properFraction, recip, round, signum, sin, sinh, sqrt, subtract, sum, tan, tanh, toInteger, toRational, truncate, (*), (**), (+), (-), (.), (/), (^), (^^))

-- $usage
--
-- >>> :m -Prelude
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
