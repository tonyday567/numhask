{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK prune #-}

-- | A numeric prelude, composed by splicing numhask modules with [protolude](https://hackage.haskell.org/package/protolude), together with a few minor tweaks and additions.
module NumHask.Prelude
  ( -- * numhask exports
    module NumHask.Algebra.Additive,
    module NumHask.Algebra.Field,
    module NumHask.Algebra.Group,
    module NumHask.Algebra.Lattice,
    module NumHask.Algebra.Module,
    module NumHask.Algebra.Multiplicative,
    module NumHask.Algebra.Ring,
    module NumHask.Analysis.Metric,
    module NumHask.Data.Complex,
    module NumHask.Data.Integral,
    module NumHask.Data.LogField,
    module NumHask.Data.Rational,
    module NumHask.Data.Positive,
    module NumHask.Exception,

    -- * rebindables
    -- $rebindables
    fromString,
    fail,
    ifThenElse,
    fromList,
    fromListN,

    -- * extras
    -- $extras
    Category (..),
    pack,
    unpack,
    module Data.Bifunctor,
    module Data.Biapplicative,
    module Control.Monad.Morph,
    module Data.Functor.Constant,
    module System.Random,
    module System.Random.Stateful,
    Natural (..),

    -- * protolude
    -- $protolude
    module Protolude,
  )
where

import Control.Category (Category (..))
import Control.Monad (fail)
import Control.Monad.Morph
import Data.Biapplicative
import Data.Bifunctor
import Data.Functor.Constant
import Data.String
import Data.Text (pack, unpack)
import GHC.Exts
import GHC.Natural (Natural (..))
import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Group
import NumHask.Algebra.Lattice
import NumHask.Algebra.Module
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import NumHask.Analysis.Metric
import NumHask.Data.Complex
import NumHask.Data.Integral
import NumHask.Data.LogField
import NumHask.Data.Positive
import NumHask.Data.Rational
import NumHask.Exception
import Protolude hiding ((*), (**), (+), (-), (.), (/), (<<$>>), (<<*>>), Complex (..), Integral (..), Ratio, Product (..), Rep, Semiring (..), Sum (..), (^), (^^), abs, acos, acosh, asin, asinh, atan, atan2, atanh, ceiling, cis, cos, cosh, even, exp, floor, fromInteger, fromIntegral, fromRational, gcd, imagPart, infinity, log, logBase, magnitude, mkPolar, negate, odd, phase, pi, polar, product, properFraction, realPart, recip, reduce, round, sin, sinh, sqrt, subtract, sum, tan, tanh, toInteger, toRational, trans, truncate, zero, rotate)
import System.Random
import System.Random.Stateful

-- $usage
--
-- >>> :set -XRebindableSyntax
-- >>> :set -XNegativeLiterals
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

-- $extras
--
-- Bits and pieces different to protolude, including:
--
-- - re-inserting 'id' which should never be overwritten in haskell code.
--
-- - 'Data.Bifunctors' & 'Data.Biapplicative' which are favorites of the OA.
--
-- - 'Control.Monad.Morph'; another essential, ubiquitous library.
--
-- - 'Data.Functor.Constant'
--
-- - 'pack' and 'unpack', which may encourage usage of 'String' but can also quickly escape from the same.

-- $protolude
-- It would be nice to just link straight through to the [protolude documentation](https://hackage.haskell.org/package/protolude), but, alas, at time of production, haddock insists on dumping everything here.
