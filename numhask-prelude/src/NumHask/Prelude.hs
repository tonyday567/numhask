{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

-- | A prelude for NumHask
module NumHask.Prelude
  ( -- * Backend
    -- $backend
    module Protolude
#if !MIN_VERSION_base(4,11,0)
  , (<>)
  , Semigroup
#endif
    -- RebindableSyntax takes fromString and fail away so we need to put it back in
  , fromString
  , fail
  , Complex(..)
  , LogField(..)
  , Natural(..)
    -- * Algebraic Heirarchy
    -- $instances
  , module NumHask.Algebra.Additive
  , module NumHask.Algebra.Basis
  , module NumHask.Algebra.Distribution
  , module NumHask.Algebra.Field
  , module NumHask.Algebra.Integral
  , module NumHask.Algebra.Magma
  , module NumHask.Algebra.Metric
  , module NumHask.Algebra.Module
  , module NumHask.Algebra.Multiplicative
  , module NumHask.Algebra.Rational
  , module NumHask.Algebra.Ring
  , module NumHask.Algebra.Singleton

  ) where

#if MIN_VERSION_base(4,11,0)
import Protolude
       hiding (Integral(..), Rep, Semiring(..), (*), (**),
               (+), (-), (/), (^), (^^), abs, acos, acosh, asin, asinh, atan,
               atan2, atanh, ceiling, cos, cosh, exp, floor, fromInteger,
               fromIntegral, even, odd, infinity, log, logBase, negate, pi, product,
               properFraction, recip, round, sin, sinh, sqrt, sum, tan, tanh, toInteger, trans,
               zero, fromRational, Ratio(..), Rational, reduce, gcd, subtract)
#else
import Protolude
       hiding (Integral(..), Rep, Semiring(..), (*), (**),
               (+), (-), (/), (^), (^^), abs, acos, acosh, asin, asinh, atan,
               atan2, atanh, ceiling, cos, cosh, exp, floor, fromInteger,
               fromIntegral, even, odd, infinity, log, logBase, negate, pi, product,
               properFraction, recip, round, sin, sinh, sqrt, sum, tan, tanh, toInteger, trans,
               zero, fromRational, Ratio(..), Rational, reduce, gcd, subtract, (<>), Semigroup)
import Data.Semigroup ((<>), Semigroup)
#endif

import Control.Monad (fail)
import Data.String
import GHC.Natural(Natural(..))

import NumHask.Algebra.Additive
import NumHask.Algebra.Basis
import NumHask.Algebra.Distribution
import NumHask.Algebra.Field
import NumHask.Algebra.Integral
import NumHask.Algebra.Magma
import NumHask.Algebra.Metric
import NumHask.Algebra.Module
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Rational
import NumHask.Algebra.Ring
import NumHask.Algebra.Singleton
import NumHask.Data.LogField

-- $backend
-- NumHask imports Protolude as the prelude and replaces much of the 'Num' heirarchy in base.
-- Usage of 'Semigroup' and 'Monoid' has been avoided to retain basic compatability.
-- $instances
-- Re-defines the numeric tower.
--
-- Instances for 'Int', 'Integer', 'Float', 'Double', 'Bool', 'Complex' and 'Natural'are supplied.
--
