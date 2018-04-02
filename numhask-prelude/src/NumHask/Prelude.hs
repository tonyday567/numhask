{-# OPTIONS_GHC -Wall #-}

-- | A prelude for NumHask
module NumHask.Prelude
  ( -- * Backend
    -- $backend
    module Protolude

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
  , module NumHask.Algebra.Ring
  , module NumHask.Algebra.Singleton

  ) where

import Protolude
       hiding (Bounded(..), Integral(..), Rep, Semiring(..), (*), (**),
               (+), (-), (/), (^), (^^), abs, acos, acosh, asin, asinh, atan,
               atan2, atanh, ceiling, cos, cosh, exp, floor, fromInteger,
               fromIntegral, infinity, isNaN, log, logBase, negate, pi, product,
               recip, round, sin, sinh, sqrt, sum, tan, tanh, toInteger, trans,
               zero)

import NumHask.Algebra.Additive
import NumHask.Algebra.Basis
import NumHask.Algebra.Distribution
import NumHask.Algebra.Field
import NumHask.Algebra.Integral
import NumHask.Algebra.Magma
import NumHask.Algebra.Metric
import NumHask.Algebra.Module
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import NumHask.Algebra.Singleton

-- $backend
-- NumHask imports Protolude as the prelude and replaces much of the 'Num' heirarchy in base.
-- Usage of 'Semigroup' and 'Monoid' has been avoided to retain basic compatability.
-- $instances
-- Re-defines the numeric tower.
--
-- Instances for 'Int', 'Integer', 'Float', 'Double', 'Bool' and 'Complex' are supplied.
--
