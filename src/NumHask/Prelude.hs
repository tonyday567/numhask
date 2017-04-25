{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wall #-}

-- | A prelude for NumHask

module NumHask.Prelude (
    -- * Backend
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
  , module NumHask.Algebra.Ordering
  , module NumHask.Algebra.Ring
  ) where

import Protolude hiding
    ( (+)
    , (-)
    , (*)
    , (/)
    , zero
    , negate
    , recip
    , Integral(..)
    , round
    , ceiling
    , floor
    , (^^)
    , Semiring(..)
    , log
    , logBase
    , exp
    , sqrt
    , (**)
    , abs
    , (^)
    , infinity
    , Bounded(..)
    , isNaN
    , fromIntegral
    , toInteger
    , fromInteger
    , Rep
    )

import NumHask.Algebra.Additive
import NumHask.Algebra.Basis
import NumHask.Algebra.Distribution
import NumHask.Algebra.Field
import NumHask.Algebra.Integral
import NumHask.Algebra.Magma
import NumHask.Algebra.Metric
import NumHask.Algebra.Module
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ordering
import NumHask.Algebra.Ring

-- $backend
-- NumHask imports Protolude as the prelude and replaces much of the 'Num' heirarchy in base.
-- Usage of 'Semigroup' and 'Monoid' has been avoided to retain basic compatability.

-- $instances
-- Re-defines the numeric tower.
--
-- Instances for 'Int', 'Integer', 'Float', 'Double', 'Bool' and 'Complex' are supplied
--
