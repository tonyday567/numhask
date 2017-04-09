{-# OPTIONS_GHC -Wall #-}

-- | A prelude for NumHask

module NumHask.Prelude (
    -- * Backend
    -- $backend
    module Protolude
  , module Data.Distributive
  , module Data.Functor.Rep
    -- * Algebraic Heirarchy
    -- $instances
  , module NumHask.Algebra.Additive
  , module NumHask.Algebra.Basis
  , module NumHask.Algebra.Distribution
  , module NumHask.Algebra.Exponential
  , module NumHask.Algebra.Field
  , module NumHask.Algebra.Integral
  , module NumHask.Algebra.Magma
  , module NumHask.Algebra.Metric
  , module NumHask.Algebra.Module
  , module NumHask.Algebra.Multiplicative
  , module NumHask.Algebra.Ordering
  , module NumHask.Algebra.Ring
    -- * Representations
    -- $representables
  , module NumHask.Matrix
  , module NumHask.Tensor
  , module NumHask.Vector
  , module NumHask.Naperian
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
import NumHask.Algebra.Exponential
import NumHask.Algebra.Field
import NumHask.Algebra.Integral
import NumHask.Algebra.Magma
import NumHask.Algebra.Metric
import NumHask.Algebra.Module
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ordering
import NumHask.Algebra.Ring

import NumHask.Matrix
import NumHask.Tensor
import NumHask.Vector
import NumHask.Naperian

import Data.Distributive
import Data.Functor.Rep

-- $backend
-- NumHask imports Protolude as the prelude and replaces much of the 'Num' heirarchy in base.
-- Usage of 'Semigroup' and 'Monoid' has been avoided to retain basic compatability.

-- $instances
-- Re-defines the numeric tower.
--
-- Instances for 'Int', 'Integer', 'Float', 'Double', 'Bool' and 'Representable' Functors are supplied
--

-- $representables
-- Different classes are supplied for holding shape information at the type level and value level.
--
-- Value-level classes are not (yet) wired in to the Algebra
--
-- Type-level shaped numbers are wired in via the 'Representable' 'Functor' instances.
--
