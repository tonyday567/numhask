{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wall #-}

-- | Positive numbers.
--
-- Positivity is enforced via the positive constructor
module NumHask.Data.Positive
  ( Positive,
    positive,
    positive_,
  ) where

import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Lattice
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import NumHask.Analysis.Metric
import NumHask.Data.Integral
import NumHask.Exception
import qualified Prelude as P

-- | Wrapper for positive numbers.  Note that the constructor is not exported.
newtype Positive a = Positive {unPositive :: a}
  deriving
    ( P.Show,
      P.Eq,
      P.Ord,
      Additive,
      Multiplicative,
      Divisive,
      Distributive,
      IntegralDomain,
      Field,
      ExpField,
      TrigField,
      Integral,
      Signed,
      JoinSemiLattice,
      MeetSemiLattice,
      Epsilon
    )

-- not sure if this is correct or needed
type role Positive representational

-- | maybe construct a 'Positive'
positive_ :: (P.Ord a, Additive a) => a -> P.Maybe (Positive a)
positive_ a
  | a P.< zero = P.Nothing
  | P.otherwise = P.Just (Positive a)

-- | Construct a Positive, throwing an error if the input is negative.
positive :: (P.Ord a, Additive a) => a -> Positive a
positive a
  | a P.< zero = throw (NumHaskException "positive number less than zero")
  | P.otherwise = Positive a

instance (P.Ord a, Subtractive a) => Subtractive (Positive a) where
  negate (Positive a)
    | a P.== zero = Positive zero
    | P.otherwise = throw (NumHaskException "negating a positive number")

  (Positive a) - (Positive b)
    | a P.>= b = Positive (a - b)
    | P.otherwise = throw (NumHaskException "subtracting a larger positive")

instance
  (P.Ord a, QuotientField a P.Integer) =>
  QuotientField (Positive a) (Positive P.Integer)
  where
  properFraction (Positive a) = let (i, r) = properFraction a in (Positive i, Positive r)

instance
  (P.Ord a, UpperBoundedField a) =>
  UpperBoundedField (Positive a)
  where
  infinity = Positive infinity

instance (P.Ord a, UpperBoundedField a) => P.Bounded (Positive a) where
  minBound = zero
  maxBound = infinity

instance
  (Normed a a) =>
  Normed a (Positive a)
  where
  norm a = Positive (norm a)

instance (Subtractive a, Normed a a) => Metric a (Positive a) where
  distance a b = Positive P.$ norm (a - b)
