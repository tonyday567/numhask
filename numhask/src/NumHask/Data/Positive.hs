{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wall #-}

module NumHask.Data.Positive where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Algebra.Abstract.Lattice
import NumHask.Analysis.Metric
import NumHask.Data.Integral
import NumHask.Exception
import qualified Prelude as P

newtype Positive a = Positive { unPositive :: a }
  deriving
    ( P.Show
    , P.Eq
    , P.Ord
    , Additive
    , Multiplicative
    , Divisive
    , Distributive
    , IntegralDomain
    , Field
    , ExpField
    , TrigField
    , Integral
    , Signed
    , JoinSemiLattice
    , MeetSemiLattice
    , Epsilon
    )

-- not sure if this is correct or needed
type role Positive representational

positive :: (P.Ord a, Additive a) => a -> P.Maybe (Positive a)
positive a
  | a P.< zero = P.Nothing
  | P.otherwise = P.Just (Positive a)

positive_ :: (P.Ord a, Additive a) => a -> Positive a
positive_ a
  | a P.< zero = throw (NumHaskException "positive number less than zero")
  | P.otherwise = Positive a

instance (P.Ord a, Subtractive a) => Subtractive (Positive a) where
  negate (Positive a)
    | a P.== zero = Positive zero
    | P.otherwise = throw (NumHaskException "negating a positive number")

  (Positive a) - (Positive b)
    | a P.>= b = Positive (a - b)
    | P.otherwise = throw (NumHaskException "subtracting a larger positive")

instance (P.Ord a, QuotientField a P.Integer) =>
  QuotientField (Positive a) (Positive P.Integer) where
  properFraction (Positive a) = let (i,r) = properFraction a in (Positive i, Positive r)

instance (UpperBoundedField a) =>
  UpperBoundedField (Positive a) where
  infinity = Positive infinity

instance (UpperBoundedField a) => P.Bounded (Positive a) where
  minBound = zero
  maxBound = infinity

-- Metric
instance (Normed a a) =>
  Normed a (Positive a) where
  normL1 a = Positive (normL1 a)
  normL2 a = Positive (normL2 a)

instance (Subtractive a, Normed a a) => Metric a (Positive a) where
  distanceL1 a b = Positive P.$ normL1 (a - b)
  distanceL2 a b = Positive P.$ normL2 (a - b)
