{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wall #-}

module NumHask.Data.Wrapped where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Algebra.Abstract.Lattice
import NumHask.Algebra.Abstract.Group
import NumHask.Analysis.Metric
import NumHask.Data.Integral
import NumHask.Data.Rational
import qualified Prelude as P

newtype Wrapped a = Wrapped { unWrapped :: a }
  deriving
    ( P.Show
    , P.Eq
    , P.Ord
    , Magma
    , Idempotent
    , Additive
    , Subtractive
    , Multiplicative
    , Divisive
    , Distributive
    , IntegralDomain
    , InvolutiveRing
    , StarSemiring
    , KleeneAlgebra
    , Field
    , ExpField
    , TrigField
    , Integral
    , Signed
    , MeetSemiLattice
    , JoinSemiLattice
    , Epsilon
    , UpperBoundedField
    , LowerBoundedField
    )

-- not sure if this is correct or needed
type role Wrapped representational

instance (P.Ord a, QuotientField a P.Integer) =>
  QuotientField (Wrapped a) (Wrapped P.Integer) where
  properFraction (Wrapped a) = let (i,r) = properFraction a in (Wrapped i, Wrapped r)

instance (FromIntegral a b) => FromIntegral (Wrapped a) b where
  fromIntegral_ a = Wrapped (fromIntegral_ a)

instance (ToIntegral a b) => ToIntegral (Wrapped a) b where
  toIntegral_ (Wrapped a) = toIntegral_ a

instance (FromRatio a b) => FromRatio (Wrapped a) b where
  fromRatio a = Wrapped (fromRatio a)

instance (ToRatio a b) => ToRatio (Wrapped a) b where
  toRatio (Wrapped a) = toRatio a

instance (Normed a b) => Normed (Wrapped a) (Wrapped b) where
  normL1 (Wrapped a) = Wrapped (normL1 a)
  normL2 (Wrapped a) = Wrapped (normL2 a)

instance (Metric a b) => Metric (Wrapped a) (Wrapped b) where
  distanceL1 (Wrapped a) (Wrapped b) = Wrapped (distanceL1 a b)
  distanceL2 (Wrapped a) (Wrapped b) = Wrapped (distanceL2 a b)
