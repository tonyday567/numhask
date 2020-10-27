{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wall #-}

module NumHask.Data.Wrapped where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Lattice
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Analysis.Metric
import NumHask.Data.Integral
import NumHask.Data.Rational
import qualified Prelude as P

newtype Wrapped a = Wrapped {unWrapped :: a}
  deriving
    ( P.Show,
      P.Eq,
      P.Ord,
      Magma,
      Idempotent,
      Additive,
      Subtractive,
      Multiplicative,
      Divisive,
      Distributive,
      Ring,
      Semiring,
      IntegralDomain,
      InvolutiveRing,
      StarSemiring,
      KleeneAlgebra,
      Field,
      ExpField,
      TrigField,
      Integral,
      Signed,
      MeetSemiLattice,
      JoinSemiLattice,
      BoundedJoinSemiLattice,
      BoundedMeetSemiLattice,
      Epsilon,
      UpperBoundedField,
      LowerBoundedField,
      FromInteger,
      FromRational
    )

-- TODO: not sure if this is correct or needed
type role Wrapped representational

instance
  (P.Ord a, QuotientField a P.Integer) =>
  QuotientField (Wrapped a) (Wrapped P.Integer)
  where
  properFraction (Wrapped a) = let (i, r) = properFraction a in (Wrapped i, Wrapped r)

instance (FromIntegral a b) => FromIntegral (Wrapped a) b where
  fromIntegral_ a = Wrapped (fromIntegral_ a)

instance (ToIntegral a b) => ToIntegral (Wrapped a) b where
  toIntegral (Wrapped a) = toIntegral a

instance (FromRatio a b) => FromRatio (Wrapped a) b where
  fromRatio a = Wrapped (fromRatio a)

instance (ToRatio a b) => ToRatio (Wrapped a) b where
  toRatio (Wrapped a) = toRatio a

instance (Normed a b) => Normed (Wrapped a) (Wrapped b) where
  norm (Wrapped a) = Wrapped (norm a)

instance (Metric a b) => Metric (Wrapped a) (Wrapped b) where
  distance (Wrapped a) (Wrapped b) = Wrapped (distance a b)

{- FIXME: Actor, Actions and Module instances stuck on

Illegal type synonym family application ‘Actor h’ in instance:
  Additive (Wrapped (Actor h))
  In the instance declaration for ‘Additive (Wrapped (Actor h))’
instance (Additive (Actor h)) => Additive (Wrapped (Actor h)) where
  (+) (Wrapped a) (Wrapped b) = Wrapped (a + b)

-}

