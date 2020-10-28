{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wall #-}

-- | Wrapped numhask instances, useful for derivingvia situations to quickly specifiy a numhask friendly numerical type.
module NumHask.Data.Wrapped where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Lattice
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Algebra.Abstract.Module
import NumHask.Analysis.Metric
import NumHask.Data.Integral
import NumHask.Data.Rational
import qualified Prelude as P

-- | Wrapped numeric instances
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
      LowerBoundedField
    )

-- TODO: not sure if this is correct or needed
type role Wrapped representational

instance
  (P.Ord a, QuotientField a P.Integer) =>
  QuotientField (Wrapped a) (Wrapped P.Integer)
  where
  properFraction (Wrapped a) = let (i, r) = properFraction a in (Wrapped i, Wrapped r)

instance (FromIntegral a b) => FromIntegral (Wrapped a) b where
  fromIntegral a = Wrapped (fromIntegral a)

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

instance (Additive a, AdditiveAction m a) => AdditiveAction m (Wrapped a) where
  (.+) (Wrapped a) m = a .+ m
  (+.) m (Wrapped a) = m +. a

instance (Subtractive a, SubtractiveAction m a) => SubtractiveAction m (Wrapped a) where
  (.-) (Wrapped a) m = a .- m
  (-.) m (Wrapped a) = m -. a

instance (Multiplicative a, MultiplicativeAction m a) => MultiplicativeAction m (Wrapped a) where
  (.*) (Wrapped a) m = a .* m
  (*.) m (Wrapped a) = m *. a

instance (Divisive a, DivisiveAction m a) => DivisiveAction m (Wrapped a) where
  (./) (Wrapped a) m = a ./ m
  (/.) m (Wrapped a) = m /. a
