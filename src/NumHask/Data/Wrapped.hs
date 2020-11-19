{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Wrapped numhask instances, useful for derivingvia situations to quickly specifiy a numhask friendly numerical type.
module NumHask.Data.Wrapped
  ( Wrapped (..),
  )
where

import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Group
import NumHask.Algebra.Lattice
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
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
