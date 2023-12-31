{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Wrapped numhask instances, useful for derivingvia situations to quickly specifiy a numhask friendly numerical type.
module NumHask.Data.Wrapped
  ( Wrapped (..),
  )
where

import NumHask.Algebra.Action
import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Lattice
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import NumHask.Algebra.Metric
import NumHask.Data.Integral
import NumHask.Data.Rational
import qualified Prelude as P

-- | Wrapped numhask instances
newtype Wrapped a = Wrapped {unWrapped :: a}
  deriving
    ( P.Show,
      P.Eq,
      P.Ord,
      Additive,
      Subtractive,
      Multiplicative,
      Divisive,
      ExpField,
      TrigField,
      StarSemiring,
      InvolutiveRing,
      Integral,
      FromInteger,
      FromRational,
      MeetSemiLattice,
      JoinSemiLattice,
      BoundedJoinSemiLattice,
      BoundedMeetSemiLattice,
      Basis,
      Direction,
      Epsilon,
      AdditiveAction,
      SubtractiveAction,
      MultiplicativeAction,
      DivisiveAction
    )

instance
  (P.Ord a, Integral (Whole a), QuotientField a) =>
  QuotientField (Wrapped a)
  where
  type Whole (Wrapped a) = Whole a
  properFraction (Wrapped a) = let (i, r) = properFraction a in (i, Wrapped r)

instance (FromIntegral a b) => FromIntegral (Wrapped a) b where
  fromIntegral a = Wrapped (fromIntegral a)

instance (ToIntegral a b) => ToIntegral (Wrapped a) b where
  toIntegral (Wrapped a) = toIntegral a

instance (FromRatio a b) => FromRatio (Wrapped a) b where
  fromRatio a = Wrapped (fromRatio a)

instance (ToRatio a b) => ToRatio (Wrapped a) b where
  toRatio (Wrapped a) = toRatio a
