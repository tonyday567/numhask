{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Field classes
module NumHask.Data.Positive
  ( Positive (..),
    positive,
    maybePositive,
    positive_,
    Monus (..),
    Addus (..),
    MonusSemiField,
  )
where

import Control.Category ((>>>))
import Data.Bool (bool)
import Data.Maybe
import NumHask.Algebra.Action
import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Lattice
import NumHask.Algebra.Metric
import NumHask.Algebra.Ring
import NumHask.Data.Integral
import NumHask.Data.Rational
import NumHask.Data.Wrapped
import Prelude (Eq, Ord, Show)
import qualified Prelude as P

-- $setup
--
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Data.Positive

-- | zero is positive
--
-- >>> 1 :: Positive Int
-- UnsafePositive {unPositive = 1}
--
-- >>> positive 0 == zero
-- True
--
-- >>> positive (-1)
-- UnsafePositive {unPositive = 0}
--
-- >>> maybePositive (-1)
-- Nothing
--
newtype Positive a = UnsafePositive {unPositive :: a}
  deriving stock
    (Eq, Ord, Show)
  deriving
    ( Additive,
      Multiplicative,
      Divisive,
      Integral,
      FromInteger,
      FromRational,
      Basis,
      Direction,
      Epsilon,
      AdditiveAction,
      SubtractiveAction,
      MultiplicativeAction,
      DivisiveAction,
      JoinSemiLattice,
      MeetSemiLattice,
      BoundedMeetSemiLattice
    )
    via (Wrapped a)

instance (MeetSemiLattice a , Integral a) => FromIntegral (Positive a) a where
  fromIntegral a = positive a

instance (FromIntegral a b) => FromIntegral (Positive a) b where
  fromIntegral a = UnsafePositive (fromIntegral a)

instance (ToIntegral a b) => ToIntegral (Positive a) b where
  toIntegral (UnsafePositive a) = toIntegral a

instance (FromRatio a b) => FromRatio (Positive a) b where
  fromRatio a = UnsafePositive (fromRatio a)

instance (ToRatio a b) => ToRatio (Positive a) b where
  toRatio (UnsafePositive a) = toRatio a

instance (Additive a, JoinSemiLattice a) => BoundedJoinSemiLattice (Positive a)
  where
    bottom = UnsafePositive zero

instance QuotientField (Positive P.Double) where
  type Whole (Positive P.Double) = Positive P.Int
  properFraction (UnsafePositive a) = (\(n, r) -> (UnsafePositive n, UnsafePositive r)) (P.properFraction a)
  ceiling = properFraction >>> P.fst >>> (+one)
  floor = properFraction >>> P.fst
  truncate = floor
  round x = case properFraction x of
    (n, r) ->
      let half_up = r + half
       in case P.compare half_up one of
            P.LT -> n
            P.EQ -> bool (n + one) n (even n)
            P.GT -> n + one

-- | Constructor which returns zero for a negative input.
--
-- >>> positive (-1)
-- UnsafePositive {unPositive = 0}
positive :: (Additive a, MeetSemiLattice a) => a -> Positive a
positive a = UnsafePositive (a /\ zero)

-- | Unsafe constructors.
--
-- >>> positive_ (-one)
-- UnsafePositive {unPositive = -1}
positive_ :: a -> Positive a
positive_ = UnsafePositive

-- | Constructor which returns Nothing for a negative number.
-- >>> maybePositive (-one)
-- Nothing
maybePositive :: (Additive a, MeetSemiLattice a) => a -> Maybe (Positive a)
maybePositive a = bool Nothing (Just (UnsafePositive a)) (a `meetLeq` zero)

instance (Subtractive a, MeetSemiLattice a) => Monus (Positive a) where
  (UnsafePositive a) ∸ (UnsafePositive b) = positive (a - b)

-- | A field but with truncated subtraction.
type MonusSemiField a = (Monus a, Distributive a, Divisive a)

-- | <https://en.wikipedia.org/wiki/Monus Monus> or truncated subtraction.
--
-- >>> positive 4 ∸ positive 7
-- UnsafePositive {unPositive = 0}
--
-- >>> 4 ∸ 7 :: Positive Int
-- UnsafePositive {unPositive = 0}
class Monus a where
  {-# MINIMAL (∸) #-}

  infixl 6 ∸
  (∸) :: a -> a -> a
  default (∸) :: (BoundedJoinSemiLattice a, MeetSemiLattice a, Subtractive a) => a -> a -> a
  a ∸ b = bottom /\ (a - b)

-- | Truncated addition
class Addus a where
  {-# MINIMAL (∔) #-}
  infixl 6 ∔
  (∔) :: a -> a -> a
  default (∔) :: (BoundedMeetSemiLattice a, JoinSemiLattice a, Additive a) => a -> a -> a
  a ∔ b = top \/ (a + b)

