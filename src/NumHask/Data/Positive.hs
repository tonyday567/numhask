{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |  A positive number type, defined as existing on [zero, +infinity)
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
import Numeric.Natural (Natural, minusNaturalMaybe)
import NumHask.Algebra.Action
import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Lattice
import NumHask.Algebra.Metric
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import NumHask.Data.Integral
import NumHask.Data.Rational
import NumHask.Data.Wrapped
import Prelude (Eq, Ord, Show)
import Prelude qualified as P

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Data.Positive

-- | A positive number is a number that is contained in [zero,+infinity).
--
-- >>> 1 :: Positive Int
-- UnsafePositive {unPositive = 1}
--
--
-- >>> -1 :: Positive Int
-- ...
--     • No instance for ‘Subtractive (Positive Int)’
--         arising from a use of syntactic negation
-- ...
--
-- zero is positive
--
-- >>> positive 0 == zero
-- True
--
-- The main constructors:
--
-- >>> positive (-1)
-- UnsafePositive {unPositive = 0}
--
-- >>> maybePositive (-1)
-- Nothing
--
-- >>> UnsafePositive (-1)
-- UnsafePositive {unPositive = -1}
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
      UpperBounded
    )
    via (Wrapped a)

instance (MeetSemiLattice a, Integral a) => FromIntegral (Positive a) a where
  fromIntegral a = positive a

instance (FromIntegral a b) => FromIntegral (Positive a) b where
  fromIntegral a = UnsafePositive (fromIntegral a)

instance (ToIntegral a b) => ToIntegral (Positive a) b where
  toIntegral (UnsafePositive a) = toIntegral a

instance (FromRatio a b) => FromRatio (Positive a) b where
  fromRatio a = UnsafePositive (fromRatio a)

instance (ToRatio a b) => ToRatio (Positive a) b where
  toRatio (UnsafePositive a) = toRatio a

instance (Additive a, JoinSemiLattice a) => LowerBounded (Positive a) where
  bottom = UnsafePositive zero

instance QuotientField (Positive P.Double) where
  type Whole (Positive P.Double) = Positive P.Int
  properFraction (UnsafePositive a) = (\(n, r) -> (UnsafePositive n, UnsafePositive r)) (P.properFraction a)
  ceiling = properFraction >>> P.fst >>> (+ one)
  floor = properFraction >>> P.fst
  truncate = floor
  round x = case properFraction x of
    (n, r) ->
      let half_up = r + half
       in case P.compare half_up one of
            P.LT -> n
            P.EQ -> bool (n + one) n (even n)
            P.GT -> n + one

-- | Constructor which returns zero for a negative number.
--
-- >>> positive (-1)
-- UnsafePositive {unPositive = 0}
positive :: (Additive a, MeetSemiLattice a) => a -> Positive a
positive a = UnsafePositive (a /\ zero)

-- | Unsafe constructor.
--
-- >>> positive_ (-one)
-- UnsafePositive {unPositive = -1}
positive_ :: a -> Positive a
positive_ = UnsafePositive

-- | Constructor which returns Nothing if a negative number is supplied.
--
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
-- @since 0.12
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
  default (∸) :: (LowerBounded a, MeetSemiLattice a, Subtractive a) => a -> a -> a
  a ∸ b = bottom /\ (a - b)

-- | A newtype wrapper intended for defining Monus instances by:
-- "x ∸ y = if x < y then zero else x - y"
newtype MonusFromOrd a = MonusFromOrd a
  deriving (Eq, Ord, Additive, Subtractive)

instance (Ord a, Subtractive a) => Monus (MonusFromOrd a) where
  x ∸ y
    | x P.< y     = zero
    | P.otherwise = x - y

-- | It appears that Haskell doesn't have any built in truncated
-- subtraction operation for Word
deriving via MonusFromOrd P.Word instance Monus P.Word

instance Monus Natural where
  x ∸ y = fromMaybe 0 (minusNaturalMaybe x y)

-- | Truncated addition
--
-- @since 0.12
class Addus a where
  {-# MINIMAL (∔) #-}
  infixl 6 ∔
  (∔) :: a -> a -> a
  default (∔) :: (UpperBounded a, JoinSemiLattice a, Additive a) => a -> a -> a
  a ∔ b = top \/ (a + b)
