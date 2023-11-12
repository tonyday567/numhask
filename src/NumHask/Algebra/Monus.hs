{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Field classes
module NumHask.Algebra.Monus
  ( Positive (..),
    SemiField,
    LowerTruncated (..),
    UpperTruncated (..),
    Truncated,
    Monus (..),
    Addus (..),
    MonusQuotientField (..),
    MSemiField,
    makeLT,
    makeUT,
    makeT,
    positive,
    maybePositive,
  )
where

import Data.Bool (bool)
import Data.Kind
import NumHask.Algebra.Additive (Additive (..), Subtractive (..), (-))
import NumHask.Algebra.Multiplicative
  ( Divisive (..),
    Multiplicative (..),
  )
import NumHask.Algebra.Ring (Distributive)
import NumHask.Data.Integral (Integral (..), even, FromIntegral (..), FromInteger (..))
import NumHask.Data.Rational (FromRational (..))
import Prelude (Eq, Ord, Show)
import qualified Prelude as P
import NumHask.Algebra.Field (half)
import Data.Maybe

-- $setup
--
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude hiding (QuotientField (..))
-- >>> import NumHask.Algebra.Monus

type SemiField a = (NumHask.Algebra.Ring.Distributive a, Divisive a)

class LowerTruncated a where
  lowerT :: a

class UpperTruncated a where
  upperT :: a

type Truncated a = (LowerTruncated a, UpperTruncated a)

makeLT :: (Monus a, Additive a, LowerTruncated a) => a -> a
makeLT a = a -. zero

makeUT :: (Addus a, Additive a, UpperTruncated a) => a -> a
makeUT a = a +. zero

makeT :: (Additive a, Monus a, Addus a, Truncated a) => a -> a
makeT a = makeUT (makeLT a)

-- | zero is positive
--
-- >>> 1 :: Positive Int
-- Positive {unPositive = 1}
--
-- >>> positive 0 == zero
-- True
--
-- >>> positive (-1)
-- Positive {unPositive = 0}
--
-- >>> maybePositive (-1)
-- Nothing
--
-- -1 :: Positive Int doesn't work (without NegativeLiterals) because the minus sign is (hard-coded) applied after conversion:
--
-- >>> :t -1
-- -1 :: (Subtractive a, FromInteger a) => a
newtype Positive a =
  Positive { unPositive :: a }
  deriving stock
  (Eq, Ord, Show)

instance (Additive a) => Additive (Positive a)
  where
    zero = Positive zero
    (Positive a) + (Positive b) = Positive (a + b)

instance (Multiplicative a) => Multiplicative (Positive a)
  where
    one = Positive one
    (Positive a) * (Positive b) = Positive (a * b)

instance (Divisive a) => Divisive (Positive a)
  where
    recip (Positive a) = Positive (recip a)

instance (Ord a, Integral a) => FromIntegral (Positive a) a
  where
    fromIntegral :: (Ord a, Integral a) => a -> Positive a
    fromIntegral a = positive a

instance (Additive a, Ord a, FromInteger a) => FromInteger (Positive a)
  where
    fromInteger a = positive (fromInteger a)

instance (FromRational a, Additive a, Ord a) => FromRational (Positive a)
  where
    fromRational a = positive (fromRational a)

instance (Integral a) => Integral (Positive a) where
  divMod (Positive a) (Positive b) = (\(n,r) -> (Positive n, Positive r)) (divMod a b)
  quotRem (Positive a) (Positive b) =  (\(n,r) -> (Positive n, Positive r)) (quotRem a b)

positive :: (Ord a, Additive a) => a -> Positive a
positive a = Positive (P.max zero a)

maybePositive :: (Additive a, Ord a) => a -> Maybe (Positive a)
maybePositive a = bool Nothing (Just (Positive a)) (a P.>= zero)

instance (Additive a) => LowerTruncated (Positive a)
  where
    lowerT = Positive zero

instance (Ord a, Subtractive a) => Monus (Positive a)
  where
    (Positive a) -. (Positive b) = positive (a - b)

class Monus a where
  {-# MINIMAL (-.) #-}

  infixl 6 -.
  (-.) :: a -> a -> a
  default (-.) :: (Ord a, LowerTruncated a, Subtractive a) => a -> a -> a
  a -. b = P.max lowerT (a - b)

class Addus a where
  {-# MINIMAL (+.) #-}

  infixl 6 +.
  (+.) :: a -> a -> a
  default (+.) :: (Ord a, UpperTruncated a, Additive a) => a -> a -> a
  a +. b = P.min upperT (a + b)

type MSemiField a = (Monus a, NumHask.Algebra.Ring.Distributive a, Divisive a)

instance MonusQuotientField (Positive P.Double) where
  type MonusWhole (Positive P.Double) = Positive P.Int
  properFraction (Positive a) = (\(n,r) -> (Positive n, Positive r)) (P.properFraction a)

-- | Quotienting of a 'Field' into a 'NumHask.Algebra.Ring'
--
-- See [Field of fractions](https://en.wikipedia.org/wiki/Field_of_fractions)
--
-- > \a -> a - one < floor a <= a <= ceiling a < a + one
class (MSemiField a) => MonusQuotientField a where
  type MonusWhole a :: Type
  properFraction :: a -> (MonusWhole a, a)

  -- | round to the nearest Int
  --
  -- Exact ties are managed by rounding down ties if the whole component is even.
  --
  -- >>> round (1.5 :: Positive Double)
  -- Positive {unPositive = 2}
  --
  -- >>> round (2.5 :: Positive Double)
  -- Positive {unPositive = 2}
  round :: (MSemiField a) => a -> MonusWhole a
  default round :: (Monus (MonusWhole a), MSemiField a, Integral (MonusWhole a), Ord a, Eq (MonusWhole a)) => a -> MonusWhole a
  round x = case properFraction x of
    (n, r) ->
      let m = n + one
       in case P.compare r half of
            P.LT -> n
            P.EQ -> bool m n (even n)
            P.GT -> m

  -- | supply the next upper whole component
  --
  -- >>> ceiling (1.001 :: Positive Double)
  -- Positive {unPositive = 2}
  ceiling :: (MSemiField a) => a -> MonusWhole a
  default ceiling :: (Distributive (MonusWhole a), MSemiField a) => a -> MonusWhole a
  ceiling x = n + one
    where
      (n, _) = properFraction x

  -- | supply the previous lower whole component
  --
  -- >>> floor (1.001 :: Positive Double)
  -- Positive {unPositive = 1}
  floor :: (MSemiField a) => a -> MonusWhole a
  floor x = n
    where
      (n, _) = properFraction x

  -- | for completeness
  --
  truncate :: (MSemiField a) => a -> MonusWhole a
  truncate x = floor x
