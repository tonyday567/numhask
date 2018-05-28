{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

-- | Integral classes
module NumHask.Algebra.Rational
  ( Ratio(..)
  , Rational
  , ToRatio(..)
  , FromRatio(..)
  , fromRational
  -- * $integral_functionality
  , reduce
  , gcd
  ) where

-- import Data.Coerce
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Float
import GHC.Natural (Natural(..))
import qualified GHC.Real
import qualified Prelude as P
import Prelude (Double, Float, Int, Integer, (.))
import NumHask.Algebra.Additive
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Distribution
import NumHask.Algebra.Integral
import NumHask.Algebra.Metric
import NumHask.Algebra.Ring
import NumHask.Algebra.Field

data Ratio a = !a :% !a deriving (P.Eq, P.Show)

type Rational = Ratio Integer

instance  (P.Ord a, Multiplicative a, Integral a)  => P.Ord (Ratio a)  where
  (x:%y) <= (x':%y')  =  x * y' P.<= x' * y
  (x:%y) <  (x':%y')  =  x * y' P.<  x' * y

instance (P.Ord a, Integral a, Signed a, AdditiveInvertible a) => AdditiveMagma (Ratio a) where
  (x:%y) `plus` (x':%y') =
    reduce ((x `times` y') `plus` (x' `times` y)) (y `times` y')

instance (P.Ord a, Integral a, Signed a, AdditiveInvertible a) => AdditiveUnital (Ratio a) where
  zero = zero :% one

instance (P.Ord a, Signed a, Integral a, AdditiveInvertible a) => AdditiveAssociative (Ratio a)

instance (P.Ord a, Signed a, Integral a, AdditiveInvertible a) => AdditiveCommutative (Ratio a)

instance (P.Ord a, Signed a, Integral a, AdditiveInvertible a) => AdditiveInvertible (Ratio a) where
  negate (x :% y) = negate x :% y

instance (P.Ord a, Signed a, Integral a, AdditiveInvertible a) => Additive (Ratio a)

instance (P.Ord a, Signed a, Integral a, AdditiveGroup a) => AdditiveGroup (Ratio a)

instance (P.Ord a, Signed a, Integral a, AdditiveInvertible a) => MultiplicativeMagma (Ratio a) where
  (x:%y) `times` (x':%y') = reduce (x `times` x') (y `times` y')

instance (P.Ord a, Signed a, Integral a, AdditiveInvertible a) => MultiplicativeUnital (Ratio a) where
  one = one :% one

instance (P.Ord a, Signed a, Integral a, AdditiveInvertible a) =>
         MultiplicativeAssociative (Ratio a)

instance (P.Ord a, Signed a, Integral a, AdditiveInvertible a) =>
         MultiplicativeCommutative (Ratio a)

instance (P.Ord a, Signed a, Integral a, AdditiveInvertible a) =>
         MultiplicativeInvertible (Ratio a) where
  recip (x :% y)
    | x P.< zero = negate y :% negate x
    | P.otherwise = y :% x

instance (Signed a, AdditiveInvertible a, AdditiveUnital a, Integral a, P.Ord a, Multiplicative a) => Multiplicative (Ratio a)

instance (Signed a, AdditiveInvertible a, AdditiveUnital a, Integral a, P.Ord a, Multiplicative a) =>
         MultiplicativeGroup (Ratio a)

instance (P.Ord a, Signed a, Integral a, AdditiveInvertible a) => Distribution (Ratio a)

instance (P.Ord a, Signed a, Integral a, AdditiveInvertible a) => Semiring (Ratio a)
instance (P.Ord a, Signed a, Integral a, AdditiveGroup a) => Ring (Ratio a)
instance (P.Ord a, Signed a, Integral a, Multiplicative a, Ring a) => CRing (Ratio a)
instance (P.Ord a, Signed a, Integral a, Multiplicative a, Ring a) =>
  InvolutiveRing (Ratio a)

instance (P.Ord a, Signed a, Integral a, Multiplicative a, Ring a) =>
  Semifield (Ratio a)

instance (P.Ord a, Signed a, Integral a, Multiplicative a, Ring a) =>
  Field (Ratio a)

instance (P.Ord a, Signed a, ToInteger a, Integral a, Multiplicative a, Ring a, P.Eq b, AdditiveGroup b, Integral b, FromInteger b) => QuotientField (Ratio a) b where
  properFraction (n :% d) = let (w,r) = quotRem n d in (fromIntegral w,r:%d)

instance (P.Ord a, Signed a, Integral a, AdditiveInvertible a, Multiplicative a, Ring a) => UpperBoundedField (Ratio a)

instance (P.Ord a, Signed a, Integral a, Multiplicative a, Ring a, AdditiveInvertible a) => LowerBoundedField (Ratio a)

instance (P.Ord a, Signed a, Integral a, AdditiveInvertible a) => Signed (Ratio a) where
  sign (n :% _)
    | n P.== zero = zero
    | n P.> zero = one
    | P.otherwise = negate one
  abs (n :% d) = abs n :% abs d

instance (P.Ord a, Integral a, Signed a, AdditiveInvertible a) => Normed (Ratio a) (Ratio a) where
  normL1 = abs
  normL2 = abs
  normLp _ = abs

instance (P.Ord a, Integral a, Signed a, AdditiveGroup a) => Metric (Ratio a) (Ratio a) where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)
  distanceLp p a b = normLp p (a - b)

instance (P.Ord a, Signed a, Integral a, AdditiveGroup a) => Epsilon (Ratio a)

instance (FromInteger a, MultiplicativeUnital a) => FromInteger (Ratio a) where
  fromInteger x = fromInteger x :% one

-- | toRatio is equivalent to `Real` in base.
class ToRatio a where
  toRatio :: a -> Ratio Integer

instance (ToInteger a) => ToRatio (Ratio a) where
  toRatio (n :% d) = toInteger n :% toInteger d

-- | `Fractional` in base splits into fromRatio and MultiplicativeGroup
class FromRatio a where
  fromRatio :: Ratio Integer -> a

instance (FromInteger a) => FromRatio (Ratio a) where
  fromRatio (n :% d) = fromInteger n :% fromInteger d

-- | coercion of 'Rational's
--
-- > fromRational a == a
fromRational :: (ToRatio a, FromRatio b) => a -> b
fromRational = fromRatio . toRatio

-- | fixme: use coerce
fromBaseRational :: P.Rational -> Ratio Integer
fromBaseRational (n GHC.Real.:% d) = n :% d

instance FromRatio Double where
  fromRatio (n:%d)= rationalToDouble n d

instance FromRatio Float where
  fromRatio (n:%d)= rationalToFloat n d

instance ToRatio Double where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Float where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Int where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Integer where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Natural where
  toRatio = fromBaseRational . P.toRational

instance ToRatio P.Rational where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Int8 where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Int16 where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Int32 where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Int64 where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Word where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Word8 where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Word16 where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Word32 where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Word64 where
  toRatio = fromBaseRational . P.toRational

-- * $integral_functions
-- integral functionality is largely based on GHC.Real
--
-- | 'reduce' is a subsidiary function used only in this module.
-- It normalises a ratio by dividing both numerator and denominator by
-- their greatest common divisor.
reduce :: (P.Ord a, AdditiveInvertible a, Signed a, Integral a) => a -> a -> Ratio a
reduce x y
  | x P.== zero P.&& y P.== zero = zero :% zero
  | z P.== zero = one :% zero
  | P.otherwise = (x `quot` z) % (y `quot` z)
  where
    z = gcd x y
    n % d
      | d P.< zero = negate n :% negate d
      | P.otherwise = n:%d

-- | @'gcd' x y@ is the non-negative factor of both @x@ and @y@ of which
-- every common factor of @x@ and @y@ is also a factor; for example
-- @'gcd' 4 2 = 2@, @'gcd' (-4) 6 = 2@, @'gcd' 0 4@ = @4@. @'gcd' 0 0@ = @0@.
-- (That is, the common divisor that is \"greatest\" in the divisibility
-- preordering.)
--
-- Note: Since for signed fixed-width integer types, @'abs' 'minBound' < 0@,
-- the result may be negative if one of the arguments is @'minBound'@ (and
-- necessarily is if the other is @0@ or @'minBound'@) for such types.
gcd :: (P.Ord a, Signed a, Integral a) => a -> a -> a
gcd x y =  gcd' (abs x) (abs y)
  where
    gcd' a b
      | b P.== zero = a
      | P.otherwise = gcd' b (a `rem` b)
