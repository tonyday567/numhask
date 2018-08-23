{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Integral classes
module NumHask.Data.Rational
  ( Ratio(..)
  , Rational
  , ToRatio(..)
  , FromRatio(..)
  , fromRational
  -- * $integral_functionality
  , reduce
  , gcd
  )
where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Bool (bool)
import GHC.Float
import GHC.Natural (Natural(..))
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Analysis.Metric
import NumHask.Data.Integral
import Prelude (Double, Float, Int, Integer, (.))
import qualified GHC.Real
import qualified Prelude as P

data Ratio a = !a :% !a deriving (P.Show)

instance (P.Eq a, Additive a) => P.Eq (Ratio a) where
  a == b
    | isRNaN a P.|| isRNaN b = P.False
    | P.otherwise = (x P.== x') P.&& (y P.== y')
      where
        (x:%y) = a
        (x':%y') = b

isRNaN :: (P.Eq a, Additive a) => Ratio a -> P.Bool
isRNaN (x :% y)
  | x P.== zero P.&& y P.== zero = P.True
  | P.otherwise = P.False


type Rational = Ratio Integer

instance  (P.Ord a, Multiplicative a, Integral a)  => P.Ord (Ratio a)  where
  (x:%y) <= (x':%y')  =  x * y' P.<= x' * y
  (x:%y) <  (x':%y')  =  x * y' P.<  x' * y

-- | These common constraints over the Ratio instances are due to the gcd algorithm. Subtractive is somewhat problematic with obtaining a `Ratio (Positive Integer)` which should be made possible.
type GCDConstraints a = (P.Ord a, Signed a, Integral a, Subtractive a)

instance (GCDConstraints a) => Additive (Ratio a) where
  (x :% y) + (x' :% y')
    | y P.== zero P.&& y' P.== zero = (bool one (negate one) (x + x' P.< zero) :% zero)
    | y P.== zero                   = (x :% y)
    | y' P.== zero                  = (x' :% y')
    | P.otherwise = (reduce ((x * y') + (x' * y)) (y * y'))

  zero = zero :% one

instance (GCDConstraints a) => Subtractive (Ratio a) where
  negate (x :% y) = negate x :% y

instance (GCDConstraints a) => Multiplicative (Ratio a) where
  (x:%y) * (x':%y') = reduce (x * x') (y * y')

  one = one :% one

instance (GCDConstraints a) =>
  Divisive (Ratio a) where
  recip (x :% y)
    | sign x P.== negate one = negate y :% negate x
    | P.otherwise = (y :% x)

instance (GCDConstraints a) => Distributive  (Ratio a)

instance (GCDConstraints a) => IntegralDomain (Ratio a)

instance (GCDConstraints a) => Field (Ratio a)

instance (GCDConstraints a, GCDConstraints b, ToInteger a, Field a, FromInteger b) => QuotientField (Ratio a) b where
  properFraction (n :% d) = let (w,r) = quotRem n d in (fromIntegral w,r:%d)

instance (GCDConstraints a, Ring a, IntegralDomain a) =>
  UpperBoundedField (Ratio a) where
  isNaN (a :% b) = (a P.== zero) P.&& (b P.== zero)

instance (GCDConstraints a, Field a) => LowerBoundedField (Ratio a)

instance (GCDConstraints a) => Signed (Ratio a) where
  sign (n :% _)
    | n P.== zero = zero
    | n P.> zero = one
    | P.otherwise = negate one
  abs (n :% d) = abs n :% abs d

instance (GCDConstraints a) => Normed (Ratio a) (Ratio a) where
  normL1 = abs
  normL2 = abs
  normLp _ = abs

instance (GCDConstraints a) => Metric (Ratio a) (Ratio a) where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)
  distanceLp p a b = normLp p (a - b)

instance (GCDConstraints a) => Epsilon (Ratio a)

instance (FromInteger a, Multiplicative a) => FromInteger (Ratio a) where
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
reduce
  :: (P.Eq a, Subtractive a, Signed a, Integral a) => a -> a -> Ratio a
reduce x y
  | x P.== zero P.&& y P.== zero = zero :% zero
  | z P.== zero = one :% zero
  | P.otherwise = (x `quot` z) % (y `quot` z)
 where
  z = gcd x y
  n % d
    | sign d P.== negate one = negate n :% negate d
    | P.otherwise = n :% d

-- | @'gcd' x y@ is the non-negative factor of both @x@ and @y@ of which
-- every common factor of @x@ and @y@ is also a factor; for example
-- @'gcd' 4 2 = 2@, @'gcd' (-4) 6 = 2@, @'gcd' 0 4@ = @4@. @'gcd' 0 0@ = @0@.
-- (That is, the common divisor that is \"greatest\" in the divisibility
-- preordering.)
--
-- Note: Since for signed fixed-width integer types, @'abs' 'minBound' < 0@,
-- the result may be negative if one of the arguments is @'minBound'@ (and
-- necessarily is if the other is @0@ or @'minBound'@) for such types.
gcd :: (P.Eq a, Signed a, Integral a) => a -> a -> a
gcd x y = gcd' (abs x) (abs y)
 where
  gcd' a b
    | b P.== zero = a
    | P.otherwise = gcd' b (a `rem` b)
