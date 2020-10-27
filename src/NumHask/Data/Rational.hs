{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | Integral classes
module NumHask.Data.Rational
  ( Ratio (..),
    Rational,
    ToRatio (..),
    ToRational,
    toRational,
    FromRatio (..),
    FromRational,
    fromRational,
    fromRational',
    fromBaseRational,

    -- * \$integral_functionality
    reduce,
    gcd,
  )
where

import Data.Bool (bool)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Float
import GHC.Natural (Natural (..))
import qualified GHC.Real
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Lattice
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Analysis.Metric
import NumHask.Data.Integral
import Prelude ((.), Int, Integer, Rational)
import qualified Prelude as P

data Ratio a = !a :% !a deriving (P.Show)

instance (P.Eq a, Additive a) => P.Eq (Ratio a) where
  a == b
    | isRNaN a P.|| isRNaN b = P.False
    | P.otherwise = (x P.== x') P.&& (y P.== y')
    where
      (x :% y) = a
      (x' :% y') = b

isRNaN :: (P.Eq a, Additive a) => Ratio a -> P.Bool
isRNaN (x :% y)
  | x P.== zero P.&& y P.== zero = P.True
  | P.otherwise = P.False

instance (P.Ord a, Multiplicative a, Additive a) => P.Ord (Ratio a) where
  (x :% y) <= (x' :% y') = x * y' P.<= x' * y
  (x :% y) < (x' :% y') = x * y' P.< x' * y

-- | These common constraints over the Ratio instances are due to the gcd algorithm. Subtractive is somewhat problematic with obtaining a `Ratio (Positive Integer)` which should be made possible.
type GCDConstraints a = (P.Ord a, Signed a, Integral a, Subtractive a)

instance (GCDConstraints a) => Additive (Ratio a) where
  (x :% y) + (x' :% y')
    | y P.== zero P.&& y' P.== zero = bool one (negate one) (x + x' P.< zero) :% zero
    | y P.== zero = x :% y
    | y' P.== zero = x' :% y'
    | P.otherwise = reduce ((x * y') + (x' * y)) (y * y')

  zero = zero :% one

instance (GCDConstraints a) => Subtractive (Ratio a) where
  negate (x :% y) = negate x :% y

instance (GCDConstraints a) => Multiplicative (Ratio a) where
  (x :% y) * (x' :% y') = reduce (x * x') (y * y')

  one = one :% one

instance
  (GCDConstraints a) =>
  Divisive (Ratio a)
  where
  recip (x :% y)
    | sign x P.== negate one = negate y :% negate x
    | P.otherwise = y :% x

instance (GCDConstraints a) => Distributive (Ratio a)

instance (GCDConstraints a) => IntegralDomain (Ratio a)

instance (GCDConstraints a) => Field (Ratio a)

instance (GCDConstraints a, GCDConstraints b, ToInteger a, Field a, FromIntegral b a) => QuotientField (Ratio a) b where
  properFraction (n :% d) = let (w, r) = quotRem n d in (fromIntegral_ w, r :% d)

instance
  (GCDConstraints a, Distributive a, IntegralDomain a) =>
  UpperBoundedField (Ratio a)

instance (GCDConstraints a, Field a) => LowerBoundedField (Ratio a)

instance (GCDConstraints a) => Signed (Ratio a) where
  sign (n :% _)
    | n P.== zero = zero
    | n P.> zero = one
    | P.otherwise = negate one
  abs (n :% d) = abs n :% abs d

instance (GCDConstraints a) => Normed (Ratio a) (Ratio a) where
  norm = abs

instance (GCDConstraints a) => Metric (Ratio a) (Ratio a) where
  distance a b = norm (a - b)

instance (GCDConstraints a, MeetSemiLattice a) => Epsilon (Ratio a)

instance (FromIntegral a b, Multiplicative a) => FromIntegral (Ratio a) b where
  fromIntegral_ x = fromIntegral_ x :% one

-- | toRatio is equivalent to `Real` in base, but is polymorphic in the Integral type.
class ToRatio a b where
  toRatio :: a -> Ratio b
  default toRatio :: (Ratio c ~ a, ToIntegral c Integer, ToRatio (Ratio b) b, FromInteger b) => a -> Ratio b
  toRatio (n :% d) = toRatio ((fromIntegral n :: b) :% fromIntegral d)

type ToRational a = ToRatio a Integer

toRational :: (ToRatio a Integer) => a -> Ratio Integer
toRational = toRatio

instance ToRatio Double Integer where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Float Integer where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Rational Integer where
  toRatio = fromBaseRational

instance ToRatio (Ratio Integer) Integer where
  toRatio = P.id

instance ToRatio Int Integer where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Integer Integer where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Natural Integer where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Int8 Integer where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Int16 Integer where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Int32 Integer where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Int64 Integer where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Word Integer where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Word8 Integer where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Word16 Integer where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Word32 Integer where
  toRatio = fromBaseRational . P.toRational

instance ToRatio Word64 Integer where
  toRatio = fromBaseRational . P.toRational

-- | `Fractional` in base splits into fromRatio and Field
-- FIXME: work out why the default type isn't firing so that an explicit instance is needed
-- for `FromRatio (Ratio Integer) Integer`
class FromRatio a b where
  fromRatio :: Ratio b -> a
  -- default fromRatio :: (a ~ Ratio c, ToIntegral b c) => Ratio b -> a
  -- fromRatio (n :% d) = toIntegral n :% toIntegral d
  default fromRatio :: (Ratio b ~ a) => Ratio b -> a
  fromRatio = P.id

fromBaseRational :: P.Rational -> Ratio Integer
fromBaseRational (n GHC.Real.:% d) = n :% d

instance FromRatio Double Integer where
  fromRatio (n :% d) = rationalToDouble n d

instance FromRatio Float Integer where
  fromRatio (n :% d) = rationalToFloat n d

instance FromRatio Rational Integer where
  fromRatio (n :% d) = n GHC.Real.% d

instance FromRatio (Ratio Integer) Integer where
  fromRatio = P.id

-- | with RebindableSyntax the literal '1.0' mean exactly `fromRational (1.0::GHC.Real.Rational)`.
class FromRational a where
  fromRational :: P.Rational -> a
  default fromRational :: (FromRatio a Integer) => P.Rational -> a
  fromRational = fromRatio . fromBaseRational

instance FromRational Double

instance FromRational Float

instance FromRational Rational

-- | FIXME: Given that fromRational is reserved, fromRational' provides general conversion between numhask rationals.
fromRational' :: (FromRatio b Integer, ToRatio a Integer) => a -> b
fromRational' a = fromRatio (toRatio a :: Ratio Integer)

instance (GCDConstraints a) => JoinSemiLattice (Ratio a) where
  (\/) = P.min

instance (GCDConstraints a) => MeetSemiLattice (Ratio a) where
  (/\) = P.max

-- * \$integral_functions
-- integral functionality is largely based on GHC.Real
--

-- | 'reduce' is a subsidiary function used only in this module.
-- It normalises a ratio by dividing both numerator and denominator by
-- their greatest common divisor.
reduce ::
  (P.Eq a, Subtractive a, Signed a, Integral a) => a -> a -> Ratio a
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
