{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | Rational classes
module NumHask.Data.Rational
  ( Ratio (..),
    Rational,
    ToRatio (..),
    FromRatio (..),
    FromRational (..),
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
import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Lattice
import NumHask.Algebra.Metric
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import NumHask.Data.Integral
import Prelude (Eq (..), Int, Integer, Ord (..), Ordering (..), Rational, (.))
import qualified Prelude as P

-- $setup
--
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude

-- | A rational number
data Ratio a = !a :% !a deriving (P.Show)

instance (P.Eq a, Subtractive a, Signed a, Integral a) => P.Eq (Ratio a) where
  a@(xa :% ya) == b@(xb :% yb)
    | isRNaN a P.|| isRNaN b = P.False
    | xa == zero P.&& xb == zero = P.True
    | xa == zero P.|| xb == zero = P.False
    | P.otherwise =
        let (xa' :% ya', xb' :% yb') = (reduce xa ya, reduce xb yb)
         in (xa' P.== xb') P.&& (ya' P.== yb')

-- | Has a zero denominator
isRNaN :: (P.Eq a, Additive a) => Ratio a -> P.Bool
isRNaN (x :% y)
  | x P.== zero P.&& y P.== zero = P.True
  | P.otherwise = P.False

instance (P.Ord a, Integral a, Signed a, Subtractive a) => P.Ord (Ratio a) where
  (x :% y) <= (x' :% y') = x * y' P.<= x' * y
  (x :% y) < (x' :% y') = x * y' P.< x' * y

instance (P.Ord a, Signed a, Integral a, Ring a) => Additive (Ratio a) where
  (x :% y) + (x' :% y')
    | y P.== zero P.&& y' P.== zero = bool one (negate one) (x + x' P.< zero) :% zero
    | y P.== zero = x :% y
    | y' P.== zero = x' :% y'
    | P.otherwise = reduce ((x * y') + (x' * y)) (y * y')

  zero = zero :% one

instance (P.Ord a, Signed a, Integral a, Ring a) => Subtractive (Ratio a) where
  negate (x :% y) = negate x :% y

instance (P.Ord a, Signed a, Integral a, Ring a) => Multiplicative (Ratio a) where
  (x :% y) * (x' :% y') = reduce (x * x') (y * y')

  one = one :% one

instance
  (P.Ord a, Signed a, Integral a, Ring a) =>
  Divisive (Ratio a)
  where
  recip (x :% y)
    | sign x P.== negate one = negate y :% negate x
    | P.otherwise = y :% x

instance (P.Ord a, Signed a, Integral a, Ring a) => Distributive (Ratio a)

instance (P.Ord a, Signed a, Integral a, Ring a) => Field (Ratio a)

instance (P.Ord a, P.Ord b, Signed a, Integral a, Ring a, Signed b, Subtractive b, Integral b, FromIntegral b a) => QuotientField (Ratio a) b where
  properFraction (n :% d) = let (w, r) = quotRem n d in (fromIntegral w, r :% d)

instance (P.Ord a, Signed a, Integral a, Ring a) => Signed (Ratio a) where
  sign (n :% _) =
    case compare n zero of
      EQ -> zero
      GT -> one
      LT -> negate one
  abs (n :% d) = abs n :% abs d

instance (P.Ord a, Signed a, Integral a, Ring a) => Norm (Ratio a) (Ratio a) where
  norm = abs
  basis = sign

instance (P.Ord a, Integral a, Signed a, Subtractive a) => JoinSemiLattice (Ratio a) where
  (\/) = P.min

instance (P.Ord a, Integral a, Signed a, Subtractive a) => MeetSemiLattice (Ratio a) where
  (/\) = P.max

instance (P.Ord a, Signed a, Integral a, Ring a, MeetSemiLattice a) => Epsilon (Ratio a)

instance (FromIntegral a b, Multiplicative a) => FromIntegral (Ratio a) b where
  fromIntegral x = fromIntegral x :% one

-- | toRatio is equivalent to `GHC.Real.Real` in base, but is polymorphic in the Integral type.
--
-- >>> toRatio (3.1415927 :: Float) :: Ratio Integer
-- 13176795 :% 4194304
class ToRatio a b where
  toRatio :: a -> Ratio b

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

-- | `GHC.Real.Fractional` in base splits into fromRatio and Field
--
-- >>> fromRatio (5 :% 2 :: Ratio Integer) :: Double
-- 2.5
class FromRatio a b where
  fromRatio :: Ratio b -> a

fromBaseRational :: P.Rational -> Ratio Integer
fromBaseRational (n GHC.Real.:% d) = n :% d

instance FromRatio Double Integer where
  fromRatio (n :% d) = rationalToDouble n d

instance FromRatio Float Integer where
  fromRatio (n :% d) = rationalToFloat n d

instance FromRatio Rational Integer where
  fromRatio (n :% d) = n GHC.Real.% d

-- | fromRational is special in two ways:
--
-- - numeric decimal literals (like "53.66") are interpreted as exactly "fromRational (53.66 :: GHC.Real.Ratio Integer)". The prelude version, GHC.Real.fromRational is used as default (or whatever is in scope if RebindableSyntax is set).
--
-- - The default rules in < https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-750004.3 haskell2010> specify that contraints on 'fromRational' need to be in a form @C v@, where v is a Num or a subclass of Num.
--
-- So a type synonym of `type FromRational a = FromRatio a Integer` doesn't work well with type defaulting; hence the need for a separate class.
class FromRational a where
  fromRational :: P.Rational -> a

instance FromRational Double where
  fromRational (n GHC.Real.:% d) = rationalToDouble n d

instance FromRational Float where
  fromRational (n GHC.Real.:% d) = rationalToFloat n d

instance FromRational (Ratio Integer) where
  fromRational (n GHC.Real.:% d) = n :% d

-- | 'reduce' normalises a ratio by dividing both numerator and denominator by
-- their greatest common divisor.
--
-- >>> reduce 72 60
-- 6 :% 5
--
-- prop> \a b -> reduce a b == a :% b || b == zero
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
-- Note: Since for signed fixed-width integer types, @'abs' 'GHC.Enum.minBound' < 0@,
-- the result may be negative if one of the arguments is @'GHC.Enum.minBound'@ (and
-- necessarily is if the other is @0@ or @'GHC.Enum.minBound'@) for such types.
--
-- >>> gcd 72 60
-- 12
gcd :: (P.Eq a, Signed a, Integral a) => a -> a -> a
gcd x y = gcd' (abs x) (abs y)
  where
    gcd' a b
      | b P.== zero = a
      | P.otherwise = gcd' b (a `rem` b)
