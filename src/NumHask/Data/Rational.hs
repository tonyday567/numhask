{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Rational classes
module NumHask.Data.Rational
  ( Ratio (..),
    Rational,
    ToRatio (..),
    FromRatio (..),
#if defined(__GLASGOW_HASKELL__)
    FromRational (..),
#endif
    reduce,
    gcd,
  )
where

import Data.Bool (bool)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8)
#if defined(__GLASGOW_HASKELL__)
import GHC.Float qualified
import GHC.Natural (Natural (..))
import GHC.Real qualified
#endif
#if defined(__MHS__)
import Data.Float
import Data.Double
import Data.Ratio_Type
import Data.Ratio
import Data.Fractional
#endif
import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Lattice
import NumHask.Algebra.Metric
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import NumHask.Data.Integral
import Prelude (Eq (..), Int, Integer, Ord (..), Ordering (..), (.), Double, Float)
import Prelude qualified as P

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntaxeal
-- >>> import NumHask.Prelude

-- | A rational number, represented as the ratio of two 'Integral' numbers.
data Ratio a = !a :% !a deriving (P.Show)

-- | Ratio of two integers
type Rational = Ratio Integer

instance (P.Eq a, Subtractive a, EndoBased a, Integral a) => P.Eq (Ratio a) where
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

instance (P.Ord a, Integral a, EndoBased a, Subtractive a) => P.Ord (Ratio a) where
  (x :% y) <= (x' :% y') = x * y' P.<= x' * y
  (x :% y) < (x' :% y') = x * y' P.< x' * y

instance (P.Ord a, EndoBased a, Integral a, Ring a) => Additive (Ratio a) where
  (x :% y) + (x' :% y')
    | y P.== zero P.&& y' P.== zero = bool one (negate one) (x + x' P.< zero) :% zero
    | y P.== zero = x :% y
    | y' P.== zero = x' :% y'
    | P.otherwise = reduce ((x * y') + (x' * y)) (y * y')

  zero = zero :% one

instance (P.Ord a, EndoBased a, Integral a, Ring a) => Subtractive (Ratio a) where
  negate (x :% y) = negate x :% y

instance (P.Ord a, EndoBased a, Integral a, Ring a) => Multiplicative (Ratio a) where
  (x :% y) * (x' :% y') = reduce (x * x') (y * y')

  one = one :% one

instance
  (P.Ord a, EndoBased a, Integral a, Ring a) =>
  Divisive (Ratio a)
  where
  recip (x :% y)
    | signum x P.== negate one = negate y :% negate x
    | P.otherwise = y :% x

instance (P.Ord a, EndoBased a, ToIntegral a Int, Integral a, Ring a) => QuotientField (Ratio a) Int where
  properFraction (n :% d) = let (w, r) = quotRem n d in (toIntegral w, r :% d)

instance (P.Ord a, EndoBased a, Integral a, Ring a) => Basis (Ratio a) (Ratio a) (Ratio a) where
  basis (n :% _) =
    case compare n zero of
      EQ -> zero
      GT -> one
      LT -> negate one
  magnitude (n :% d) = abs n :% abs d

instance (P.Ord a, Integral a, EndoBased a, Subtractive a) => JoinSemiLattice (Ratio a) where
  (\/) = P.min

instance (P.Ord a, Integral a, EndoBased a, Subtractive a) => MeetSemiLattice (Ratio a) where
  (/\) = P.max

instance (P.Ord a, EndoBased a, Integral a, Ring a, MeetSemiLattice a) => Epsilon (Ratio a)

instance (FromIntegral a b, Multiplicative a) => FromIntegral (Ratio a) b where
  fromIntegral x = fromIntegral x :% one

-- | toRatio is equivalent to `GHC.Real.Real` in base, but is polymorphic in the Integral type.
--
-- >>> toRatio (3.1415927 :: Float) :: Ratio Integer
-- 13176795 :% 4194304
class ToRatio a b where
  toRatio :: a -> Ratio b

#if defined(__GLASGOW_HASKELL__)

instance ToRatio Double Integer where
  toRatio = fromRational . P.toRational

instance ToRatio Float Integer where
  toRatio = fromRational . P.toRational

instance ToRatio (Ratio Integer) Integer where
  toRatio = P.id

instance ToRatio Int Integer where
  toRatio = fromRational . P.toRational

instance ToRatio Integer Integer where
  toRatio = fromRational . P.toRational

instance ToRatio Natural Integer where
  toRatio = fromRational . P.toRational

instance ToRatio Int8 Integer where
  toRatio = fromRational . P.toRational

instance ToRatio Int16 Integer where
  toRatio = fromRational . P.toRational

instance ToRatio Int32 Integer where
  toRatio = fromRational . P.toRational

instance ToRatio Int64 Integer where
  toRatio = fromRational . P.toRational

instance ToRatio Word Integer where
  toRatio = fromRational . P.toRational

instance ToRatio Word8 Integer where
  toRatio = fromRational . P.toRational

instance ToRatio Word16 Integer where
  toRatio = fromRational . P.toRational

instance ToRatio Word32 Integer where
  toRatio = fromRational . P.toRational

instance ToRatio Word64 Integer where
  toRatio = fromRational . P.toRational
#endif

-- | `GHC.Real.Fractional` in base splits into fromRatio and Field
--
-- >>> fromRatio (5 :% 2 :: Ratio Integer) :: Double
-- 2.5
class FromRatio a b where
  fromRatio :: Ratio b -> a

#if defined(__GLASGOW_HASKELL__)
toBaseRational :: Ratio Integer -> P.Rational
toBaseRational (n :% d) = n GHC.Real.:% d

instance FromRatio Double Integer where
  fromRatio = P.fromRational . toBaseRational

instance FromRatio Float Integer where
  fromRatio = P.fromRational . toBaseRational
#endif

instance FromRatio Rational Integer where
  fromRatio = P.id

#if defined(__GLASGOW_HASKELL__)
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
  fromRational = P.fromRational

instance FromRational Float where
  fromRational = P.fromRational

instance FromRational (Ratio Integer) where
  fromRational (n GHC.Real.:% d) = n :% d
#endif

-- | 'reduce' normalises a ratio by dividing both numerator and denominator by
-- their greatest common divisor.
--
-- >>> reduce 72 60
-- 6 :% 5
--
-- prop> \a b -> reduce a b == a :% b || b == zero
reduce ::
  (P.Eq a, Subtractive a, EndoBased a, Integral a) => a -> a -> Ratio a
reduce x y
  | x P.== zero P.&& y P.== zero = zero :% zero
  | z P.== zero = one :% zero
  | P.otherwise = (x `quot` z) % (y `quot` z)
  where
    z = gcd x y
    n % d
      | signum d P.== negate one = negate n :% negate d
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
gcd :: (P.Eq a, EndoBased a, Integral a) => a -> a -> a
gcd x y = gcd' (abs x) (abs y)
  where
    gcd' a b
      | b P.== zero = a
      | P.otherwise = gcd' b (a `rem` b)
