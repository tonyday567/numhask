{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Multiplicative classes
module NumHask.Algebra.Multiplicative
  ( Multiplicative (..),
    product,
    accproduct,
    Divisive (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Traversable (mapAccumL)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Natural (Natural (..))
import Prelude (Double, Float, Int, Integer, fromInteger, fromRational)
import qualified Prelude as P

-- $setup
--
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude

-- | or [Multiplication](https://en.wikipedia.org/wiki/Multiplication)
--
-- For practical reasons, we begin the class tree with 'NumHask.Algebra.Additive.Additive' and 'Multiplicative'.  Starting with  'NumHask.Algebra.Group.Associative' and 'NumHask.Algebra.Group.Unital', or using 'Data.Semigroup.Semigroup' and 'Data.Monoid.Monoid' from base tends to confuse the interface once you start having to disinguish between (say) monoidal addition and monoidal multiplication.
--
--
-- prop> \a -> one * a == a
-- prop> \a -> a * one == a
-- prop> \a b c -> (a * b) * c == a * (b * c)
--
-- By convention, (*) is regarded as not necessarily commutative, but this is not universal, and the introduction of another symbol which means commutative multiplication seems a bit dogmatic.
--
-- >>> one * 2
-- 2
--
-- >>> 2 * 3
-- 6
class Multiplicative a where
  infixl 7 *
  (*) :: a -> a -> a

  one :: a

-- | Compute the product of a 'Data.Foldable.Foldable'.
--
-- >>> product [1..5]
-- 120
product :: (Multiplicative a, P.Foldable f) => f a -> a
product = P.foldr (*) one

-- | Compute the accumulating product of a 'Data.Traversable.Traversable'.
--
-- >>> accproduct [1..5]
-- [1,2,6,24,120]
accproduct :: (Multiplicative a, P.Traversable f) => f a -> f a
accproduct = P.snd P.. mapAccumL (\a b -> (a * b, a * b)) one

-- | or [Division](https://en.wikipedia.org/wiki/Division_(mathematics\))
--
-- Though unusual, the term Divisive usefully fits in with the grammer of other classes and avoids name clashes that occur with some popular libraries.
--
-- prop> \(a :: Double) -> a / a ~= one || a == zero
-- prop> \(a :: Double) -> recip a ~= one / a || a == zero
-- prop> \(a :: Double) -> recip a * a ~= one || a == zero
-- prop> \(a :: Double) -> a * recip a ~= one || a == zero
--
-- >>> recip 2.0
-- 0.5
--
-- >>> 1 / 2
-- 0.5
class (Multiplicative a) => Divisive a where
  recip :: a -> a
  recip a = one / a

  infixl 7 /

  (/) :: a -> a -> a
  (/) a b = a * recip b

instance Multiplicative Double where
  (*) = (P.*)
  one = 1.0

instance Divisive Double where
  recip = P.recip

instance Multiplicative Float where
  (*) = (P.*)
  one = 1.0

instance Divisive Float where
  recip = P.recip

instance Multiplicative Int where
  (*) = (P.*)
  one = 1

instance Multiplicative Integer where
  (*) = (P.*)
  one = 1

instance Multiplicative P.Bool where
  (*) = (P.&&)
  one = P.True

instance Multiplicative Natural where
  (*) = (P.*)
  one = 1

instance Multiplicative Int8 where
  (*) = (P.*)
  one = 1

instance Multiplicative Int16 where
  (*) = (P.*)
  one = 1

instance Multiplicative Int32 where
  (*) = (P.*)
  one = 1

instance Multiplicative Int64 where
  (*) = (P.*)
  one = 1

instance Multiplicative Word where
  (*) = (P.*)
  one = 1

instance Multiplicative Word8 where
  (*) = (P.*)
  one = 1

instance Multiplicative Word16 where
  (*) = (P.*)
  one = 1

instance Multiplicative Word32 where
  (*) = (P.*)
  one = 1

instance Multiplicative Word64 where
  (*) = (P.*)
  one = 1

instance Multiplicative b => Multiplicative (a -> b) where
  f * f' = \a -> f a * f' a
  one _ = one

instance Divisive b => Divisive (a -> b) where
  recip f = recip P.. f
