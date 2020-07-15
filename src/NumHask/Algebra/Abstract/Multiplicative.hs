{-# OPTIONS_GHC -Wall #-}

-- | Multiplicative
module NumHask.Algebra.Abstract.Multiplicative
  ( Multiplicative (..),
    product,
    Divisive (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Natural (Natural (..))
import Prelude (Double, Float, Int, Integer)
import qualified Prelude as P

-- | For practical reasons, 'Multiplicative' has no super classes. Using 'Associative' and 'Unital' from this library, or using 'Semigroup' and 'Monoid' from base tends to complexify the interface once you start having to disinguish between (say) monoidal addition and monoidal multiplication.
--
-- > one * a == a
-- > a * one == a
-- > (a * b) * c == a * (b * c)
-- > a * b == b * a
--
-- By convention, (*) is regarded as commutative, but this is not universal, and the introduction of another symbol which means non-commutative multiplication seems a bit dogmatic.
class Multiplicative a where
  infixl 7 *
  (*) :: a -> a -> a

  one :: a

-- | Compute the product of a 'Foldable'.
product :: (Multiplicative a, P.Foldable f) => f a -> a
product = P.foldr (*) one

-- |
--
-- > a / a = one
-- > recip a = one / a
-- > recip a * a = one
-- > a * recip a = one
class (Multiplicative a) => Divisive a where
  recip :: a -> a

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
