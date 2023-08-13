{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

-- | Additive classes
module NumHask.Algebra.Additive
  ( Additive (..),
    Sum (..),
    sum,
    accsum,
    Subtractive (..),
  )
where

import Control.Applicative
import Data.Foldable (foldl')
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Semigroup (Semigroup (..))
import Data.Traversable (mapAccumL)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Natural (Natural (..))
import Prelude (Applicative, Bool, Double, Eq, Float, Functor, Int, Integer, Ord, Show, fromInteger, ($))
import qualified Prelude as P

-- $setup
--
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude

-- | or [Addition](https://en.wikipedia.org/wiki/Addition)
--
-- For practical reasons, we begin the class tree with 'NumHask.Algebra.Additive.Additive'.  Starting with  'NumHask.Algebra.Group.Associative' and 'NumHask.Algebra.Group.Unital', or using 'Data.Semigroup.Semigroup' and 'Data.Monoid.Monoid' from base tends to confuse the interface once you start having to disinguish between (say) monoidal addition and monoidal multiplication.
--
-- prop> \a -> zero + a == a
-- prop> \a -> a + zero == a
-- prop> \a b c -> (a + b) + c == a + (b + c)
-- prop> \a b -> a + b == b + a
--
-- By convention, (+) is regarded as commutative, but this is not universal, and the introduction of another symbol which means non-commutative addition seems a bit dogmatic.
--
-- >>> zero + 1
-- 1
--
-- >>> 1 + 1
-- 2
class Additive a where
  infixl 6 +
  (+) :: a -> a -> a

  zero :: a


-- | A wrapper for an Additive which distinguishes the additive structure
newtype Sum a = Sum {
  getSum :: a
} deriving (Eq, Ord, Show)

instance Additive a => P.Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)

instance Additive a => P.Monoid (Sum a) where
  mempty = Sum zero

deriving instance Additive a => Additive (Sum a)


-- | Compute the sum of a 'Data.Foldable.Foldable'.
--
-- >>> sum [0..10]
-- 55
sum :: (Additive a, P.Foldable f) => f a -> a
sum = getSum P.. P.foldMap Sum

-- | Compute the accumulating sum of a 'Data.Traversable.Traversable'.
--
-- >>> accsum [0..10]
-- [0,1,3,6,10,15,21,28,36,45,55]
accsum :: (Additive a, P.Traversable f) => f a -> f a
accsum = P.snd P.. mapAccumL (\a b -> (a + b, a + b)) zero

-- | or [Subtraction](https://en.wikipedia.org/wiki/Subtraction)
--
-- prop> \a -> a - a == zero
-- prop> \a -> negate a == zero - a
-- prop> \a -> negate a + a == zero
-- prop> \a -> a + negate a == zero
--
--
-- >>> negate 1
-- -1
--
-- >>> 1 - 2
-- -1
class (Additive a) => Subtractive a where
  negate :: a -> a
  negate a = zero - a

  infixl 6 -
  (-) :: a -> a -> a
  a - b = a + negate b

instance Additive Double where
  (+) = (P.+)
  zero = 0

instance Subtractive Double where
  negate = P.negate

instance Additive Float where
  (+) = (P.+)
  zero = 0

instance Subtractive Float where
  negate = P.negate

instance Additive Int where
  (+) = (P.+)
  zero = 0

instance Subtractive Int where
  negate = P.negate

instance Additive Integer where
  (+) = (P.+)
  zero = 0

instance Subtractive Integer where
  negate = P.negate

instance Additive Bool where
  (+) = (P.||)
  zero = P.False

instance Additive Natural where
  (+) = (P.+)
  zero = 0

instance Subtractive Natural where
  negate = P.negate

instance Additive Int8 where
  (+) = (P.+)
  zero = 0

instance Subtractive Int8 where
  negate = P.negate

instance Additive Int16 where
  (+) = (P.+)
  zero = 0

instance Subtractive Int16 where
  negate = P.negate

instance Additive Int32 where
  (+) = (P.+)
  zero = 0

instance Subtractive Int32 where
  negate = P.negate

instance Additive Int64 where
  (+) = (P.+)
  zero = 0

instance Subtractive Int64 where
  negate = P.negate

instance Additive Word where
  (+) = (P.+)
  zero = 0

instance Subtractive Word where
  negate = P.negate

instance Additive Word8 where
  (+) = (P.+)
  zero = 0

instance Subtractive Word8 where
  negate = P.negate

instance Additive Word16 where
  (+) = (P.+)
  zero = 0

instance Subtractive Word16 where
  negate = P.negate

instance Additive Word32 where
  (+) = (P.+)
  zero = 0

instance Subtractive Word32 where
  negate = P.negate

instance Additive Word64 where
  (+) = (P.+)
  zero = 0

instance Subtractive Word64 where
  negate = P.negate

instance (Additive b) => Additive (a -> b) where
  f + f' = \a -> f a + f' a
  zero _ = zero

instance (Subtractive b) => Subtractive (a -> b) where
  negate f = negate P.. f
