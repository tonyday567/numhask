{-# OPTIONS_GHC -Wall #-}

-- | Additive
module NumHask.Algebra.Abstract.Additive
  ( Additive(..)
  , sum
  , Subtractive(..)
  )
where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Natural (Natural(..))
import Prelude (Int, Integer, Float, Double, Bool)
import qualified Prelude as P


class Additive a where
  infixl 6 +
  (+) :: a -> a -> a

  zero :: a

sum :: (Additive a, P.Foldable f) => f a -> a
sum = P.foldr (+) zero

class (Additive a) => Subtractive a where
  negate :: a -> a

  infixl 6 -
  (-) :: a -> a -> a
  (-) a b = a + negate b

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

instance Subtractive Bool where
  negate = P.not

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


instance Additive b => Additive (a -> b) where
  f + f' = \a -> f a + f' a 
  zero _ = zero

instance Subtractive b => Subtractive (a -> b) where
  negate f = negate P.. f
