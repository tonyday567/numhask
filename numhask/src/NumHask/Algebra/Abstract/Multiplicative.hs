{-# OPTIONS_GHC -Wall #-}

-- | Multiplicative
module NumHask.Algebra.Abstract.Multiplicative
  ( Multiplicative(..)
  , product
  , Divisive(..)
  )
where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Natural (Natural(..))
import Prelude (Int, Integer, Float, Double)
import qualified Prelude as P

class Multiplicative a where

  infixl 7 *
  (*) :: a -> a -> a

  one :: a

product :: (Multiplicative a, P.Foldable f) => f a -> a
product = P.foldr (*) one

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

