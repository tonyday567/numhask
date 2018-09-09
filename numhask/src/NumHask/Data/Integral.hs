{-# OPTIONS_GHC -Wall #-}

-- | Integral classes
module NumHask.Data.Integral
  ( Integral(..)
  , ToInteger(..)
  , FromInteger(..)
  , fromIntegral
  , even
  , odd
  , (^)
  , (^^)
  )
where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Natural (Natural(..))
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import Prelude (Double, Float, Int, Integer, (.), fst, snd)
import qualified Prelude as P

-- | Integral laws
--
-- > b == zero || b * (a `div` b) + (a `mod` b) == a
class (Distributive a) =>
  Integral a where
  infixl 7 `div`, `mod`
  div :: a -> a -> a
  div a1 a2 = fst (divMod a1 a2)
  mod :: a -> a -> a
  mod a1 a2 = snd (divMod a1 a2)

  divMod :: a -> a -> (a, a)

  quot :: a -> a -> a
  quot a1 a2 = fst (quotRem a1 a2)
  rem :: a -> a -> a
  rem a1 a2 = snd (quotRem a1 a2)

  quotRem :: a -> a -> (a, a)

instance Integral Int where
  divMod = P.divMod
  quotRem = P.quotRem

instance Integral Integer where
  divMod = P.divMod
  quotRem = P.quotRem

instance Integral Natural where
  divMod = P.divMod
  quotRem = P.quotRem

instance Integral Int8 where
  divMod = P.divMod
  quotRem = P.quotRem

instance Integral Int16 where
  divMod = P.divMod
  quotRem = P.quotRem

instance Integral Int32 where
  divMod = P.divMod
  quotRem = P.quotRem

instance Integral Int64 where
  divMod = P.divMod
  quotRem = P.quotRem

instance Integral Word where
  divMod = P.divMod
  quotRem = P.quotRem

instance Integral Word8 where
  divMod = P.divMod
  quotRem = P.quotRem

instance Integral Word16 where
  divMod = P.divMod
  quotRem = P.quotRem

instance Integral Word32 where
  divMod = P.divMod
  quotRem = P.quotRem

instance Integral Word64 where
  divMod = P.divMod
  quotRem = P.quotRem

instance Integral b => Integral (a -> b) where
  div f f' a = f a `div` f' a
  mod f f' a = f a `mod` f' a
  divMod f f' = (\a -> fst (f a `divMod` f' a), \a -> snd (f a `divMod` f' a))
  quot f f' a = f a `mod` f' a
  rem f f' a = f a `mod` f' a
  quotRem f f' = (\a -> fst (f a `quotRem` f' a), \a -> snd (f a `quotRem` f' a))

-- | toInteger is kept separate from Integral to help with compatability issues.
class ToInteger a where
  toInteger :: a -> Integer

instance ToInteger Int where
  toInteger = P.toInteger

instance ToInteger Integer where
  toInteger = P.toInteger

instance ToInteger Natural where
  toInteger = P.toInteger

instance ToInteger Int8 where
  toInteger = P.toInteger

instance ToInteger Int16 where
  toInteger = P.toInteger

instance ToInteger Int32 where
  toInteger = P.toInteger

instance ToInteger Int64 where
  toInteger = P.toInteger

instance ToInteger Word where
  toInteger = P.toInteger

instance ToInteger Word8 where
  toInteger = P.toInteger

instance ToInteger Word16 where
  toInteger = P.toInteger

instance ToInteger Word32 where
  toInteger = P.toInteger

instance ToInteger Word64 where
  toInteger = P.toInteger

-- | fromInteger is the most problematic of the 'Num' class operators.  Particularly heinous, it is assumed that any number type can be constructed from an Integer, so that the broad classes of objects that are composed of multiple elements is avoided in haskell.
class FromInteger a where
  fromInteger :: Integer -> a

instance FromInteger b => FromInteger (a -> b) where
  fromInteger i _ = fromInteger i

-- | coercion of 'Integral's
--
-- > fromIntegral a == a
fromIntegral :: (ToInteger a, FromInteger b) => a -> b
fromIntegral = fromInteger . toInteger

instance FromInteger Double where
  fromInteger = P.fromInteger

instance FromInteger Float where
  fromInteger = P.fromInteger

instance FromInteger Int where
  fromInteger = P.fromInteger

instance FromInteger Integer where
  fromInteger = P.fromInteger

instance FromInteger Natural where
  fromInteger = P.fromInteger

instance FromInteger Int8 where
  fromInteger = P.fromInteger

instance FromInteger Int16 where
  fromInteger = P.fromInteger

instance FromInteger Int32 where
  fromInteger = P.fromInteger

instance FromInteger Int64 where
  fromInteger = P.fromInteger

instance FromInteger Word where
  fromInteger = P.fromInteger

instance FromInteger Word8 where
  fromInteger = P.fromInteger

instance FromInteger Word16 where
  fromInteger = P.fromInteger

instance FromInteger Word32 where
  fromInteger = P.fromInteger

instance FromInteger Word64 where
  fromInteger = P.fromInteger

-- $operators

even :: (P.Eq a, Integral a) => a -> P.Bool
even n = n `rem` (one + one) P.== zero

odd :: (P.Eq a, Integral a) => a -> P.Bool
odd = P.not . even

-------------------------------------------------------
-- | raise a number to a non-negative integral power
(^)
  :: (P.Ord b, Multiplicative a, Integral b)
  => a
  -> b
  -> a
x0 ^ y0
  | y0 P.< zero = P.undefined
  | -- P.errorWithoutStackTrace "Negative exponent"
    y0 P.== zero = one
  | P.otherwise = f x0 y0
  where

  -- f : x0 ^ y0 = x ^ y
  f x y
    | even y = f (x * x) (y `quot` two)
    | y P.== one = x
    | P.otherwise = g (x * x) (y `quot` two) x
          -- See Note [Half of y - 1]
  -- g : x0 ^ y0 = (x ^ y) * z
  g x y z
    | even y = g (x * x) (y `quot` two) z
    | y P.== one = x * z
    | P.otherwise = g (x * x) (y `quot` two) (x * z)
                -- See Note [Half of y - 1]

(^^)
  :: (Divisive a, Subtractive b, Integral b, P.Ord b) => a -> b -> a
(^^) x n = if n P.>= zero then x ^ n else recip (x ^ negate n)
