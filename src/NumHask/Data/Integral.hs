{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

-- | Integral classes
module NumHask.Data.Integral
  ( Integral (..),
    ToIntegral (..),
    ToInteger,
    toInteger,
    FromIntegral (..),
    FromInteger (..),
    fromIntegral,
    even,
    odd,
    (^),
    (^^),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Natural (Natural (..))
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import Prelude ((.), Double, Float, Int, Integer, fst, snd)
import qualified Prelude as P

-- | Integral laws
--
-- > b == zero || b * (a `div` b) + (a `mod` b) == a
class
  (Distributive a) =>
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

-- | toIntegral is kept separate from Integral to help with compatability issues.
-- > toIntegral a == a
class ToIntegral a b where
  toIntegral :: a -> b
  default toIntegral :: (a ~ b) => a -> b
  toIntegral = P.id

type ToInteger a = ToIntegral a Integer

-- fitting in with legacy naming conventions.
toInteger :: (ToInteger a) => a -> Integer
toInteger = toIntegral

instance ToIntegral Integer Integer where
  toIntegral = P.id

instance ToIntegral Int Integer where
  toIntegral = P.toInteger

instance ToIntegral Natural Integer where
  toIntegral = P.toInteger

instance ToIntegral Int8 Integer where
  toIntegral = P.toInteger

instance ToIntegral Int16 Integer where
  toIntegral = P.toInteger

instance ToIntegral Int32 Integer where
  toIntegral = P.toInteger

instance ToIntegral Int64 Integer where
  toIntegral = P.toInteger

instance ToIntegral Word Integer where
  toIntegral = P.toInteger

instance ToIntegral Word8 Integer where
  toIntegral = P.toInteger

instance ToIntegral Word16 Integer where
  toIntegral = P.toInteger

instance ToIntegral Word32 Integer where
  toIntegral = P.toInteger

instance ToIntegral Word64 Integer where
  toIntegral = P.toInteger

instance ToIntegral Int Int where
  toIntegral = P.id

instance ToIntegral Natural Natural where
  toIntegral = P.id

instance ToIntegral Int8 Int8 where
  toIntegral = P.id

instance ToIntegral Int16 Int16 where
  toIntegral = P.id

instance ToIntegral Int32 Int32 where
  toIntegral = P.id

instance ToIntegral Int64 Int64 where
  toIntegral = P.id

instance ToIntegral Word Word where
  toIntegral = P.id

instance ToIntegral Word8 Word8 where
  toIntegral = P.id

instance ToIntegral Word16 Word16 where
  toIntegral = P.id

instance ToIntegral Word32 Word32 where
  toIntegral = P.id

instance ToIntegral Word64 Word64 where
  toIntegral = P.id

-- | fromIntegral abstracts the codomain type, compared with the preludes Integral type.
-- > fromIntegral_ a == a
--
-- fromIntegral is widely used as general coercion, hence the underscore for the operator.
class FromIntegral a b where
  fromIntegral_ :: b -> a
  default fromIntegral_ :: (a ~ b) => b -> a
  fromIntegral_ = P.id

-- | general coercion via Integer
fromIntegral :: (FromInteger b, ToInteger a) => a -> b
fromIntegral = fromInteger . toInteger

instance (FromIntegral a b) => FromIntegral (c -> a) b where
  fromIntegral_ i _ = fromIntegral_ i

instance FromIntegral Double Integer where
  fromIntegral_ = P.fromInteger

instance FromIntegral Float Integer where
  fromIntegral_ = P.fromInteger

instance FromIntegral Int Integer where
  fromIntegral_ = P.fromInteger

instance FromIntegral Integer Integer where
  fromIntegral_ = P.id

instance FromIntegral Natural Integer where
  fromIntegral_ = P.fromInteger

instance FromIntegral Int8 Integer where
  fromIntegral_ = P.fromInteger

instance FromIntegral Int16 Integer where
  fromIntegral_ = P.fromInteger

instance FromIntegral Int32 Integer where
  fromIntegral_ = P.fromInteger

instance FromIntegral Int64 Integer where
  fromIntegral_ = P.fromInteger

instance FromIntegral Word Integer where
  fromIntegral_ = P.fromInteger

instance FromIntegral Word8 Integer where
  fromIntegral_ = P.fromInteger

instance FromIntegral Word16 Integer where
  fromIntegral_ = P.fromInteger

instance FromIntegral Word32 Integer where
  fromIntegral_ = P.fromInteger

instance FromIntegral Word64 Integer where
  fromIntegral_ = P.fromInteger

instance FromIntegral Int Int where
  fromIntegral_ = P.id

instance FromIntegral Natural Natural where
  fromIntegral_ = P.id

instance FromIntegral Int8 Int8 where
  fromIntegral_ = P.id

instance FromIntegral Int16 Int16 where
  fromIntegral_ = P.id

instance FromIntegral Int32 Int32 where
  fromIntegral_ = P.id

instance FromIntegral Int64 Int64 where
  fromIntegral_ = P.id

instance FromIntegral Word Word where
  fromIntegral_ = P.id

instance FromIntegral Word8 Word8 where
  fromIntegral_ = P.id

instance FromIntegral Word16 Word16 where
  fromIntegral_ = P.id

instance FromIntegral Word32 Word32 where
  fromIntegral_ = P.id

instance FromIntegral Word64 Word64 where
  fromIntegral_ = P.id

-- | ghc defaulting rules and, it seems, -XExtendedDefaultRules do not permit multiple parameter typeclasses to be in the mix when types are resolved, hence the simpler `type FromInteger a = FromIntegral a Integer` does not suffice.
class FromInteger a where
  fromInteger :: Integer -> a
  default fromInteger :: (FromIntegral a Integer) => Integer -> a
  fromInteger = fromIntegral_

instance FromInteger Integer

instance FromInteger Int

instance FromInteger Double

instance FromInteger Float

instance FromInteger Natural

instance FromInteger Int8

instance FromInteger Int16

instance FromInteger Int32

instance FromInteger Int64

instance FromInteger Word

instance FromInteger Word8

instance FromInteger Word16

instance FromInteger Word32

instance FromInteger Word64

-- $operators

even :: (P.Eq a, Integral a) => a -> P.Bool
even n = n `rem` (one + one) P.== zero

odd :: (P.Eq a, Integral a) => a -> P.Bool
odd = P.not . even

-------------------------------------------------------

-- | raise a number to a non-negative integral power
(^) ::
  (P.Ord b, Multiplicative a, Integral b) =>
  a ->
  b ->
  a
x0 ^ y0
  | y0 P.< zero = P.undefined
  | -- P.errorWithoutStackTrace "Negative exponent"
    y0 P.== zero =
    one
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

(^^) ::
  (Divisive a, Subtractive b, Integral b, P.Ord b) => a -> b -> a
(^^) x n = if n P.>= zero then x ^ n else recip (x ^ negate n)
