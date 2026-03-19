-- | Integral classes
module NumHask.Data.Integral
  ( Integral (..),
    ToIntegral (..),
    ToInt,
    FromIntegral (..),
    FromInt,
    FromInteger (..),
    even,
    odd,
    (^^),
    (^),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ord
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Natural (Natural (..), naturalFromInteger)
import NumHask.Algebra.Additive
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import Prelude (Double, Float, Int, Integer, fst, snd, (.))
import Prelude qualified as P

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude

-- | An Integral is anything that satisfies the law:
--
-- prop> \a b -> b == zero || b * (a `div` b) + (a `mod` b) == a
--
-- >>> 3 `divMod` 2
-- (1,1)
--
-- >>> (-3) `divMod` 2
-- (-2,1)
--
-- >>> (-3) `quotRem` 2
-- (-1,-1)
class
  (Distributive a) =>
  Integral a
  where
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

instance (Integral b) => Integral (a -> b) where
  div f f' a = f a `div` f' a
  mod f f' a = f a `mod` f' a
  divMod f f' = (\a -> fst (f a `divMod` f' a), \a -> snd (f a `divMod` f' a))
  quot f f' a = f a `mod` f' a
  rem f f' a = f a `mod` f' a
  quotRem f f' = (\a -> fst (f a `quotRem` f' a), \a -> snd (f a `quotRem` f' a))

-- |
-- >>> even 2
-- True
even :: (P.Eq a, Integral a) => a -> P.Bool
even n = n `rem` (one + one) P.== zero

-- |
-- >>> odd 3
-- True
odd :: (P.Eq a, Integral a) => a -> P.Bool
odd = P.not . even

-- | toIntegral is kept separate from Integral to help with compatability issues.
--
-- > toIntegral a == a
class ToIntegral a b where
  {-# MINIMAL toIntegral #-}

  toIntegral :: a -> b

-- | Convert to an 'Int'
type ToInt a = ToIntegral a Int

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

instance ToIntegral Integer Int where
  toIntegral = P.fromIntegral

instance ToIntegral Natural Int where
  toIntegral = P.fromIntegral

instance ToIntegral Int8 Int where
  toIntegral = P.fromIntegral

instance ToIntegral Int16 Int where
  toIntegral = P.fromIntegral

instance ToIntegral Int32 Int where
  toIntegral = P.fromIntegral

instance ToIntegral Int64 Int where
  toIntegral = P.fromIntegral

instance ToIntegral Word Int where
  toIntegral = P.fromIntegral

instance ToIntegral Word8 Int where
  toIntegral = P.fromIntegral

instance ToIntegral Word16 Int where
  toIntegral = P.fromIntegral

instance ToIntegral Word32 Int where
  toIntegral = P.fromIntegral

instance ToIntegral Word64 Int where
  toIntegral = P.fromIntegral

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

-- | Polymorphic version of fromInteger
--
-- > fromIntegral a == a
class FromIntegral a b where
  {-# MINIMAL fromIntegral #-}

  fromIntegral :: b -> a

-- | Convert from an 'Int'
type FromInt a = FromIntegral a Int

instance (FromIntegral a b) => FromIntegral (c -> a) b where
  fromIntegral i _ = fromIntegral i

instance FromIntegral Double Integer where
  fromIntegral = P.fromInteger

instance FromIntegral Float Integer where
  fromIntegral = P.fromInteger

instance FromIntegral Int Integer where
  fromIntegral = P.fromInteger

instance FromIntegral Integer Integer where
  fromIntegral = P.id

instance FromIntegral Natural Integer where
  fromIntegral = naturalFromInteger

instance FromIntegral Int8 Integer where
  fromIntegral = P.fromInteger

instance FromIntegral Int16 Integer where
  fromIntegral = P.fromInteger

instance FromIntegral Int32 Integer where
  fromIntegral = P.fromInteger

instance FromIntegral Int64 Integer where
  fromIntegral = P.fromInteger

instance FromIntegral Word Integer where
  fromIntegral = P.fromInteger

instance FromIntegral Word8 Integer where
  fromIntegral = P.fromInteger

instance FromIntegral Word16 Integer where
  fromIntegral = P.fromInteger

instance FromIntegral Word32 Integer where
  fromIntegral = P.fromInteger

instance FromIntegral Word64 Integer where
  fromIntegral = P.fromInteger

instance FromIntegral Double Int where
  fromIntegral = P.fromIntegral

instance FromIntegral Float Int where
  fromIntegral = P.fromIntegral

instance FromIntegral Int Int where
  fromIntegral = P.id

instance FromIntegral Integer Int where
  fromIntegral = P.fromIntegral

instance FromIntegral Natural Int where
  fromIntegral = P.fromIntegral

instance FromIntegral Int8 Int where
  fromIntegral = P.fromIntegral

instance FromIntegral Int16 Int where
  fromIntegral = P.fromIntegral

instance FromIntegral Int32 Int where
  fromIntegral = P.fromIntegral

instance FromIntegral Int64 Int where
  fromIntegral = P.fromIntegral

instance FromIntegral Word Int where
  fromIntegral = P.fromIntegral

instance FromIntegral Word8 Int where
  fromIntegral = P.fromIntegral

instance FromIntegral Word16 Int where
  fromIntegral = P.fromIntegral

instance FromIntegral Word32 Int where
  fromIntegral = P.fromIntegral

instance FromIntegral Word64 Int where
  fromIntegral = P.fromIntegral

instance FromIntegral Natural Natural where
  fromIntegral = P.id

instance FromIntegral Int8 Int8 where
  fromIntegral = P.id

instance FromIntegral Int16 Int16 where
  fromIntegral = P.id

instance FromIntegral Int32 Int32 where
  fromIntegral = P.id

instance FromIntegral Int64 Int64 where
  fromIntegral = P.id

instance FromIntegral Word Word where
  fromIntegral = P.id

instance FromIntegral Word8 Word8 where
  fromIntegral = P.id

instance FromIntegral Word16 Word16 where
  fromIntegral = P.id

instance FromIntegral Word32 Word32 where
  fromIntegral = P.id

instance FromIntegral Word64 Word64 where
  fromIntegral = P.id

-- | 'fromInteger' is special in two ways:
--
-- - numeric integral literals (like "42") are interpreted specifically as "fromInteger (42 :: GHC.Num.Integer)". The prelude version is used as default (or whatever fromInteger is in scope if RebindableSyntax is set).
--
-- - The default rules in < https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-750004.3 haskell2010> specify that constraints on 'fromInteger' need to be in a form @C v@, where v is a Num or a subclass of Num.
--
-- So a type synonym such as @type FromInteger a = FromIntegral a Integer@ doesn't work well with type defaulting; hence the need for a separate class.
class FromInteger a where
  fromInteger :: Integer -> a

instance FromInteger Double where
  fromInteger = P.fromInteger

instance FromInteger Float where
  fromInteger = P.fromInteger

instance FromInteger Int where
  fromInteger = P.fromInteger

instance FromInteger Integer where
  fromInteger = P.id

instance FromInteger Natural where
  fromInteger = naturalFromInteger

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

deriving instance FromInteger a => FromInteger (Sum a)

deriving instance FromInteger a => FromInteger (Product a)

infixr 8 ^^

-- | raise a number to an 'Integral' power
--
-- >>> 2 ^^ 3
-- 8.0
--
-- >>> 2 ^^ (-2)
-- 0.25
(^^) ::
  (P.Ord b, Divisive a, Subtractive b, Integral b) =>
  a ->
  b ->
  a
x0 ^^ y0 =
  case compare y0 zero of
    EQ -> one
    GT -> f x0 y0
    LT -> recip (x0 ^^ negate y0)
  where
    f x y
      | even y = f (x * x) (y `quot` two)
      | y P.== one = x
      | P.otherwise = g (x * x) (y `quot` two) x
    g x y z
      | even y = g (x * x) (y `quot` two) z
      | y P.== one = x * z
      | P.otherwise = g (x * x) (y `quot` two) (x * z)

infixr 8 ^

-- | raise a number to an 'Int' power
--
-- Note: This differs from (^) found in prelude which is a partial function (it errors on negative integrals). This is a monomorphic version of '(^^)' provided to help reduce ambiguous type noise in common usages.
--
-- >>> 2 ^ 3
-- 8.0
--
-- >>> 2 ^ (-2)
-- 0.25
(^) ::
  (Divisive a) => a -> Int -> a
(^) x n = x ^^ n
