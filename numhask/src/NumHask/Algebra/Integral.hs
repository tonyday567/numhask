{-# OPTIONS_GHC -Wall #-}

-- | Integral classes
module NumHask.Algebra.Integral
  ( Integral(..)
  , ToInteger(..)
  , FromInteger(..)
  , fromIntegral
  ) where

import GHC.Natural (Natural(..))
import NumHask.Algebra.Ring
import qualified Prelude as P
import Prelude (Double, Float, Int, Integer, (.), fst, snd)

-- | Integral laws
--
-- > b == zero || b * (a `div` b) + (a `mod` b) == a
class (Semiring a) =>
      Integral a where
  infixl 7 `div`, `mod`
  div :: a -> a -> a
  div a1 a2 = fst (divMod a1 a2)
  mod :: a -> a -> a
  mod a1 a2 = snd (divMod a1 a2)
  divMod :: a -> a -> (a, a)

instance Integral Int where
  divMod = P.divMod

instance Integral Integer where
  divMod = P.divMod

instance Integral Natural where
  divMod = P.divMod

-- | toInteger is kept separate from Integral to help with compatability issues.
class ToInteger a where
  toInteger :: a -> Integer

-- | fromInteger is the most problematic of the 'Num' class operators.  Particularly heinous, it is assumed that any number type can be constructed from an Integer, so that the broad classes of objects that are composed of multiple elements is avoided in haskell.
class FromInteger a where
  fromInteger :: Integer -> a

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

instance ToInteger Int where
  toInteger = P.toInteger

instance ToInteger Integer where
  toInteger = P.toInteger

instance ToInteger Natural where
  toInteger = P.toInteger
