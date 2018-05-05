{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Integral classes
module NumHask.Algebra.Rational
  ( ToRatio(..)
  , FromRatio(..)
  , fromRational
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Natural (Natural(..))
import GHC.Real (Ratio, Rational)
import qualified Prelude as P
import Prelude (Double, Float, Int, Integer, (.))

-- | toRatio is equivalent to `Real` in base.
class ToRatio a where
  toRatio :: a -> Ratio Integer

-- | `Fractional` in base splits into fromRatio and MultiplicativeGroup
class FromRatio a where
  fromRatio :: Ratio Integer -> a

-- | coercion of 'Rational's
--
-- > fromRational a == a
fromRational :: (ToRatio a, FromRatio b) => a -> b
fromRational = fromRatio . toRatio

instance FromRatio Double where
  fromRatio = P.fromRational

instance FromRatio Float where
  fromRatio = P.fromRational

instance ToRatio Double where
  toRatio = P.toRational

instance ToRatio Float where
  toRatio = P.toRational

instance ToRatio Int where
  toRatio = P.toRational

instance ToRatio Integer where
  toRatio = P.toRational

instance ToRatio Natural where
  toRatio = P.toRational

instance ToRatio Rational where
  toRatio = P.toRational

instance ToRatio Int8 where
  toRatio = P.toRational

instance ToRatio Int16 where
  toRatio = P.toRational

instance ToRatio Int32 where
  toRatio = P.toRational

instance ToRatio Int64 where
  toRatio = P.toRational

instance ToRatio Word where
  toRatio = P.toRational

instance ToRatio Word8 where
  toRatio = P.toRational

instance ToRatio Word16 where
  toRatio = P.toRational

instance ToRatio Word32 where
  toRatio = P.toRational

instance ToRatio Word64 where
  toRatio = P.toRational


