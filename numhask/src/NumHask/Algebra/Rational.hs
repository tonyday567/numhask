{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Integral classes
module NumHask.Algebra.Rational
  ( ToRatio(..)
  , FromRatio(..)
  , fromRational
  ) where

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
