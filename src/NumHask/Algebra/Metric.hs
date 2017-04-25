{-# OPTIONS_GHC -Wall #-}

-- | Metric structure
module NumHask.Algebra.Metric (
    -- * Metric
    Metric(..)
  , Normed(..)
  , Signed(..)
  , Epsilon(..)
  , (≈)
  ) where

import qualified Protolude as P
import Protolude (Double, Float, Int, Integer, ($), Bool(..), Ord(..), Eq(..), (&&))
import NumHask.Algebra.Field
import NumHask.Algebra.Additive
import NumHask.Algebra.Multiplicative
import Data.Complex (Complex(..))

-- | abs and signnum are warts on the standard 'Num' class, and are separated here to provide a cleaner structure.
class ( AdditiveUnital a
      , AdditiveGroup a
      , MultiplicativeUnital a
      ) => Signed a where
    sign :: a -> a
    abs :: a -> a

instance Signed Double where
    sign a = if a >= zero then one else negate one
    abs = P.abs
instance Signed Float where
    sign a = if a >= zero then one else negate one
    abs = P.abs
instance Signed Int where
    sign a = if a >= zero then one else negate one
    abs = P.abs
instance Signed Integer where
    sign a = if a >= zero then one else negate one
    abs = P.abs

-- | Normed is a current wart on the NumHask api, causing all sorts of runaway constraint boiler-plate.
class Normed a b where
    size :: a -> b

instance Normed Double Double where size = P.abs
instance Normed Float Float where size = P.abs
instance Normed Int Int where size = P.abs
instance Normed Integer Integer where size = P.abs
instance {-# Overlapping #-} (Multiplicative a, ExpField a, Normed a a) => Normed (Complex a) a where
    size (rx :+ ix) = sqrt (rx * rx + ix * ix)

-- | This should probably be split off into some sort of alternative Equality logic, but to what end?
class (AdditiveGroup a) => Epsilon a where
    nearZero :: a -> Bool
    aboutEqual :: a -> a -> Bool

infixl 4 ≈

-- | utf ???
(≈) :: (Epsilon a) => a -> a -> Bool
(≈) = aboutEqual

instance Epsilon Double where
    nearZero a = abs a <= (1e-12 :: Double)
    aboutEqual a b = nearZero $ a - b

instance Epsilon Float where
    nearZero a = abs a <= (1e-6 :: Float)
    aboutEqual a b = nearZero $ a - b

instance Epsilon Int where
    nearZero a = a == zero
    aboutEqual a b = nearZero $ a - b

instance Epsilon Integer where
    nearZero a = a == zero
    aboutEqual a b = nearZero $ a - b

instance {-# Overlapping #-} (Epsilon a) => Epsilon (Complex a) where
    nearZero (rx :+ ix) = nearZero rx && nearZero ix
    aboutEqual a b = nearZero $ a - b

-- | distance between numbers
class Metric a b where
    distance :: a -> a -> b

instance Metric Double Double where distance a b = abs (a - b)
instance Metric Float Float where distance a b = abs (a - b)
instance Metric Int Int where distance a b = abs (a - b)
instance Metric Integer Integer where distance a b = abs (a - b)
instance {-# Overlapping #-} (Multiplicative a, ExpField a, Normed a a) => Metric (Complex a) a where
    distance a b = size (a - b)

