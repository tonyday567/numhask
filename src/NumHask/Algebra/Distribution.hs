{-# OPTIONS_GHC -Wall #-}

-- | Distribution, avoiding name clashes with 'Data.Distributive'
module NumHask.Algebra.Distribution (
    -- * Distribution
    Distribution
  ) where

import Protolude (Double, Float, Int, Integer,Bool(..))
import NumHask.Algebra.Additive
import NumHask.Algebra.Multiplicative
import Data.Complex (Complex(..))

-- | Distribution
--
-- > a * (b + c) == a * b + a * c
--
-- > (a + b) * c == a * c + b * c
--
class (
    Additive a
  , MultiplicativeMagma a
  ) => Distribution a

instance Distribution Double
instance Distribution Float
instance Distribution Int
instance Distribution Integer
instance Distribution Bool
instance {-# Overlapping #-} (AdditiveGroup a, Distribution a) =>
    Distribution (Complex a)


