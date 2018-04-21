{-# OPTIONS_GHC -Wall #-}

-- | 'Distribution' avoids a name clash with 'Data.Distributive'
module NumHask.Algebra.Distribution
  ( Distribution
  ) where

import Data.Complex (Complex(..))
import GHC.Natural (Natural(..))
import NumHask.Algebra.Additive
import NumHask.Algebra.Multiplicative
import Prelude (Bool(..), Double, Float, Int, Integer)

-- | Distribution (and annihilation) laws
--
-- > a * (b + c) == a * b + a * c
-- > (a + b) * c == a * c + b * c
-- > a * zero == zero
-- > zero * a == zero
class (Additive a, MultiplicativeMagma a) =>
      Distribution a

instance Distribution Double

instance Distribution Float

instance Distribution Int

instance Distribution Integer

instance Distribution Bool

instance (AdditiveGroup a, Distribution a) => Distribution (Complex a)

instance Distribution Natural
