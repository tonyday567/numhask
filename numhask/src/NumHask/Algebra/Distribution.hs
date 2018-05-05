{-# OPTIONS_GHC -Wall #-}

-- | 'Distribution' avoids a name clash with 'Data.Distributive'
module NumHask.Algebra.Distribution
  ( Distribution
  ) where

import Data.Complex (Complex(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
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

instance Distribution Int8

instance Distribution Int16

instance Distribution Int32

instance Distribution Int64

instance Distribution Word

instance Distribution Word8

instance Distribution Word16

instance Distribution Word32

instance Distribution Word64

