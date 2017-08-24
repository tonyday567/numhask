{-# OPTIONS_GHC -Wall #-}

-- | Rings
-- A distinguishment is made between Rings and Commutative Rings.
module NumHask.Algebra.Ring (
    Semiring
  , Ring
  , CRing
  ) where

import Protolude (Double, Float, Int, Integer,Bool(..))
import NumHask.Algebra.Additive
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Distribution
import Data.Complex (Complex(..))

-- | a semiring
class ( MultiplicativeAssociative a
      , MultiplicativeUnital a
      , Distribution a) =>
      Semiring a

instance Semiring Double
instance Semiring Float
instance Semiring Int
instance Semiring Integer
instance Semiring Bool
instance (AdditiveGroup a, Semiring a) => Semiring (Complex a)

-- | Ring
class ( AdditiveGroup a
      , MultiplicativeAssociative a
      , MultiplicativeUnital a
      , Distribution a) =>
      Ring a

instance Ring Double
instance Ring Float
instance Ring Int
instance Ring Integer
instance (Ring a) => Ring (Complex a)

-- | CRing is a Ring with Multiplicative Commutation.  It arises often due to * being defined as only multiplicative commutative.
class ( Multiplicative a, Ring a) => CRing a

instance CRing Double
instance CRing Float
instance CRing Int
instance CRing Integer
instance (CRing a) => CRing (Complex a)
