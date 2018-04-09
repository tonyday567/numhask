{-# OPTIONS_GHC -Wall #-}
{-# language FlexibleInstances, UndecidableInstances #-}

-- | Ring classes. A distinguishment is made between Rings and Commutative Rings.
module NumHask.Algebra.Ring
  ( Semiring
  , Ring
  , CRing
  , StarSemiring(..)
  , KleeneAlgebra
  , InvolutiveRing(..)
  ) where

import Data.Complex (Complex(..))
import NumHask.Algebra.Additive
import NumHask.Algebra.Distribution
import NumHask.Algebra.Multiplicative
import Prelude (Bool(..), Double, Float, Int, Integer, Real(..))

-- | Semiring
class (MultiplicativeAssociative a, MultiplicativeUnital a, Distribution a) =>
      Semiring a

instance Semiring Double

instance Semiring Float

instance Semiring Int

instance Semiring Integer

instance Semiring Bool

instance (AdditiveGroup a, Semiring a) => Semiring (Complex a)

-- | Ring
-- a summary of the laws inherited from the ring super-classes
--
-- > zero + a == a
-- > a + zero == a
-- > (a + b) + c == a + (b + c)
-- > a + b == b + a
-- > a - a = zero
-- > negate a = zero - a
-- > negate a + a = zero
-- > a + negate a = zero
-- > one `times` a == a
-- > a `times` one == a
-- > (a `times` b) `times` c == a `times` (b `times` c)
-- > a `times` (b + c) == a `times` b + a `times` c
-- > (a + b) `times` c == a `times` c + b `times` c
-- > a `times` zero == zero
-- > zero `times` a == zero
class ( Semiring a
      , AdditiveGroup a
      ) =>
      Ring a

instance Ring Double

instance Ring Float

instance Ring Int

instance Ring Integer

instance (Ring a) => Ring (Complex a)

-- | CRing is a Ring with Multiplicative Commutation.  It arises often due to '*' being defined as a multiplicative commutative operation.
class (Multiplicative a, Ring a) =>
      CRing a

instance CRing Double

instance CRing Float

instance CRing Int

instance CRing Integer

instance (CRing a) => CRing (Complex a)

-- | StarSemiring
--
-- > star a = one + a `times` star a
--
class (Semiring a) => StarSemiring a where
    star :: a -> a
    star a = one + plus' a

    plus' :: a -> a
    plus' a = a `times` star a

-- | KleeneAlgebra
--
-- > a `times` x + x = a ==> star a `times` x + x = x
-- > x `times` a + x = a ==> x `times` star a + x = x
--
class (StarSemiring a, AdditiveIdempotent a) => KleeneAlgebra a



-- | Involutive Ring
--
-- > adj (a + b) ==> adj a + adj b
-- > adj (a * b) ==> adj a * adj b
-- > adj one = one
-- > adj (adj a) = a
--
-- Note: elements for which @adj a == a@ are called "self-adjoint".
-- 
class Ring a => InvolutiveRing a where
  adj :: a -> a
  adj x = x


instance InvolutiveRing Double

instance InvolutiveRing Float

instance InvolutiveRing Integer

instance InvolutiveRing Int

instance InvolutiveRing (Complex Double) where
  adj (a :+ b) = a :+ negate b

instance InvolutiveRing (Complex Float) where
  adj (a :+ b) = a :+ negate b

instance InvolutiveRing (Complex Int) where
  adj (a :+ b) = a :+ negate b

instance InvolutiveRing (Complex Integer) where
  adj (a :+ b) = a :+ negate b

  
