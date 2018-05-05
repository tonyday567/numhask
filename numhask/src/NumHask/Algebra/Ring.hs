{-# OPTIONS_GHC -Wall #-}
{-# language FlexibleInstances #-}

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
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Natural (Natural(..))
import NumHask.Algebra.Additive
import NumHask.Algebra.Distribution
import NumHask.Algebra.Multiplicative
import Prelude (Bool(..), Double, Float, Int, Integer)

-- | Semiring
class (MultiplicativeAssociative a, MultiplicativeUnital a, Distribution a) =>
      Semiring a

instance Semiring Double

instance Semiring Float

instance Semiring Int

instance Semiring Integer

instance Semiring Bool

instance (AdditiveGroup a, Semiring a) => Semiring (Complex a)

instance Semiring Natural

instance Semiring Int8

instance Semiring Int16

instance Semiring Int32

instance Semiring Int64

instance Semiring Word

instance Semiring Word8

instance Semiring Word16

instance Semiring Word32

instance Semiring Word64

-- | Ring
-- 
-- A Ring consists of a set equipped with two binary operations that generalize the arithmetic operations of addition and multiplication; it is an abelian group with a second binary operation that is associative, is distributive over the abelian group operation, and has an identity element.
-- 
-- Summary of the laws inherited from the ring super-classes:
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
-- 
class ( Semiring a
      , AdditiveGroup a
      ) =>
      Ring a

instance Ring Double

instance Ring Float

instance Ring Int

instance Ring Integer

instance (Ring a) => Ring (Complex a)

instance Ring Int8

instance Ring Int16

instance Ring Int32

instance Ring Int64

instance Ring Word

instance Ring Word8

instance Ring Word16

instance Ring Word32

instance Ring Word64

-- | CRing is a Ring with Multiplicative Commutation.  It arises often due to '*' being defined as a multiplicative commutative operation.
class (Multiplicative a, Ring a) =>
      CRing a

instance CRing Double

instance CRing Float

instance CRing Int

instance CRing Integer

instance (CRing a) => CRing (Complex a)

instance CRing Int8

instance CRing Int16

instance CRing Int32

instance CRing Int64

instance CRing Word

instance CRing Word8

instance CRing Word16

instance CRing Word32

instance CRing Word64

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
-- > adj one ==> one
-- > adj (adj a) ==> a
--
-- Note: elements for which @adj a == a@ are called "self-adjoint".
--
class Semiring a => InvolutiveRing a where
  adj :: a -> a
  adj x = x

instance InvolutiveRing Double

instance InvolutiveRing Float

instance InvolutiveRing Integer

instance InvolutiveRing Int

instance (Ring a) => InvolutiveRing (Complex a) where
  adj (a :+ b) = a :+ negate b

instance InvolutiveRing Natural

instance InvolutiveRing Int8

instance InvolutiveRing Int16

instance InvolutiveRing Int32

instance InvolutiveRing Int64

instance InvolutiveRing Word

instance InvolutiveRing Word8

instance InvolutiveRing Word16

instance InvolutiveRing Word32

instance InvolutiveRing Word64

