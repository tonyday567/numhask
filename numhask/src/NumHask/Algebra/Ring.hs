{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | The Ring hirarchy
module NumHask.Algebra.Ring
    (
        MultiplicativeMagma
    ,   MComb(..)
    ,   MultiplicativeUnital
    ,   MultiplicativeSemigroup
    ,   MultiplicativeCommutative
    ,   MultiplicativeInvertible
    ,   MultiplicativeIdempotent
    ,   Absorbing
    ,   Distribution
    ,   Semiring
    ,   Ring
    ,   CommutativeRing
    ,   IntegralDomain
    )
    where

import Data.Coerce
import NumHask.Algebra.Group

-- | 'times' is used as the operator for the multiplicative magam to distinguish from '*' which, by convention, implies commutativity
--
-- > ∀ a,b ∈ A: a `times` b ∈ A
--
-- law is true by construction in Haskell
class MultiplicativeMagma a where
    times :: a -> a -> a

newtype MagmaCompat a = MComb a

instance (MultiplicativeMagma a) => Magma (MagmaCompat a) where
    comb = coerce times

-- | A Unital Magma
--
-- > unit comb a = a
-- > a comb unit = a
--
class MultiplicativeMagma a =>
    MultiplicativeUnital a where
    one :: a

instance (MultiplicativeUnital a) => Unital (MagmaCompat a) where
    unit = coerce one
  
-- | A semigroup is an associative Magma
--
-- > (a comb b) comb c = a comb (b comb c)
class MultiplicativeMagma a =>
    MultiplicativeSemigroup a where
    (*) = times

instance (MultiplicativeSemigroup a) => Unital (MagmaCompat a)

-- | A Monoid is a Semigroup with an identity element
--
class (MultiplicativeUnital a, MultiplicativeSemigroup a) => MultiplicativeMonoid a
instance (MultiplicativeUnital a, MultiplicativeSemigroup a) => MultiplicativeMonoid a

-- | A Commutative Magma
--
-- > a comb b = b comb a
class MultiplicativeMagma a =>
    MultiplicativeCommutative a

instance (MultiplicativeCommutative a) => Commutative (MagmaCompat a)

-- | An Invertible Magma
--
-- > ∀ a ∈ T: inv a ∈ T
--
-- law is true by construction in Haskell
--
class MultiplicativeMagma a =>
    MultiplicativeInvertible a where
    recip :: a -> a

instance (MultiplicativeMagma a) => Unital (MagmaCompat a) where
    unit = coerce one

-- | An Idempotent Magma
--
-- > a comb a = a
class MultiplicativeMagma a =>
    MultiplicativeIdempotent a

instance (MultiplicativeIdempotent a) => Idempotent (MagmaCompat a)

-- | A magma with an absorbing Element
--
-- > a `times` zero = zero
class MultiplicativeMagma a =>
    Absorbing a where
    zero :: a

-- | Distribution laws
--
-- > a `times` (b + c) == a `times` b + a `times` c
-- > (a `times` b) * c == a `times` c + b `times` c
class (Additive a, MultiplicativeMagma a) =>
    Distribution a

-- | Semiring
class (Monoid a, MultiplicativeMonoid a, Distribution a, Absorbing a) =>
    Semiring a
instance (Monoid a, MultiplicativeMonoid a, Distribution a, Absorbing a) =>
    Semiring a

-- | Ring
class (Semiring a, AbelianGroup a) =>
    Ring a
instance (Semiring a, AbelianGroup a) =>
    Ring a

-- | Ring with a commutative Multiplication
class (Ring a, MultiplicativeCommutative a) =>
    CommutativeRing a
instance (Ring a, MultiplicativeCommutative a) =>
    CommutativeRing a

-- | generalization of ring of integers
--  product of any two nonzero elements is nonzero, also
--  if a ≠ 0, an equality ab = ac implies b = c.
class (CommutativeRing a) =>
    IntegralDomain a