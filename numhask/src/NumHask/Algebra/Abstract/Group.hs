{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | The Group hierarchy
module NumHask.Algebra.Abstract.Group
  ( Magma(..)
  , Unital(..)
  , Associative
  , Commutative
  , Absorbing(..)
  , Invertible(..)
  , Idempotent
  , Group
  , AbelianGroup
  )
where

-- * Magma structure
-- | A <https://en.wikipedia.org/wiki/Magma_(algebra) Magma> is a tuple (T,magma) consisting of
--
-- - a type a, and
--
-- - a function (magma) :: T -> T -> T
--
-- The mathematical laws for a magma are:
--
-- - magma is defined for all possible pairs of type T, and
--
-- - magma is closed in the set of all possible values of type T
--
-- or, more tersly,
--
-- > ∀ a, b ∈ T: a magma b ∈ T
--
-- These laws are true by construction in haskell: the type signature of 'magma' and the above mathematical laws are synonyms.
--
--
class Magma a where
  magma :: a -> a -> a

-- | A Unital Magma
--
-- > unit magma a = a
-- > a magma unit = a
--
class Magma a =>
  Unital a where
  unit :: a

-- | An Associative Magma
--
-- > (a magma b) magma c = a magma (b magma c)
class Magma a =>
  Associative a

-- | A Commutative Magma
--
-- > a magma b = b magma a
class Magma a =>
  Commutative a

-- | An Invertible Magma
--
-- > ∀ a,b ∈ T: inv a `magma` (a `magma` b) = b = (b `magma` a) `magma` inv a
--
class Magma a =>
  Invertible a where
  inv :: a -> a

-- | A group is Associative, Unital and Invertible
class (Associative a, Unital a, Invertible a) => Group a
instance (Associative a, Unital a, Invertible a) => Group a

-- | A magma with an absorbing Element
--
-- > a `times` absorb = absorb
class Magma a =>
  Absorbing a where
  absorb :: a

-- | An Idempotent Magma
--
-- > a magma a = a
class Magma a =>
  Idempotent a

-- | An Abelian Group is associative, unital, invertible and commutative
class (Group a, Commutative a) => AbelianGroup a
instance (Group a, Commutative a) => AbelianGroup a
