{-# OPTIONS_GHC -Wall #-}

-- | The Group hierarchy
module NumHask.Algebra.Group
  ( Magma (..),
    Unital (..),
    Associative,
    Commutative,
    Absorbing (..),
    Invertible (..),
    Idempotent,
    Group,
    AbelianGroup,
  )
where

import Prelude

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
-- > ∀ a, b ∈ T: a ⊕ b ∈ T
--
-- These laws are true by construction in haskell: the type signature of '⊕' and the above mathematical laws are synonyms.
class Magma a where
  infix 3 ⊕
  (⊕) :: a -> a -> a

instance (Magma b) => Magma (a -> b) where
  f ⊕ g = \a -> f a ⊕ g a

-- | A Unital Magma is a magma with an
--   <https://en.wikipedia.org/wiki/Identity_element identity element> (the
--   unit).
--
-- > unit ⊕ a = a
-- > a ⊕ unit = a
class
  (Magma a) =>
  Unital a
  where
  unit :: a

instance (Unital b) => Unital (a -> b) where
  {-# INLINE unit #-}
  unit _ = unit

-- | An Associative Magma
--
-- > (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
class
  (Magma a) =>
  Associative a

instance (Associative b) => Associative (a -> b)

-- | A Commutative Magma is a Magma where the binary operation is
-- <https://en.wikipedia.org/wiki/Commutative_property commutative>.
--
-- > a ⊕ b = b ⊕ a
class
  (Magma a) =>
  Commutative a

instance (Commutative b) => Commutative (a -> b)

-- | An Invertible Magma
--
-- > ∀ a,b ∈ T: inv a ⊕ (a ⊕ b) = b = (b ⊕ a) ⊕ inv a
class
  (Magma a) =>
  Invertible a
  where
  inv :: a -> a

instance (Invertible b) => Invertible (a -> b) where
  {-# INLINE inv #-}
  inv f = inv . f

-- | A <https://en.wikipedia.org/wiki/Group_(mathematics) Group> is a
--   Associative, Unital and Invertible Magma.
type Group a = (Associative a, Unital a, Invertible a)

-- | An Absorbing is a Magma with an
--   <https://en.wikipedia.org/wiki/Absorbing_element Absorbing Element>
--
-- > a ⊕ absorb = absorb
class
  (Magma a) =>
  Absorbing a
  where
  absorb :: a

instance (Absorbing b) => Absorbing (a -> b) where
  {-# INLINE absorb #-}
  absorb _ = absorb

-- | An Idempotent Magma is a magma where every element is
--   <https://en.wikipedia.org/wiki/Idempotence Idempotent>.
--
-- > a ⊕ a = a
class
  (Magma a) =>
  Idempotent a

instance (Idempotent b) => Idempotent (a -> b)

-- | An <https://en.wikipedia.org/wiki/Abelian_group Abelian Group> is an
--   Associative, Unital, Invertible and Commutative Magma . In other words, it
--   is a Commutative Group
type AbelianGroup a = (Associative a, Unital a, Invertible a, Commutative a)
