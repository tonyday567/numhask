{-# OPTIONS_GHC -Wall #-}

-- | Magma
module NumHask.Algebra.Magma
  ( Magma(..)
  , Unital(..)
  , Associative
  , Commutative
  , Invertible(..)
  , Idempotent
  , Monoidal
  , CMonoidal
  , Loop
  , Group
  , groupSwap
  , Abelian
  ) where

-- * Magma structure
-- | A <https://en.wikipedia.org/wiki/Magma_(algebra) Magma> is a tuple (T,⊕) consisting of
--
-- - a type a, and
--
-- - a function (⊕) :: T -> T -> T
--
-- The mathematical laws for a magma are:
--
-- - ⊕ is defined for all possible pairs of type T, and
--
-- - ⊕ is closed in the set of all possible values of type T
--
-- or, more tersly,
--
-- > ∀ a, b ∈ T: a ⊕ b ∈ T
--
-- These laws are true by construction in haskell: the type signature of 'magma' and the above mathematical laws are synonyms.
--
--
class Magma a where
  (⊕) :: a -> a -> a

-- | A Unital Magma
--
-- > unit ⊕ a = a
-- > a ⊕ unit = a
--
class Magma a =>
      Unital a where
  unit :: a

-- | An Associative Magma
--
-- > (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
class Magma a =>
      Associative a

-- | A Commutative Magma
--
-- > a ⊕ b = b ⊕ a
class Magma a =>
      Commutative a

-- | An Invertible Magma
--
-- > ∀ a ∈ T: inv a ∈ T
--
-- law is true by construction in Haskell
--
class Magma a =>
      Invertible a where
  inv :: a -> a

-- | An Idempotent Magma
--
-- > a ⊕ a = a
class Magma a =>
      Idempotent a

-- | A Monoidal Magma is associative and unital.
class (Associative a, Unital a) =>
      Monoidal a

-- | A CMonoidal Magma is commutative, associative and unital.
class (Commutative a, Associative a, Unital a) =>
      CMonoidal a

-- | A Loop is unital and invertible
class (Unital a, Invertible a) =>
      Loop a

-- | A Group is associative, unital and invertible
class (Associative a, Unital a, Invertible a) =>
      Group a

-- | see http://chris-taylor.github.io/blog/2013/02/25/xor-trick/
groupSwap :: (Group a) => (a, a) -> (a, a)
groupSwap (a, b) =
  let a' = a ⊕ b
      b' = a ⊕ inv b
      a'' = inv b' ⊕ a'
  in (a'', b')

-- | An Abelian Group is associative, unital, invertible and commutative
class (Associative a, Unital a, Invertible a, Commutative a) =>
      Abelian a
