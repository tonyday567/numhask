{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | The Group hirarchy
module NumHask.Algebra.Group
  ( Magma(..)
  , Unital(..)
  , Semigroup
  , Commutative
  , Addition
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
-- | A <https://en.wikipedia.org/wiki/Magma_(algebra) Magma> is a tuple (T,comb) consisting of
--
-- - a type a, and
--
-- - a function (comb) :: T -> T -> T
--
-- The mathematical laws for a magma are:
--
-- - comb is defined for all possible pairs of type T, and
--
-- - comb is closed in the set of all possible values of type T
--
-- or, more tersly,
--
-- > ∀ a, b ∈ T: a comb b ∈ T
--
-- These laws are true by construction in haskell: the type signature of 'magma' and the above mathematical laws are synonyms.
--
--
class Magma a where
  comb :: a -> a -> a

-- | A Unital Magma
--
-- > unit comb a = a
-- > a comb unit = a
--
class Magma a =>
      Unital a where
  unit :: a

-- | A semigroup is an associative Magma
--
-- > (a comb b) comb c = a comb (b comb c)
class Magma a =>
      Semigroup a

-- | A Commutative Magma
--
-- > a comb b = b comb a
class Magma a =>
      Commutative a

class (Semigroup a, Commutative a) => Addition a where
      infixl 6 +
      (+) = comb
instance (Semigroup a, Commutative a) => Addition a

-- | A Monoid is a Semigroup with an identity element
--
class (Unital a, Semigroup a) => Monoid a
instance (Unital a, Semigroup a) => Monoid a

-- | An Invertible Magma
--
-- > ∀ a ∈ T: inv a ∈ T
--
-- law is true by construction in Haskell
--
class Magma a =>
      Invertible a where
  inv :: a -> a

-- | A group is a Monoid with an invertible
class (Monoid a, Invertible a) => Group a
instance (Monoid a, Invertible a) => Group a

-- | An Idempotent Magma
--
-- > a comb a = a
class Magma a =>
      Idempotent a

-- | see http://chris-taylor.github.io/blog/2013/02/25/xor-trick/
groupSwap :: (Group a) => (a, a) -> (a, a)
groupSwap (a, b) =
  let a' = a comb b
      b' = a comb (inv b)
      a'' = inv b' comb a'
  in (a'', b')

-- | An Abelian Group is associative, unital, invertible and commutative
class (Group a, Addition a) =>
      AbelianGroup a
instance (Group a, Addition a) => AbelianGroup a
