{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | The Group hirarchy
module NumHask.Algebra.Group
      ( Magma(..)
      , Unital(..)
      , one
      , zero
      , Semigroup(..)
      , Commutative(..)
      , Add(..)
      , Addition(..)
      , Invertible(..)
      , neg
      , (-)
      , recip
      , Idempotent(..)
      , Monoid(..)
      , Invertible(..)
      , Mult(..)
      , Multplicative(..)
      , Group(..)
      , groupSwap
      , AbelianGroup(..)
      )
where

import           Data.Coerce
import qualified Prelude                       as P

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

instance (P.Semigroup a) => (Magma a) where
  comb = (P.<>)

-- | A Unital Magma
--
-- > unit comb a = a
-- > a comb unit = a
--
class Magma a =>
      Unital a where
  unit :: a

one :: Unital (Mult a) => a
one = coerce (unit :: (Mult a))

zero :: Unital (Add a) => a
zero = coerce (unit :: (Add a))

-- | A semigroup is an associative Magma
--
-- > (a comb b) comb c = a comb (b comb c)
class Magma a =>
      Semigroup a where
      infixl 6 <>
      (<>) = comb

instance (P.Semigroup a) => (Semigroup a)

-- | A Commutative Magma
--
-- > a comb b = b comb a
class Magma a =>
      Commutative a

newtype Add a = Add a

class (Semigroup (Add a), Commutative (Add a)) => Addition a where
      infixl 6 +
      (+) = coerce comb

      sum :: (P.Foldable f, Unital (Add a)) => f a -> a
      sum = P.foldr (+) zero

      infixl 6 *
      (-) :: Invertible (Add a) => a -> a -> a
      (-) a b = a + neg b

instance (Semigroup (Add a), Commutative (Add a)) => Addition a

-- | A Monoid is a Semigroup with an identity element
--
class (Unital a, Semigroup a) => Monoid a where
      mempty = unit
instance (Unital a, Semigroup a) => Monoid a

instance (P.Monoid a) => (Monoid a) where
      unit = P.mempty

-- | An Invertible Magma
--
-- > ∀ a ∈ T: a `comb` inv a = unit
--
class Unital a =>
      Invertible a where
  inv :: a -> a

neg :: Invertible (Add a) => a -> a
neg = coerce inv

recip :: Invertible (Mult a) => a -> a
recip = coerce inv

-- | A group is a Monoid with an invertible
class (Monoid a, Invertible a) => Group a
instance (Monoid a, Invertible a) => Group a

-- | A magma with an absorbing Element
--
-- > a `times` absorb = absorb
class Magma a =>
    Absorbing a where
    absorb :: a

newtype Mult a = Mult a

class (Absorbing (Mult a), Commutative (Mult a)) =>
      Multplicative a where
    infixl 6 *
    (*) = coerce comb

    zero' :: a
    zero' = coerce absorb

    product :: (P.Foldable f, Unital (Mult a)) => f a -> a
    product = P.foldr (*) one

    infixl 6 *
    (/) :: Invertible (Mult a) => a -> a -> a
    (/) a b = a * recip b

instance (Absorbing (Mult a), Commutative (Mult a)) =>
    Multplicative a

-- | An Idempotent Magma
--
-- > a comb a = a
class Magma a =>
      Idempotent a

-- | see http://chris-taylor.github.io/blog/2013/02/25/xor-trick/
groupSwap :: (Group a) => (a, a) -> (a, a)
groupSwap (a, b) =
      let a'  = a comb b
          b'  = a comb (inv b)
          a'' = inv b' comb a'
      in  (a'', b')

-- | An Abelian Group is associative, unital, invertible and commutative
class (Group a, Addition a) =>
      AbelianGroup a
instance (Group a, Addition a) => AbelianGroup a
