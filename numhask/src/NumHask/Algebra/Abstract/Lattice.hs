{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE IncoherentInstances #-}

module NumHask.Algebra.Abstract.Lattice where

import GHC.Generics
import Data.Data
import Control.Monad

-- | A algebraic structure with element joins: <http://en.wikipedia.org/wiki/Semilattice>
--
-- > Associativity: x \/ (y \/ z) == (x \/ y) \/ z
-- > Commutativity: x \/ y == y \/ x
-- > Idempotency:   x \/ x == x
class JoinSemiLattice a where
    infixr 5 \/
    (\/) :: a -> a -> a

-- | The partial ordering induced by the join-semilattice structure
joinLeq :: (Eq a, JoinSemiLattice a) => a -> a -> Bool
joinLeq x y = (x \/ y) == y

-- | A algebraic structure with element meets: <http://en.wikipedia.org/wiki/Semilattice>
--
-- > Associativity: x /\ (y /\ z) == (x /\ y) /\ z
-- > Commutativity: x /\ y == y /\ x
-- > Idempotency:   x /\ x == x
class MeetSemiLattice a where
    infixr 6 /\
    (/\) :: a -> a -> a

-- | The partial ordering induced by the meet-semilattice structure
meetLeq :: (Eq a, MeetSemiLattice a) => a -> a -> Bool
meetLeq x y = (x /\ y) == x

-- | The combination of two semi lattices makes a lattice if the absorption law holds:
-- see <http://en.wikipedia.org/wiki/Absorption_law> and <http://en.wikipedia.org/wiki/Lattice_(order)>
--
-- > Absorption: a \/ (a /\ b) == a /\ (a \/ b) == a
class (JoinSemiLattice a, MeetSemiLattice a) => Lattice a
instance (JoinSemiLattice a, MeetSemiLattice a) => Lattice a

instance JoinSemiLattice b => JoinSemiLattice (a -> b) where
  (\/) f f' a = f a \/ f' a

instance MeetSemiLattice b => MeetSemiLattice (a -> b) where
  (/\) f f' a = f a /\ f' a

newtype Ordered a = Ordered { getOrdered :: a }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
           , Generic1
           )

instance Applicative Ordered where
  pure = return
  (<*>) = ap

instance Monad Ordered where
  return           = Ordered
  Ordered x >>= f  = f x

instance Ord a => JoinSemiLattice (Ordered a) where
  Ordered x \/ Ordered y = Ordered (max x y)

instance Ord a => MeetSemiLattice (Ordered a) where
  Ordered x /\ Ordered y = Ordered (min x y)

instance Ord a => Lattice (Ordered a)

instance JoinSemiLattice Float where
  (\/) = min

instance MeetSemiLattice Float where
  (/\) = max

{-
instance {-# OVERLAPPABLE #-} (Ord a) => JoinSemiLattice a where
  (\/) = min

instance {-# OVERLAPPABLE #-} (Ord a) => MeetSemiLattice a where
  (/\) = max


-}

