{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | [Lattices](https://en.wikipedia.org/wiki/Lattice_(order\))
module NumHask.Algebra.Lattice
  ( JoinSemiLattice (..),
    joinLeq,
    MeetSemiLattice (..),
    meetLeq,
    BoundedJoinSemiLattice (..),
    BoundedMeetSemiLattice (..),
  )
where

import Data.Bool (Bool (..), (&&), (||))
import Data.Eq (Eq ((==)))
import Data.Function (const)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ord (Ord (..))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Enum (Bounded (..))
import GHC.Float (Double, Float)
import GHC.Int (Int)
import GHC.Natural (Natural (..))
import GHC.Num (Integer)
import GHC.Word (Word)
import NumHask.Algebra.Additive (zero)
import NumHask.Algebra.Field
  ( negInfinity,
    infinity,
  )

-- | A algebraic structure with element joins: See [Semilattice](http://en.wikipedia.org/wiki/Semilattice)
--
-- > Associativity: x \/ (y \/ z) == (x \/ y) \/ z
-- > Commutativity: x \/ y == y \/ x
-- > Idempotency:   x \/ x == x
class (Eq a) => JoinSemiLattice a where
  infixr 5 \/
  (\/) :: a -> a -> a

-- | The partial ordering induced by the join-semilattice structure
joinLeq :: (JoinSemiLattice a) => a -> a -> Bool
joinLeq x y = (x \/ y) == y

-- | A algebraic structure with element meets: See [Semilattice](http://en.wikipedia.org/wiki/Semilattice)
--
-- > Associativity: x /\ (y /\ z) == (x /\ y) /\ z
-- > Commutativity: x /\ y == y /\ x
-- > Idempotency:   x /\ x == x
class (Eq a) => MeetSemiLattice a where
  infixr 6 /\
  (/\) :: a -> a -> a

-- | The partial ordering induced by the meet-semilattice structure
meetLeq :: (MeetSemiLattice a) => a -> a -> Bool
meetLeq x y = (x /\ y) == x

-- | The combination of two semi lattices makes a lattice if the absorption law holds:
-- see [Absorption Law](http://en.wikipedia.org/wiki/Absorption_law) and [Lattice](http://en.wikipedia.org/wiki/Lattice_(order\))
--
-- > Absorption: a \/ (a /\ b) == a /\ (a \/ b) == a
class (JoinSemiLattice a, MeetSemiLattice a) => Lattice a

instance (JoinSemiLattice a, MeetSemiLattice a) => Lattice a

-- | A join-semilattice with an identity element 'bottom' for '\/'.
--
-- > Identity: x \/ bottom == x
class JoinSemiLattice a => BoundedJoinSemiLattice a where
  bottom :: a

-- | A meet-semilattice with an identity element 'top' for '/\'.
--
-- > Identity: x /\ top == x
class MeetSemiLattice a => BoundedMeetSemiLattice a where
  top :: a

-- | Lattices with both bounds
class (JoinSemiLattice a, MeetSemiLattice a, BoundedJoinSemiLattice a, BoundedMeetSemiLattice a) => BoundedLattice a

instance (JoinSemiLattice a, MeetSemiLattice a, BoundedJoinSemiLattice a, BoundedMeetSemiLattice a) => BoundedLattice a

instance JoinSemiLattice Float where
  (\/) = min

instance MeetSemiLattice Float where
  (/\) = max

instance JoinSemiLattice Double where
  (\/) = min

instance MeetSemiLattice Double where
  (/\) = max

instance JoinSemiLattice Int where
  (\/) = min

instance MeetSemiLattice Int where
  (/\) = max

instance JoinSemiLattice Integer where
  (\/) = min

instance MeetSemiLattice Integer where
  (/\) = max

instance JoinSemiLattice Bool where
  (\/) = (||)

instance MeetSemiLattice Bool where
  (/\) = (&&)

instance JoinSemiLattice Natural where
  (\/) = min

instance MeetSemiLattice Natural where
  (/\) = max

instance JoinSemiLattice Int8 where
  (\/) = min

instance MeetSemiLattice Int8 where
  (/\) = max

instance JoinSemiLattice Int16 where
  (\/) = min

instance MeetSemiLattice Int16 where
  (/\) = max

instance JoinSemiLattice Int32 where
  (\/) = min

instance MeetSemiLattice Int32 where
  (/\) = max

instance JoinSemiLattice Int64 where
  (\/) = min

instance MeetSemiLattice Int64 where
  (/\) = max

instance JoinSemiLattice Word where
  (\/) = min

instance MeetSemiLattice Word where
  (/\) = max

instance JoinSemiLattice Word8 where
  (\/) = min

instance MeetSemiLattice Word8 where
  (/\) = max

instance JoinSemiLattice Word16 where
  (\/) = min

instance MeetSemiLattice Word16 where
  (/\) = max

instance JoinSemiLattice Word32 where
  (\/) = min

instance MeetSemiLattice Word32 where
  (/\) = max

instance JoinSemiLattice Word64 where
  (\/) = min

instance MeetSemiLattice Word64 where
  (/\) = max

instance (Eq (a -> b), JoinSemiLattice b) => JoinSemiLattice (a -> b) where
  f \/ f' = \a -> f a \/ f' a

instance (Eq (a -> b), MeetSemiLattice b) => MeetSemiLattice (a -> b) where
  f /\ f' = \a -> f a /\ f' a

-- from here

instance BoundedJoinSemiLattice Float where
  bottom = negInfinity

instance BoundedMeetSemiLattice Float where
  top = infinity

instance BoundedJoinSemiLattice Double where
  bottom = negInfinity

instance BoundedMeetSemiLattice Double where
  top = infinity

instance BoundedJoinSemiLattice Int where
  bottom = minBound

instance BoundedMeetSemiLattice Int where
  top = maxBound

instance BoundedJoinSemiLattice Bool where
  bottom = False

instance BoundedMeetSemiLattice Bool where
  top = True

instance BoundedJoinSemiLattice Natural where
  bottom = zero

instance BoundedJoinSemiLattice Int8 where
  bottom = minBound

instance BoundedMeetSemiLattice Int8 where
  top = maxBound

instance BoundedJoinSemiLattice Int16 where
  bottom = minBound

instance BoundedMeetSemiLattice Int16 where
  top = maxBound

instance BoundedJoinSemiLattice Int32 where
  bottom = minBound

instance BoundedMeetSemiLattice Int32 where
  top = maxBound

instance BoundedJoinSemiLattice Int64 where
  bottom = minBound

instance BoundedMeetSemiLattice Int64 where
  top = maxBound

instance BoundedJoinSemiLattice Word where
  bottom = minBound

instance BoundedMeetSemiLattice Word where
  top = maxBound

instance BoundedJoinSemiLattice Word8 where
  bottom = minBound

instance BoundedMeetSemiLattice Word8 where
  top = maxBound

instance BoundedJoinSemiLattice Word16 where
  bottom = minBound

instance BoundedMeetSemiLattice Word16 where
  top = maxBound

instance BoundedJoinSemiLattice Word32 where
  bottom = minBound

instance BoundedMeetSemiLattice Word32 where
  top = maxBound

instance BoundedJoinSemiLattice Word64 where
  bottom = minBound

instance BoundedMeetSemiLattice Word64 where
  top = maxBound

instance (Eq (a -> b), BoundedJoinSemiLattice b) => BoundedJoinSemiLattice (a -> b) where
  bottom = const bottom

instance (Eq (a -> b), BoundedMeetSemiLattice b) => BoundedMeetSemiLattice (a -> b) where
  top = const top
