{-# LANGUAGE CPP #-}

-- | [Lattices](https://en.wikipedia.org/wiki/Lattice_(order\))
module NumHask.Algebra.Lattice
  ( JoinSemiLattice (..),
    joinLeq,
    (<\),
    MeetSemiLattice (..),
    meetLeq,
    (</),
    LowerBounded (..),
    UpperBounded (..),
    Lattice,
    BoundedLattice,
  )
where

import Data.Bool (Bool (..), (&&), (||))
import Data.Eq (Eq ((==)))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ord (Ord (..))
import Data.Word (Word16, Word32, Word64, Word8)
#if defined(__GLASGOW_HASKELL__)
import GHC.Enum (Bounded (..))
import GHC.Float (Double, Float)
import GHC.Int (Int)
import GHC.Natural (Natural (..))
import GHC.Num (Integer)
import GHC.Word (Word)
#endif
#if defined(__MHS__)
import Data.Bounded (Bounded (..))
import Data.Double (Double)
import Data.Float (Float)
import Data.Int.Int (Int)
import Numeric.Natural (Natural (..))
import Data.Integer (Integer)
import Data.Word.Word (Word)
#endif
import NumHask.Algebra.Additive (zero)
import NumHask.Algebra.Field
  ( infinity,
    negInfinity,
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

infixr 6 <\ -- comment to stop CPP picking up the line-ending backslash

-- | The partial ordering induced by the join-semilattice structure
(<\) :: (JoinSemiLattice a) => a -> a -> Bool
(<\) = joinLeq

-- | A algebraic structure with element meets: See [Semilattice](http://en.wikipedia.org/wiki/Semilattice)
--
-- > Associativity: x /\ (y /\ z) == (x /\ y) /\ z
-- > Commutativity: x /\ y == y /\ x
-- > Idempotency:   x /\ x == x
class (Eq a) => MeetSemiLattice a where
  infixr 6 /\ -- comment to stop CPP picking up the line-ending backslash
  (/\) :: a -> a -> a

-- | The partial ordering induced by the meet-semilattice structure
meetLeq :: (MeetSemiLattice a) => a -> a -> Bool
meetLeq x y = (x /\ y) == x

infixr 6 </ -- comment to stop CPP picking up the line-ending backslash

-- | The partial ordering induced by the meet-semilattice structure
(</) :: (MeetSemiLattice a) => a -> a -> Bool
(</) = meetLeq

-- | The combination of two semi lattices makes a lattice if the absorption law holds:
-- see [Absorption Law](http://en.wikipedia.org/wiki/Absorption_law) and [Lattice](http://en.wikipedia.org/wiki/Lattice_(order\))
--
-- > Absorption: a \/ (a /\ b) == a /\ (a \/ b) == a
type Lattice a = (JoinSemiLattice a, MeetSemiLattice a)

-- | A join-semilattice with an identity element 'bottom' for '\/'.
--
-- > x \/ bottom == bottom
class (JoinSemiLattice a) => LowerBounded a where
  bottom :: a

-- | A meet-semilattice with an identity element 'top' for '/\'.
--
-- > x /\ top == top
class (MeetSemiLattice a) => UpperBounded a where
  top :: a

-- | Lattices with both bounds
--
-- > x /\ bottom == x
-- > x \/ top = x
type BoundedLattice a = (JoinSemiLattice a, MeetSemiLattice a, LowerBounded a, UpperBounded a)

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

instance LowerBounded Float where
  bottom = negInfinity

instance UpperBounded Float where
  top = infinity

instance LowerBounded Double where
  bottom = negInfinity

instance UpperBounded Double where
  top = infinity

instance LowerBounded Int where
  bottom = minBound

instance UpperBounded Int where
  top = maxBound

instance LowerBounded Bool where
  bottom = False

instance UpperBounded Bool where
  top = True

instance LowerBounded Natural where
  bottom = zero

instance LowerBounded Int8 where
  bottom = minBound

instance UpperBounded Int8 where
  top = maxBound

instance LowerBounded Int16 where
  bottom = minBound

instance UpperBounded Int16 where
  top = maxBound

instance LowerBounded Int32 where
  bottom = minBound

instance UpperBounded Int32 where
  top = maxBound

instance LowerBounded Int64 where
  bottom = minBound

instance UpperBounded Int64 where
  top = maxBound

instance LowerBounded Word where
  bottom = minBound

instance UpperBounded Word where
  top = maxBound

instance LowerBounded Word8 where
  bottom = minBound

instance UpperBounded Word8 where
  top = maxBound

instance LowerBounded Word16 where
  bottom = minBound

instance UpperBounded Word16 where
  top = maxBound

instance LowerBounded Word32 where
  bottom = minBound

instance UpperBounded Word32 where
  top = maxBound

instance LowerBounded Word64 where
  bottom = minBound

instance UpperBounded Word64 where
  top = maxBound
