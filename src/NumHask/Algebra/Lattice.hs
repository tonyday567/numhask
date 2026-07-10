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
import GHC.Enum (Bounded (..))
import GHC.Float (Double, Float)
import GHC.Int (Int)
import GHC.Natural (Natural (..))
import GHC.Num (Integer)
import GHC.Word (Word)
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

infixr 6 <\

-- | The partial ordering induced by the join-semilattice structure
(<\) :: (JoinSemiLattice a) => a -> a -> Bool
(<\) = joinLeq

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

infixr 6 </

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
-- > x \/ bottom == x
class (JoinSemiLattice a) => LowerBounded a where
  bottom :: a

-- | A meet-semilattice with an identity element 'top' for '/\'.
--
-- > x /\ top == x
class (MeetSemiLattice a) => UpperBounded a where
  top :: a

-- | Lattices with both bounds
--
-- > x /\ bottom == bottom
-- > x \/ top == top
type BoundedLattice a = (JoinSemiLattice a, MeetSemiLattice a, LowerBounded a, UpperBounded a)

instance JoinSemiLattice Float where
  (\/) = max

instance MeetSemiLattice Float where
  (/\) = min

instance JoinSemiLattice Double where
  (\/) = max

instance MeetSemiLattice Double where
  (/\) = min

instance JoinSemiLattice Int where
  (\/) = max

instance MeetSemiLattice Int where
  (/\) = min

instance JoinSemiLattice Integer where
  (\/) = max

instance MeetSemiLattice Integer where
  (/\) = min

instance JoinSemiLattice Bool where
  (\/) = (||)

instance MeetSemiLattice Bool where
  (/\) = (&&)

instance JoinSemiLattice Natural where
  (\/) = max

instance MeetSemiLattice Natural where
  (/\) = min

instance JoinSemiLattice Int8 where
  (\/) = max

instance MeetSemiLattice Int8 where
  (/\) = min

instance JoinSemiLattice Int16 where
  (\/) = max

instance MeetSemiLattice Int16 where
  (/\) = min

instance JoinSemiLattice Int32 where
  (\/) = max

instance MeetSemiLattice Int32 where
  (/\) = min

instance JoinSemiLattice Int64 where
  (\/) = max

instance MeetSemiLattice Int64 where
  (/\) = min

instance JoinSemiLattice Word where
  (\/) = max

instance MeetSemiLattice Word where
  (/\) = min

instance JoinSemiLattice Word8 where
  (\/) = max

instance MeetSemiLattice Word8 where
  (/\) = min

instance JoinSemiLattice Word16 where
  (\/) = max

instance MeetSemiLattice Word16 where
  (/\) = min

instance JoinSemiLattice Word32 where
  (\/) = max

instance MeetSemiLattice Word32 where
  (/\) = min

instance JoinSemiLattice Word64 where
  (\/) = max

instance MeetSemiLattice Word64 where
  (/\) = min

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
