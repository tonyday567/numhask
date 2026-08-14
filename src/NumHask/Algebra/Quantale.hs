{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | [Quantales](https://en.wikipedia.org/wiki/Quantale)
module NumHask.Algebra.Quantale
  ( Quantale,
    Residuated (..),
    StarAutonomous (..),
  )
where

import Data.Bool (Bool (..))
import Data.Set (Set)
import qualified Data.Set as Set
import NumHask.Algebra.Lattice (CompleteJoinSemiLattice (..))
import NumHask.Algebra.Multiplicative (Multiplicative (..))
import NumHask.Free.Carriers (MinPlus (..), Warshall (..))
import Prelude qualified as P

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Free.Carriers

-- | A quantale is a monoid in complete join-semilattices: a complete
-- join-semilattice equipped with an associative multiplication ('*') and unit
-- ('one') that distributes over arbitrary joins.
--
-- > a * joins xs == joins (fmap (a *) xs)
-- > joins xs * a == joins (fmap (* a) xs)
-- > a * bottom == bottom
-- > bottom * a == bottom
class (CompleteJoinSemiLattice a, Multiplicative a) => Quantale a

-- | If a quantale is also a 'NumHask.Algebra.Ring.StarSemiring', then 'star'
-- is the least fixed point, equivalently the join of the geometric series:
--
-- > star a == joins (iterate (a *) one)
--
-- A 'NumHask.Algebra.Ring.KleeneAlgebra' is therefore the fragment of a
-- (commutative) quantale that only needs these iterative joins, while a
-- quantale admits arbitrary joins.

-- | A /residuated/ quantale: a quantale equipped with the left and right
-- residuals of multiplication.
--
-- In any quantale, multiplication by a fixed element preserves joins, so it
-- has both left and right adjoints. These adjoints are the residuals:
--
-- > a * lres a b <= b
-- > lres a b == joins [ x | a * x <= b ]
--
-- > rres a b * a <= b
-- > rres a b == joins [ x | x * a <= b ]
--
-- The order @<=@ is the one induced by the join-semilattice:
-- @x <= y@ iff @x \/ y == y@.
--
-- For commutative quantales the two residuals coincide: @lres = rres@.
class (Quantale a) => Residuated a where
  {-# MINIMAL lres, rres #-}

  -- | Left residual of multiplication: the greatest @x@ with @a * x <= b@.
  lres :: a -> a -> a

  -- | Right residual of multiplication: the greatest @x@ with @x * a <= b@.
  rres :: a -> a -> a

-- | A /star-autonomous/ quantale: a quantale with an involutive,
-- order-reversing linear negation.
--
-- The negation is an anti-automorphism of the underlying quantale:
--
-- > neg (neg a) == a
-- > joinLeq a b == joinLeq (neg b) (neg a)
-- > neg (a * b) == neg b * neg a
--
-- Multiplicative disjunction ('par') and its unit ('bot') are the duals of
-- '(*)' and 'one':
--
-- > par a b == neg (neg a * neg b)
-- > bot == neg one
--
-- This is the missing connective that turns a quantale into a model of
-- classical linear logic.
class (Quantale a) => StarAutonomous a where
  {-# MINIMAL neg #-}

  -- | Linear negation.
  neg :: a -> a

  -- | Multiplicative disjunction (par), dual to multiplication.
  par :: a -> a -> a
  par a b = neg (neg a * neg b)

  -- | Unit of 'par', dual to 'one'.
  bot :: a
  bot = neg one

-- | Boolean quantale: join is disjunction, multiplication is conjunction.
instance Quantale Bool

-- | Boolean implication as the residual of conjunction.
instance Residuated Bool where
  lres a b = P.not a P.|| b
  rres = lres

-- | Boolean negation is complement; 'par' is disjunction and 'bot' is 'False'.
instance StarAutonomous Bool where
  neg = P.not
  par = (P.||)
  bot = False

-- | Boolean quantale: join is disjunction, multiplication is conjunction.
instance Quantale Warshall

-- | Boolean implication as the residual of conjunction.
--
-- >>> lres (Warshall True) (Warshall False)
-- Warshall False
--
-- >>> lres (Warshall False) (Warshall True)
-- Warshall True
instance Residuated Warshall where
  lres (Warshall a) (Warshall b) = Warshall (P.not a P.|| b)
  rres = lres

-- | Boolean negation as complement; 'par' is disjunction and 'bot' is
-- 'Warshall False'.
instance StarAutonomous Warshall where
  neg (Warshall a) = Warshall (P.not a)
  par (Warshall a) (Warshall b) = Warshall (a P.|| b)
  bot = Warshall P.False

-- | Tropical (min-plus) quantale: join is minimum, bottom is positive
-- infinity, multiplication is addition.
instance Quantale (MinPlus P.Double)

-- | Tropical residual is truncated subtraction.
--
-- >>> getMinPlus (lres (MinPlus 2) (MinPlus 5) :: MinPlus Double)
-- 3.0
--
-- >>> getMinPlus (lres (MinPlus 5) (MinPlus 2) :: MinPlus Double)
-- -3.0
instance Residuated (MinPlus P.Double) where
  lres (MinPlus a) (MinPlus b) = MinPlus (b P.- a)
  rres = lres

-- | Tropical linear negation is additive inverse; 'par' is addition and
-- 'bot' is the multiplicative unit @0@.
instance StarAutonomous (MinPlus P.Double) where
  neg (MinPlus a) = MinPlus (P.negate a)
  par (MinPlus a) (MinPlus b) = MinPlus (a P.+ b)
  bot = neg one

-- | Power-set quantale of a monoid: join is union, bottom is the empty set,
-- multiplication is pointwise product of subsets, and the unit is the
-- singleton set containing 'one'.
--
-- This is the language quantale when the underlying monoid is the free monoid
-- over an alphabet.
instance (P.Ord a, Multiplicative a) => Multiplicative (Set a) where
  one = Set.singleton one
  s * t = Set.fromList [x * y | x <- Set.toList s, y <- Set.toList t]

instance (P.Ord a, Multiplicative a) => Quantale (Set a)
