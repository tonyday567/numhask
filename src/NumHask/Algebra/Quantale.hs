{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | [Quantales](https://en.wikipedia.org/wiki/Quantale)
module NumHask.Algebra.Quantale
  ( Quantale,
  )
where

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

-- | Boolean quantale: join is disjunction, multiplication is conjunction.
instance Quantale Warshall

-- | Tropical (min-plus) quantale: join is minimum, bottom is positive
-- infinity, multiplication is addition.
instance Quantale (MinPlus P.Double)

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
