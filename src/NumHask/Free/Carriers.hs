{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoRebindableSyntax #-}

-- | Concrete semiring carriers useful for demonstrating
-- 'NumHask.Algebra.Ring.StarSemiring'.
--
-- These four carriers are the classic examples of the closed-semiring /
-- algebraic-path-problem literature. They are still, fifty years on, the
-- canonical trio plus the probabilistic addition:
--
-- * Boolean semiring — reachability / transitive closure (Warshall 1962).
-- * Min-plus semiring — shortest paths (Floyd 1962, Roy 1959, Warshall 1962).
-- * Field semiring — matrix inversion via the Neumann series.
-- * Viterbi semiring — maximum-probability paths.
--
-- The unification of path problems over semirings was introduced by
-- Aho, Hopcroft and Ullman in /The Design and Analysis of Computer
-- Algorithms/ (1974) and elaborated by Tarjan in
-- "A Unified Approach to Path Problems" (JACM, 1981).
-- A modern categorical treatment is given by Höfner and Möller in
-- "Dijkstra, Floyd and Warshall Meet Kleene" (2012).
module NumHask.Free.Carriers
  ( -- * Boolean semiring
    Warshall (..),

    -- * Tropical semiring
    MinPlus (..),

    -- * Field semiring
    FieldStar (..),

    -- * Viterbi semiring
    Viterbi (..),
  )
where

import NumHask.Algebra.Additive qualified as NHA
import NumHask.Algebra.Group (Idempotent, Magma (..))
import NumHask.Algebra.Multiplicative qualified as NHM
import NumHask.Algebra.Ring qualified as NHR
import Prelude (Bool, Double, Eq, Fractional, Num, Ord, Show)
import Prelude qualified as P

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Free.Carriers

-- | Boolean semiring for Warshall's transitive closure.
--
-- 'plus' is '||', 'times' is '&&', 'star' is constantly 'True'.
--
-- >>> Warshall True + Warshall False
-- Warshall True
newtype Warshall = Warshall Bool
  deriving (Eq, Ord, Show)

instance NHM.Multiplicative Warshall where
  one = Warshall P.True
  Warshall a * Warshall b = Warshall (a P.&& b)

instance NHA.Additive Warshall where
  zero = Warshall P.False
  Warshall a + Warshall b = Warshall (a P.|| b)

instance Magma Warshall where
  a ⊕ b = a NHA.+ b

instance Idempotent Warshall

-- | Boolean star is constantly true.
--
-- >>> star (Warshall False :: Warshall)
-- Warshall True
instance NHR.StarSemiring Warshall where
  star _ = Warshall P.True

instance NHR.KleeneAlgebra Warshall

-- | Tropical (min-plus) semiring for Floyd–Warshall shortest paths.
--
-- 'plus' is 'min', 'times' is '+', 'star' is constantly '0'.
newtype MinPlus = MinPlus Double
  deriving (Eq, Ord, Show)

instance NHM.Multiplicative MinPlus where
  one = MinPlus 0
  MinPlus a * MinPlus b = MinPlus (a P.+ b)

instance NHA.Additive MinPlus where
  zero = MinPlus (1 P./ 0)
  MinPlus a + MinPlus b = MinPlus (P.min a b)

instance Magma MinPlus where
  a ⊕ b = a NHA.+ b

instance Idempotent MinPlus

-- | Min-plus star is zero.
--
-- >>> star (MinPlus 2 :: MinPlus)
-- MinPlus 0.0
instance NHR.StarSemiring MinPlus where
  star _ = NHM.one

instance NHR.KleeneAlgebra MinPlus

-- | Field semiring for matrix inversion @(I − A)⁻¹@.
--
-- 'star' is the closed Neumann series: @star a = recip (1 − a)@.
newtype FieldStar = FieldStar {unFieldStar :: Double}
  deriving (Eq, Ord, Show, Num, Fractional)

instance NHM.Multiplicative FieldStar where
  one = FieldStar 1
  FieldStar a * FieldStar b = FieldStar (a P.* b)

instance NHA.Additive FieldStar where
  zero = FieldStar 0
  FieldStar a + FieldStar b = FieldStar (a P.+ b)

instance NHA.Subtractive FieldStar where
  negate (FieldStar a) = FieldStar (P.negate a)
  FieldStar a - FieldStar b = FieldStar (a P.- b)

instance NHR.StarSemiring FieldStar where
  star (FieldStar a) = FieldStar (P.recip (1 P.- a))

-- | Viterbi semiring for maximum-probability paths.
--
-- 'plus' is 'max', 'times' is ordinary multiplication, the additive unit is
-- @0@ and the multiplicative unit is @1@. The star of any value is @1@,
-- because the empty path has probability 1 and no repeated path can improve
-- on that.
newtype Viterbi a = Viterbi {getViterbi :: a}
  deriving (Eq, Ord, Show)

instance NHM.Multiplicative (Viterbi Double) where
  one = Viterbi 1
  Viterbi a * Viterbi b = Viterbi (a P.* b)

instance NHA.Additive (Viterbi Double) where
  zero = Viterbi 0
  Viterbi a + Viterbi b = Viterbi (P.max a b)

instance Magma (Viterbi Double) where
  a ⊕ b = a NHA.+ b

instance Idempotent (Viterbi Double)

instance NHR.StarSemiring (Viterbi Double) where
  star _ = NHM.one

instance NHR.KleeneAlgebra (Viterbi Double)
