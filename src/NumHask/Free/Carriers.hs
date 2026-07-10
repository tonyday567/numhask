{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Concrete semiring carriers useful for demonstrating
-- 'NumHask.Algebra.Ring.StarSemiring'.
module NumHask.Free.Carriers
  ( -- * Boolean semiring
    Warshall (..),

    -- * Tropical semiring
    MinPlus (..),

    -- * Field semiring
    FieldStar (..),
  )
where

import NumHask.Algebra.Additive qualified as NHA
import NumHask.Algebra.Multiplicative qualified as NHM
import NumHask.Algebra.Ring qualified as NHR
import Prelude (Bool, Double, Eq, Fractional, Num, Ord, Show)
import Prelude qualified as P

-- | Boolean semiring for Warshall's transitive closure.
--
-- 'plus' is '||', 'times' is '&&', 'star' is constantly 'True'.
newtype Warshall = Warshall Bool
  deriving (Eq, Ord, Show)

instance NHM.Multiplicative Warshall where
  one = Warshall P.True
  Warshall a * Warshall b = Warshall (a P.&& b)

instance NHA.Additive Warshall where
  zero = Warshall P.False
  Warshall a + Warshall b = Warshall (a P.|| b)

instance NHR.StarSemiring Warshall where
  star _ = Warshall P.True

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

instance NHR.StarSemiring MinPlus where
  star _ = NHM.one

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
