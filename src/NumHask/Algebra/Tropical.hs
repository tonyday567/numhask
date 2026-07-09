-- | Tropical semirings.
module NumHask.Algebra.Tropical
  ( MinPlus (..),
  )
where

import NumHask.Algebra.Additive (Additive (..))
import NumHask.Algebra.Group (Idempotent, Magma (..))
import NumHask.Algebra.Multiplicative (Multiplicative (..))
import NumHask.Algebra.Ring (KleeneAlgebra, StarSemiring (..))
import Prelude (Double, Eq, Ord, Show, fromInteger)
import Prelude qualified as P

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Algebra.Tropical

-- | The min-plus tropical semiring.
--
-- Addition is 'min', multiplication is ordinary addition, the additive unit
-- is positive infinity, and the multiplicative unit is zero.
--
-- >>> MinPlus 3 + MinPlus 2 :: MinPlus Double
-- MinPlus {getMinPlus = 2.0}
--
-- >>> MinPlus 3 * MinPlus 2 :: MinPlus Double
-- MinPlus {getMinPlus = 5.0}
newtype MinPlus a = MinPlus
  { getMinPlus :: a
  }
  deriving (Eq, Ord, Show)

instance Additive (MinPlus Double) where
  MinPlus a + MinPlus b = MinPlus (P.min a b)
  zero = MinPlus (1 P./ 0)

instance Multiplicative (MinPlus Double) where
  MinPlus a * MinPlus b = MinPlus (a P.+ b)
  one = MinPlus 0

instance Magma (MinPlus Double) where
  a ⊕ b = a + b

instance Idempotent (MinPlus Double)

-- | Star is zero in a min-plus semiring: the cheapest repeated traversal is
-- to stay put.
--
-- >>> star (MinPlus 2 :: MinPlus Double)
-- MinPlus {getMinPlus = 0.0}
--
-- Conway equations for 'MinPlus Double'.
--
-- >>> let a = MinPlus 2 :: MinPlus Double; b = MinPlus 3 :: MinPlus Double in star (a * b) == one + a * star (b * a) * b
-- True
--
-- >>> let a = MinPlus 2 :: MinPlus Double; b = MinPlus 3 :: MinPlus Double in star (a + b) == star (star a * b) * star a
-- True
instance StarSemiring (MinPlus Double) where
  star _ = one

instance KleeneAlgebra (MinPlus Double)
