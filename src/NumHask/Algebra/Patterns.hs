{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Patterns for common tests
module NumHask.Algebra.Patterns
  (
    pattern Zero,
    pattern One,
    pattern MinusOne,
  )
where

import NumHask.Algebra.Additive
import NumHask.Algebra.Multiplicative
import Prelude (Bool(..), Eq(..), (.))

-- | Enabling pattern matching on zero:
--
-- >>> isItZero Zero = True
-- >>> isItZero _    = False
pattern Zero :: forall a. (Eq a, Additive a) => a
pattern Zero <- ((== zero) -> True)


-- | Enabling pattern matching on one:
--
-- >>> isItOne One = True
-- >>> isItOne _   = False
pattern One :: forall a. (Eq a, Multiplicative a) => a
pattern One <- ((== one) -> True)


-- | Enabling pattern matching on minus one:
--
-- >>> isItMinusOne MinusOne = True
-- >>> isItMinusOne _        = False
--
-- The means of testing (that is, add one, and check if it equals
-- zero) might be surprising. Other, more obvious, methods would
-- result in underflow errors. (For example, we could negate and test
-- if it's equal to one, but that would fail on any nonzero
-- 'Natural'. Similarly, we could test for equality with the negation of
-- one, but that would fail on any 'Natural' whatsoever, since 'negate
-- one' underflows.)
pattern MinusOne :: forall a. (Eq a, Additive a, Multiplicative a) => a
pattern MinusOne <- (((== zero) . (+ one)) -> True)
