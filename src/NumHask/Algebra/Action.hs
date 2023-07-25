{-# LANGUAGE TypeFamilies #-}

-- | Algebra for Actions
--
-- Convention: the |'s in the operators point towards the higher-kinded number, representing an operator or action __into__ a structure.
module NumHask.Algebra.Action
  ( AdditiveAction (..),
    (+|),
    SubtractiveAction (..),
    (-|),
    MultiplicativeAction (..),
    (*|),
    DivisiveAction (..),
    (/|),
    Module,
  )
where

import Data.Kind (Type)
import NumHask.Algebra.Additive (Additive, Subtractive, negate)
import NumHask.Algebra.Multiplicative (Divisive, Multiplicative, recip)
import NumHask.Algebra.Ring (Distributive)
import Prelude (flip)

-- | Additive Action
--
-- > m |+ zero == m
class
  (Additive (AdditiveScalar m)) =>
  AdditiveAction m
  where
  type AdditiveScalar m :: Type

  infixl 6 |+
  (|+) :: m -> AdditiveScalar m -> m

infixl 6 +|

-- | flipped additive action
--
-- > (+|) == flip (|+)
-- > zero +| m = m
(+|) :: (AdditiveAction m) => AdditiveScalar m -> m -> m
(+|) = flip (|+)

-- | Subtractive Action
--
-- > m |- zero = m
class
  (AdditiveAction m, Subtractive (AdditiveScalar m)) =>
  SubtractiveAction m
  where
  infixl 6 |-
  (|-) :: m -> AdditiveScalar m -> m

infixl 6 -|

-- | Subtraction with the scalar on the left
--
-- > (-|) == (+|) . negate
-- > zero -| m = negate m
(-|) :: (AdditiveAction m, Subtractive m) => AdditiveScalar m -> m -> m
a -| b = a +| negate b

-- | Multiplicative Action
--
-- > m |* one = m
-- > m |* zero = zero
class
  (Multiplicative (Scalar m)) =>
  MultiplicativeAction m
  where
  type Scalar m :: Type

  infixl 7 |*
  (|*) :: m -> Scalar m -> m

infixl 7 *|

-- | flipped multiplicative action
--
-- > (*|) == flip (|*)
-- > one *| m = one
-- > zero *| m = zero
(*|) :: (MultiplicativeAction m) => Scalar m -> m -> m
(*|) = flip (|*)

-- | Divisive Action
--
-- > m |/ one = m
class
  (Divisive (Scalar m), MultiplicativeAction m) =>
  DivisiveAction m
  where
  infixl 7 |/
  (|/) :: m -> Scalar m -> m

-- | left scalar division
--
-- > (/|) == (*|) . recip
-- > one |/ m = recip m
(/|) :: (MultiplicativeAction m, Divisive m) => Scalar m -> m -> m
a /| b = a *| recip b

-- | A <https://en.wikipedia.org/wiki/Module_(mathematics) Module>
--
-- > a *| one == a
-- > (a + b) *| c == (a *| c) + (b *| c)
-- > c |* (a + b) == (c |* a) + (c |* b)
-- > a *| zero == zero
-- > a *| b == b |* a
type Module m = (Distributive (Scalar m), MultiplicativeAction m)
