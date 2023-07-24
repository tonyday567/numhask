{-# LANGUAGE TypeFamilies #-}

-- | Algebra for Actions
--
-- Convention: the dots in the operators point towards the scalars
--
module NumHask.Algebra.Action
  ( AdditiveAction (..),
    (.+),
    SubtractiveAction (..),
    (.-),
    MultiplicativeAction (..),
    (.*),
    DivisiveAction (..),
    (./),
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
class
  (Additive (AdditiveScalar m)) =>
  AdditiveAction m
  where
  type AdditiveScalar m :: Type

  infixl 6 +.
  (+.) :: m -> AdditiveScalar m -> m

infixl 6 .+

-- | flipped additive action
--
-- > (.+) == flip (+.)
(.+) :: (AdditiveAction m) => AdditiveScalar m -> m -> m
(.+) = flip (+.)

-- | Subtractive Action
class
  (AdditiveAction m, Subtractive (AdditiveScalar m)) =>
  SubtractiveAction m
  where
  infixl 6 -.
  (-.) :: m -> AdditiveScalar m -> m

infixl 6 .-

-- | Subtraction with the scalar on the left
--
-- > (.-) == (.+) . negate
(.-) :: (AdditiveAction m, Subtractive m) => AdditiveScalar m -> m -> m
a .- b = a .+ negate b

-- | Multiplicative Action
class
  (Multiplicative (Scalar m)) =>
  MultiplicativeAction m
  where
  type Scalar m :: Type

  infixl 7 *.
  (*.) :: m -> Scalar m -> m

infixl 7 .*

-- | flipped multiplicative action
--
-- > (.*) == flip (*.)
(.*) :: (MultiplicativeAction m) => Scalar m -> m -> m
(.*) = flip (*.)

-- | Divisive Action
class
  (Divisive (Scalar m), MultiplicativeAction m) =>
  DivisiveAction m
  where
  infixl 7 /.
  (/.) :: m -> Scalar m -> m

-- | left scalar division
--
-- > (./) == (.*) . recip
(./) :: (MultiplicativeAction m, Divisive m) => Scalar m -> m -> m
a ./ b = a .* recip b

-- | A <https://en.wikipedia.org/wiki/Module_(mathematics) Module>
--
-- > a .* one == a
-- > (a + b) .* c == (a .* c) + (b .* c)
-- > c *. (a + b) == (c *. a) + (c *. b)
-- > a .* zero == zero
-- > a .* b == b *. a
type Module m = (Distributive (Scalar m), MultiplicativeAction m)
