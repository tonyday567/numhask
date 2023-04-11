{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | Algebra for Modules
module NumHask.Algebra.Module
  ( AdditiveAction (..),
    (+.),
    SubtractiveAction (..),
    (-.),
    MultiplicativeAction (..),
    (*.),
    DivisiveAction (..),
    (/.),
    Module,
  )
where

import NumHask.Algebra.Additive (Additive, Subtractive, negate)
import NumHask.Algebra.Multiplicative (Divisive, Multiplicative, recip)
import NumHask.Algebra.Ring (Distributive)
import Prelude (flip)
import Data.Kind (Type)

-- | Additive Action
class
  (Additive (AdditiveScalar m)) =>
  AdditiveAction m
  where
    type AdditiveScalar m :: Type

    infixl 6 .+
    (.+) :: a -> m -> m

infixl 6 +.

-- | flipped additive action
--
-- > (+.) == flip (.+)
(+.) :: (AdditiveAction m) => m -> AdditiveScalar m -> m
(+.) = flip (.+)

-- | Subtractive Action
class
  (AdditiveAction m, Subtractive (AdditiveScalar m)) =>
  SubtractiveAction m
  where
  infixl 6 .-
  (.-) :: AdditiveScalar m -> m -> m

infixl 6 -.

-- | right scalar subtraction
--
-- > (-.) == (+.) . negate
(-.) :: (AdditiveAction m, Subtractive (AdditiveScalar m)) => m -> AdditiveScalar m -> m
a -. b = a +. negate b

-- | Multiplicative Action
class
  (Multiplicative (Scalar m)) =>
  MultiplicativeAction m
  where
    type Scalar m :: Type

    infixl 7 .*
    (.*) :: Scalar m -> m -> m

infixl 7 *.

-- | flipped multiplicative action
--
-- > (*.) == flip (.*)
(*.) :: (MultiplicativeAction m) => m -> Scalar m -> m
(*.) = flip (.*)

-- | Divisive Action
class
  (Divisive (Scalar m), MultiplicativeAction m) =>
  DivisiveAction m
  where
  infixl 7 ./
  (./) :: Scalar m -> m -> m

-- | right scalar division
--
-- > (/.) == (*.) . recip
(/.) :: (MultiplicativeAction m, Divisive (Scalar m)) => m -> Scalar m -> m
a /. b = a *. recip b

-- | A <https://en.wikipedia.org/wiki/Module_(mathematics) Module>
--
-- > a .* one == a
-- > (a + b) .* c == (a .* c) + (b .* c)
-- > c *. (a + b) == (c *. a) + (c *. b)
-- > a .* zero == zero
-- > a .* b == b *. a
type Module m = (Distributive (Scalar m), MultiplicativeAction m)
