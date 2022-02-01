{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
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

import Prelude (flip)
import NumHask.Algebra.Additive (Additive, Subtractive, negate)
import NumHask.Algebra.Multiplicative (Divisive, Multiplicative, recip)
import NumHask.Algebra.Ring (Distributive)

-- | Additive Action
class
  (Additive a) =>
  AdditiveAction m a
    | m -> a
  where
  infixl 6 .+
  (.+) :: a -> m -> m

infixl 6 +.

-- | flipped additive action
--
-- > (+.) == flip (.+)
(+.) :: (AdditiveAction m a) => m -> a -> m
(+.) = flip (.+)

-- | Subtractive Action
class
  (Subtractive a) =>
  SubtractiveAction m a
    | m -> a
  where
  infixl 6 .-
  (.-) :: a -> m -> m

infixl 6 -.
-- | right scalar subtraction
--
-- > (-.) == (+.) . negate
--
(-.) :: (AdditiveAction m a, Subtractive a) => m -> a -> m
a -. b = a +. negate b

-- | Multiplicative Action
class
  (Multiplicative a) =>
  MultiplicativeAction m a
    | m -> a
  where
  infixl 7 .*
  (.*) :: a -> m -> m

infixl 7 *.

-- | flipped multiplicative action
--
-- > (*.) == flip (.*)
(*.) :: (MultiplicativeAction m a) => m -> a -> m
(*.) = flip (.*)

-- | Divisive Action
class
  (Divisive a) =>
  DivisiveAction m a
    | m -> a
  where
  infixl 7 ./
  (./) :: a -> m -> m

-- | right scalar division
--
-- > (/.) == (*.) . recip
--
(/.) :: (MultiplicativeAction m a, Divisive a) => m -> a -> m
a /. b = a *. recip b

-- | A <https://en.wikipedia.org/wiki/Module_(mathematics) Module>
--
-- > a .* one == a
-- > (a + b) .* c == (a .* c) + (b .* c)
-- > c *. (a + b) == (c *. a) + (c *. b)
-- > a .* zero == zero
-- > a .* b == b *. a
class (Distributive a, MultiplicativeAction m a) => Module m a
