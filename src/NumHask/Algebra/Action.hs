{-# LANGUAGE CPP #-}
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
#if defined(__GLASGOW_HASKELL__)
class
  (Additive (AdditiveScalar m)) =>
  AdditiveAction m
  where
  type AdditiveScalar m :: Type

  infixl 6 |+
  (|+) :: m -> AdditiveScalar m -> m
#endif
#if defined(__MHS__)
class
  (Additive a) =>
  AdditiveAction m a | m -> a
  where
  infixl 6 |+
  (|+) :: m -> a -> m
#endif

-- | flipped additive action
--
-- > (+|) == flip (|+)
-- > zero +| m = m
#if defined(__GLASGOW_HASKELL__)
(+|) :: (AdditiveAction m) => AdditiveScalar m -> m -> m
#endif
#if defined(__MHS__)
(+|) :: (Additive a, AdditiveAction m a) => a -> m a -> m a
#endif
(+|) = flip (|+)

infixl 6 +|

-- | Subtractive Action
--
-- > m |- zero = m
#if defined(__GLASGOW_HASKELL__)
class
  (AdditiveAction m, Subtractive (AdditiveScalar m)) =>
  SubtractiveAction m
  where
  infixl 6 |-
  (|-) :: m -> AdditiveScalar m -> m
#endif
#if defined(__MHS__)
class
  (Subtractive a) =>
  SubtractiveAction m a | m -> a
  where
  infixl 6 |-
  (|-) :: m -> a -> m
#endif

infixl 6 -|

-- | Subtraction with the scalar on the left
--
-- > (-|) == (+|) . negate
-- > zero -| m = negate m
#if defined(__GLASGOW_HASKELL__)
(-|) :: (AdditiveAction m, Subtractive m) => AdditiveScalar m -> m -> m
#endif
#if defined(__MHS__)
(-|) :: (AdditiveAction m a, Subtractive a) => a -> m a -> m a
#endif
a -| b = a +| negate b

-- | Multiplicative Action
--
-- > m |* one = m
-- > m |* zero = zero
#if defined(__GLASGOW_HASKELL__)
class
  (Multiplicative (Scalar m)) =>
  MultiplicativeAction m
  where
  type Scalar m :: Type
  infixl 7 |*
  (|*) :: m -> Scalar m -> m
#endif
#if defined(__MHS__)
class
  (Multiplicative a) =>
  MultiplicativeAction m a | m -> a
  where
  infixl 6 |*
  (|*) :: m -> a -> m
#endif

infixl 7 *|

-- | flipped multiplicative action
--
-- > (*|) == flip (|*)
-- > one *| m = one
-- > zero *| m = zero
#if defined(__GLASGOW_HASKELL__)
(*|) :: (MultiplicativeAction m) => Scalar m -> m -> m
#endif
#if defined(__MHS__)
(*|) :: (Multiplicative a, MultiplicativeAction m a) => a -> m a -> m a
#endif
(*|) = flip (|*)

-- | Divisive Action
--
-- > m |/ one = m
#if defined(__GLASGOW_HASKELL__)
class
  (Divisive (Scalar m), MultiplicativeAction m) =>
  DivisiveAction m
  where
  infixl 7 |/
  (|/) :: m -> Scalar m -> m
#endif
#if defined(__MHS__)
class
  (Divisive a) =>
  DivisiveAction m a | m -> a
  where
  infixl 6 |/
  (|/) :: m -> a -> m
#endif

-- | left scalar division
--
-- > (/|) == (*|) . recip
-- > one |/ m = recip m
#if defined(__GLASGOW_HASKELL__)
(/|) :: (MultiplicativeAction m, Divisive m) => Scalar m -> m -> m
#endif
#if defined(__MHS__)
(/|) :: (MultiplicativeAction m a, Divisive a) => a -> m a -> m a
#endif
a /| b = a *| recip b

-- | A <https://en.wikipedia.org/wiki/Module_(mathematics) Module>
--
-- > a *| one == a
-- > (a + b) *| c == (a *| c) + (b *| c)
-- > c |* (a + b) == (c |* a) + (c |* b)
-- > a *| zero == zero
-- > a *| b == b |* a
#if defined(__GLASGOW_HASKELL__)
type Module m = (Distributive (Scalar m), MultiplicativeAction m)
#endif
#if defined(__MHS__)
type Module m a = (Distributive a, MultiplicativeAction m a)
#endif
