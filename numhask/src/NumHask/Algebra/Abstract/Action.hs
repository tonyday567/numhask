{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

-- | Action
module NumHask.Algebra.Abstract.Action
  ( AdditiveAction(..)
  , SubtractiveAction(..)
  , MultiplicativeAction(..)
  , DivisiveAction(..)
  ) where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Multiplicative

class (Additive a) =>
  AdditiveAction r a where
  infixl 6 .+
  (.+) :: r a -> a -> r a

  infixl 6 +.
  (+.) :: a -> r a -> r a

class (Subtractive a) =>
  SubtractiveAction r a where
  infixl 6 .-
  (.-) :: r a -> a -> r a

  infixl 6 -.
  (-.) :: a -> r a -> r a

class (Multiplicative a) =>
  MultiplicativeAction r a where
  infixl 7 .*
  (.*) :: r a -> a -> r a
  infixl 7 *.
  (*.) :: a -> r a -> r a

class (Divisive a) =>
  DivisiveAction r a where
  infixl 7 ./
  (./) :: r a -> a -> r a
  infixl 7 /.
  (/.) :: a -> r a -> r a
