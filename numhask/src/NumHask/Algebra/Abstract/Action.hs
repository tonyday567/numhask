{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | Action
module NumHask.Algebra.Abstract.Action
  ( Actor
  , AdditiveAction(..)
  , SubtractiveAction(..)
  , MultiplicativeAction(..)
  , DivisiveAction(..)
  ) where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Multiplicative

-- | a type class to represent an action on a higher-kinded number
type family Actor h

class (Additive (Actor h)) =>
  AdditiveAction h where
  infixl 6 .+
  (.+) :: h -> Actor h -> h

  infixl 6 +.
  (+.) :: Actor h -> h -> h

class (Subtractive (Actor h)) =>
  SubtractiveAction h where
  infixl 6 .-
  (.-) :: h -> Actor h -> h

  infixl 6 -.
  (-.) :: Actor h -> h -> h

class (Multiplicative (Actor h)) =>
  MultiplicativeAction h where
  infixl 7 .*
  (.*) :: h -> Actor h -> h
  infixl 7 *.
  (*.) :: Actor h -> h -> h

class (Divisive (Actor h)) =>
  DivisiveAction h where
  infixl 7 ./
  (./) :: h -> Actor h -> h
  infixl 7 /.
  (/.) :: Actor h -> h -> h
