{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

-- | Action
module NumHask.Algebra.Abstract.Action
  ( AdditiveAction(..)
  , AdditiveGroupAction(..)
  , MultiplicativeGroupAction(..)
  , MultiplicativeAction(..)
  ) where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Group

class (Magma (Product (r a))) => MultiplicativeAction r a where
  infixl 7 .*
  (.*) :: r a -> a -> r a
  infixl 7 *.
  (*.) :: a -> r a -> r a

class (Magma (Sum (r a))) =>
  AdditiveAction r a where
  infixl 6 .+
  (.+) :: r a -> a -> r a

  infixl 6 +.
  (+.) :: a -> r a -> r a

class (AbelianGroup (Sum a)) =>
  AdditiveGroupAction r a where
  infixl 6 .-
  (.-) :: r a -> a -> r a

  infixl 6 -.
  (-.) :: a -> r a -> r a

class (AbelianGroup (Product a)) =>
  MultiplicativeGroupAction r a where
  infixl 7 ./
  (./) :: r a -> a -> r a
  infixl 7 /.
  (/.) :: a -> r a -> r a
