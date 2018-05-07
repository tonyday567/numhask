{-# OPTIONS_GHC -Wall #-}

module NumHask.Algebra.Additive where

class AdditiveMagma a where
  plus :: a -> a -> a

class AdditiveMagma a =>
      AdditiveUnital a where
  zero :: a

class AdditiveMagma a =>
      AdditiveInvertible a where
  negate :: a -> a

