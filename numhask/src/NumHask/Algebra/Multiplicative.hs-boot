{-# OPTIONS_GHC -Wall #-}

module NumHask.Algebra.Multiplicative where

class MultiplicativeMagma a where
  times :: a -> a -> a

class MultiplicativeMagma a =>
      MultiplicativeUnital a where
  one :: a

