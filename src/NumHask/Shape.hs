{-# OPTIONS_GHC -Wall #-}

-- | numbers with a shape
module NumHask.Shape
    ( HasShape(..)
    ) where

class HasShape f where
    type Shape f
    shape :: f a -> Shape f

