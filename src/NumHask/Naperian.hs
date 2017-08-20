{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | numbers with a shape
module NumHask.Naperian
    ( HasShape(..)
    , module Data.Functor.Rep
    , Naperian
    ) where

import Protolude (Int)
import Data.Functor.Rep

class HasShape f where
    type Shape f
    shape :: f a -> Shape f
    ndim :: f a -> Int

class (HasShape f, Representable f) => Naperian f

