{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | numbers with a shape
module NumHask.Shape
  ( HasShape(..)
    -- | Representable has most of what's needed to define things that have elements and a shape.
  , Representable(..)
  , Singleton(..)
  ) where

import Data.Functor.Rep

-- | Not everything that has a shape is representable.
-- todo: Structure is a useful alternative concept/naming convention
class HasShape f where
  type Shape f
  shape :: f a -> Shape f

-- Undecidable + missing method
-- instance (Representable f) => HasShape f where
--     type Shape f = Rep f

-- | todo: consider a schema such as:
--
-- Vector v
-- Single s
-- Zero
--
-- and then
-- singleton = Single a, and
-- singleton zero = Zero
--
class Singleton f where
  singleton :: a -> f a

instance (Representable f) => Singleton f where
  singleton = pureRep
