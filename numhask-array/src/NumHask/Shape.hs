{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | numbers with a shape
module NumHask.Shape
  ( HasShape(..)
    -- * Representable
    -- | Representable has most of what's needed to define numbers that have elements (aka scalars) and a fixed shape.
  , Representable(..)
  )
where

import Data.Functor.Rep

-- | Not everything that has a shape is representable.
--
-- todo: Structure is a useful alternative concept/naming convention
class HasShape f where
  type Shape f
  shape :: f a -> Shape f
