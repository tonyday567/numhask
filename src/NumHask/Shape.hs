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
  , Singleton(..)
  ) where

import Data.Functor.Rep

-- | Not everything that has a shape is representable.
--
-- todo: Structure is a useful alternative concept/naming convention
class HasShape f where
  type Shape f
  shape :: f a -> Shape f

-- Undecidable + missing method
-- instance (Representable f) => HasShape f where
--     type Shape f = Rep f
-- | This class could also be called replicate.  Looking forward, however, it may be useful to consider a Representable such as
--
-- > VectorThing a = Vector a | Single a | Zero
--
-- and then
--
-- > singleton a = Single a
-- > singleton zero = Zero
--
-- short-circuiting an expensive computation.  As the class action then doesn't actually involve replication, it would be mis-named.
--
class Singleton f where
  singleton :: a -> f a

instance (Representable f) => Singleton f where
  singleton = pureRep
