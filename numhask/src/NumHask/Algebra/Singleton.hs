{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

-- | Homomorphic operation from element to structured number
module NumHask.Algebra.Singleton
  ( Singleton(..)
  ) where

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

