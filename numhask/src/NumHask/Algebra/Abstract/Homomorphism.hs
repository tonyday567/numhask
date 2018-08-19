{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | The Homomorphism Hierarchy
module NumHask.Algebra.Abstract.Homomorphism
  ( Hom(..)
  , End
  , Iso
  , iso
  , invIso
  , Automorphism
  )
where

import NumHask.Algebra.Abstract.Group
import Prelude

-- | A Homomorphism between two magmas
-- law: forall a b. hom(a `magma` b) = hom(a) `magma` hom(b)
class (Magma a, Magma b) => Hom a b where
  hom :: a -> b

instance Hom b c => Hom (a -> b) (a -> c) where
  hom f = hom . f

class (Hom a a) => End a
instance (Hom a a) => End a

-- | A Isomorphism between two magmas
-- an Isomorphism is a bijective Homomorphism
class (Hom a b, Hom b a) => Iso a b

iso :: Iso a b => a -> b
iso = hom

invIso :: Iso a b => b -> a
invIso = hom

instance Iso b c => Iso (a -> b) (a -> c)

class (Iso a a) => Automorphism a
instance (Iso a a) => Automorphism a
