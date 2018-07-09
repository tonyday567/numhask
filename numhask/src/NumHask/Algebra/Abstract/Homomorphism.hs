{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | The Homomorphism Hirarchy
module NumHask.Algebra.Abstract.Homomorphism
      ( 
          Hom(..)
      ,   End
      ,   Iso(..)
      ,   Automorphism
      )
      where

import NumHask.Algebra.Abstract.Group

-- | A Homomorphism between two magmas
-- law: forall a b. hom(a `magma` b) = hom(a) `magma` hom(b)
class (Magma a, Magma b) => Hom a b where
    hom :: a -> b

class (Hom a a) => End a
instance (Hom a a) => End a

-- | A Isomorphism between two magmas
-- an Isomorphism is a bijective Homomorphism
class (Hom a b, Hom b a) => Iso a b where
    iso :: a -> b
    iso = hom
    invIso :: b -> a
    invIso = hom

class (Iso a a) => Automorphism a
instance (Iso a a) => Automorphism a
