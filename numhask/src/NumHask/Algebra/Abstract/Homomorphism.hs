{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | The Homomorphism Hirarchy
module NumHask.Algebra.Abstract.Homomorphism
      ( 
          Hom(..)
      ,   Endo(..)
      ,   Iso(..)
      ,   Automorphism(..)
      )
      where

import qualified Prelude                       as P
import NumHask.Algebra.Abstract.Group

-- | A Homomorphism between two magmas
-- law: forall a b. hom(a `comb` b) = hom(a) `comb` hom(b)
class (Magma a, Magma b) => Hom a b where
    hom :: a -> b

class (Hom a a) => Endo a
instance (Hom a a) => Endo a

-- | A Isomorphism between two magmas
-- an Isomorphism is a bijective Homomorphism
class (Hom a b, Hom b a) => Iso a b where
    iso :: a -> b
    iso = hom
    invIso :: b -> a
    invIso = hom

class (Iso a a) => Automorphism a
instance (Iso a a) => Automorphism a