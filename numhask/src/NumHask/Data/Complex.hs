{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, GeneralizedNewtypeDeriving, DeriveFoldable, DeriveTraversable #-}
module NumHask.Data.Complex where

import GHC.Generics (Generic, Generic1)
import Data.Data (Data)

import NumHask.Algebra.Additive
import Prelude hiding (Num(..), negate)

-- -----------------------------------------------------------------------------
-- The Complex type

infix  6  :+

-- | Complex numbers are an algebraic type.
--
-- For a complex number @z@, @'abs' z@ is a number with the magnitude of @z@,
-- but oriented in the positive real direction, whereas @'signum' z@
-- has the phase of @z@, but unit magnitude.
--
-- The 'Foldable' and 'Traversable' instances traverse the real part first.
data Complex a
  = !a :+ !a    -- ^ forms a complex number from its real and imaginary
                -- rectangular components.
        deriving (Eq, Show, Read, Data, Generic, Generic1
                , Functor, Foldable, Traversable)



-- | Extracts the real part of a complex number.
realPart :: Complex a -> a
realPart (x :+ _) =  x

-- | Extracts the imaginary part of a complex number.
imagPart :: Complex a -> a
imagPart (_ :+ y) =  y

instance (AdditiveMagma a) => AdditiveMagma (Complex a) where
  (rx :+ ix) `plus` (ry :+ iy) = (rx `plus` ry) :+ (ix `plus` iy)

instance (AdditiveUnital a) => AdditiveUnital (Complex a) where
  zero = zero :+ zero  

instance (AdditiveAssociative a) => AdditiveAssociative (Complex a)

instance (AdditiveCommutative a) => AdditiveCommutative (Complex a)

instance (Additive a) => Additive (Complex a)

instance (AdditiveInvertible a) => AdditiveInvertible (Complex a) where
  negate (rx :+ ix) = negate rx :+ negate ix

instance (AdditiveGroup a) => AdditiveGroup (Complex a)
