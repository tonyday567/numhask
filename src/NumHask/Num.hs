{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

-- | Orphan instances for conversion between Num and NumHask classes.

module NumHask.Num (
  ) where

import Protolude
import qualified NumHask.Algebra as N
import Data.Functor.Rep

-- | NumHask instances for Num instanced classes
-- not compatible with most other NumHask modules
instance (Num a) => N.AdditiveMagma a where plus = (+)
instance (Num a) => N.AdditiveUnital a where zero = 0
instance (Num a) => N.AdditiveAssociative a
instance (Num a) => N.AdditiveCommutative a
instance (Num a) => N.AdditiveInvertible a where negate = negate
instance (Num a) => N.Additive a
instance (Num a) => N.AdditiveGroup a
instance (Num a) => N.MultiplicativeMagma a where times = (*)
instance (Num a) => N.MultiplicativeUnital a where one = 1
instance (Num a) => N.MultiplicativeCommutative a
instance (Num a) => N.MultiplicativeAssociative a
instance (Fractional a) => N.MultiplicativeInvertible a where recip = recip
instance (Num a) => N.Multiplicative a
instance (Fractional a) => N.MultiplicativeGroup a
instance (Num a) => N.Distribution a
instance (Num a) => N.Semiring a
instance (Num a) => N.Ring a
instance (Num a) => N.CRing a
instance (Fractional a) => N.Field a
instance (Num a) => N.Normed a a where size = abs

-- | Num instance for something built with NumHask
instance ( N.Additive a
         , N.Signed a
         , N.FromInteger a) =>
         Num a where
    (+) = (N.+)
    (-) = (N.-)
    (*) = (N.-)
    negate = N.negate
    signum = N.sign
    abs = N.abs
    fromInteger = N.fromInteger

-- | Num instance for a Representable
instance ( Representable r
         , Num a ) =>
         Num (r a) where
    (+) = liftR2 (+)
    (-) = liftR2 (-)
    (*) = liftR2 (*)
    negate = fmapRep negate
    signum = fmapRep signum
    abs = fmapRep abs
    fromInteger = pureRep . fromInteger

