{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | multi-dimensional representable numbers
module NumHask.Naperian where

import Protolude (Int, foldr, Foldable(..), ($), (<$>), fmap, fst, snd, or, and)
import Data.Functor.Rep
import NumHask.Algebra

-- | Could possibly be integrated with 'Representable' instance creation
class HasShape f where
    type Shape f
    shape :: (HasShape f) => f a -> Shape f
    ndim :: (HasShape f) => f a -> Int

class (HasShape f, Representable f) => Naperian f

instance (Naperian f, AdditiveMagma a) => AdditiveMagma (f a) where
    plus = liftR2 plus
instance (Naperian f, AdditiveUnital a) => AdditiveUnital (f a) where
    zero = pureRep zero
instance (Naperian f, AdditiveAssociative a) => AdditiveAssociative (f a)
instance (Naperian f, AdditiveCommutative a) => AdditiveCommutative (f a)
instance (Naperian f, AdditiveInvertible a) => AdditiveInvertible (f a) where
    negate = fmapRep negate
instance (Naperian f, AdditiveMagma a) => AdditiveHomomorphic a (f a) where
    plushom a = pureRep a
instance (Naperian f, AdditiveMonoidal a) => AdditiveMonoidal (f a)
instance (Naperian f, Additive a) => Additive (f a)
instance (Naperian f, AdditiveGroup a) => AdditiveGroup (f a)

instance (Naperian f, MultiplicativeMagma a) => MultiplicativeMagma (f a) where
    times = liftR2 times
instance (Naperian f, MultiplicativeUnital a) => MultiplicativeUnital (f a) where
    one = pureRep one
instance (Naperian f, MultiplicativeAssociative a) => MultiplicativeAssociative (f a)
instance (Naperian f, MultiplicativeCommutative a) => MultiplicativeCommutative (f a)
instance (Naperian f, MultiplicativeInvertible a) => MultiplicativeInvertible (f a) where
    recip = fmapRep recip
instance (Naperian f, MultiplicativeMagma a) => MultiplicativeHomomorphic a (f a) where
    timeshom a = pureRep a
instance (Naperian f, MultiplicativeMonoidal a) => MultiplicativeMonoidal (f a)
instance (Naperian f, Multiplicative a) => Multiplicative (f a)
instance (Naperian f, MultiplicativeGroup a) => MultiplicativeGroup (f a)

instance (Naperian f, MultiplicativeMagma a, Additive a) => Distribution (f a)

instance (Naperian f, Semiring a) => Semiring (f a)
instance (Naperian f, Ring a) => Ring (f a)
instance (Naperian f, CRing a) => CRing (f a)
instance (Naperian f, Field a) => Field (f a)

instance (Naperian f, ExpRing a) => ExpRing (f a) where
    logBase = liftR2 logBase
    (**) = liftR2 logBase

instance (Naperian f, ExpField a) => ExpField (f a) where
    exp = fmapRep exp
    log = fmapRep log

instance (Naperian f, BoundedField a, Foldable f) => BoundedField (f a) where
    isNaN f = or (fmapRep isNaN f)

instance (Naperian f, Signed a) => Signed (f a) where
    sign = fmapRep sign
    abs = fmapRep abs

instance (Foldable f, Naperian f, ExpField a, ExpRing a) =>
    Normed (f a) a where
    size r = sqrt $ foldr (+) zero $ (**(one+one)) <$> r

instance (Foldable f, Naperian f, Epsilon a) => Epsilon (f a) where
    nearZero f = and (fmapRep nearZero f)
    aboutEqual a b = and (liftR2 aboutEqual a b)

instance (Foldable f, Naperian f, ExpField a) => Metric (f a) a where
    distance a b = size (a - b)

instance (Naperian f, Integral a) => Integral (f a) where
    divMod a b = (d,m)
        where
          x = liftR2 divMod a b
          d = fmap fst x
          m = fmap snd x
