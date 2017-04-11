{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | multi-dimensional representable numbers
module NumHask.Naperian
    ( Naperian
    , HasShape(..)
    ) where

import Protolude (Int, foldr, Foldable(..), ($), (<$>), fmap, fst, snd, or, and)
import Data.Functor.Rep
import NumHask.Algebra

-- | ToDo: integrate ni Naperian instance
class HasShape f where
    type Shape f
    shape :: f a -> Shape f
    ndim :: f a -> Int

class (HasShape f, Representable f) => Naperian f

instance {-# Overlappable #-} (Naperian f, AdditiveMagma a) => AdditiveMagma (f a) where
    plus = liftR2 plus
instance {-# Overlappable #-} (Naperian f, AdditiveUnital a) => AdditiveUnital (f a) where
    zero = pureRep zero
instance {-# Overlappable #-} (Naperian f, AdditiveAssociative a) => AdditiveAssociative (f a)
instance {-# Overlappable #-} (Naperian f, AdditiveCommutative a) => AdditiveCommutative (f a)
instance {-# Overlappable #-} (Naperian f, AdditiveInvertible a) => AdditiveInvertible (f a) where
    negate = fmapRep negate
instance {-# Overlappable #-} (Naperian f, AdditiveMagma a) => AdditiveHomomorphic a (f a) where
    plushom a = pureRep a
instance {-# Overlappable #-} (Naperian f, AdditiveMonoidal a) => AdditiveMonoidal (f a)
instance {-# Overlappable #-} (Naperian f, Additive a) => Additive (f a)
instance {-# Overlappable #-} (Naperian f, AdditiveGroup a) => AdditiveGroup (f a)

instance {-# Overlappable #-} (Naperian f, MultiplicativeMagma a) => MultiplicativeMagma (f a) where
    times = liftR2 times
instance {-# Overlappable #-} (Naperian f, MultiplicativeUnital a) => MultiplicativeUnital (f a) where
    one = pureRep one
instance {-# Overlappable #-} (Naperian f, MultiplicativeAssociative a) => MultiplicativeAssociative (f a)
instance {-# Overlappable #-} (Naperian f, MultiplicativeCommutative a) => MultiplicativeCommutative (f a)
instance {-# Overlappable #-} (Naperian f, MultiplicativeInvertible a) => MultiplicativeInvertible (f a) where
    recip = fmapRep recip
instance {-# Overlappable #-} (Naperian f, MultiplicativeMagma a) => MultiplicativeHomomorphic a (f a) where
    timeshom a = pureRep a
instance {-# Overlappable #-} (Naperian f, MultiplicativeMonoidal a) => MultiplicativeMonoidal (f a)
instance {-# Overlappable #-} (Naperian f, Multiplicative a) => Multiplicative (f a)
instance {-# Overlappable #-} (Naperian f, MultiplicativeGroup a) => MultiplicativeGroup (f a)

instance {-# Overlappable #-} (Naperian f, MultiplicativeMagma a, Additive a) => Distribution (f a)

instance {-# Overlappable #-} (Naperian f, Semiring a) => Semiring (f a)
instance {-# Overlappable #-} (Naperian f, Ring a) => Ring (f a)
instance {-# Overlappable #-} (Naperian f, CRing a) => CRing (f a)
instance {-# Overlappable #-} (Naperian f, Field a) => Field (f a)

instance {-# Overlappable #-} (Naperian f, ExpField a) => ExpField (f a) where
    exp = fmapRep exp
    log = fmapRep log

instance {-# Overlappable #-} (Naperian f, BoundedField a, Foldable f) => BoundedField (f a) where
    isNaN f = or (fmapRep isNaN f)

instance {-# Overlappable #-} (Naperian f, Signed a) => Signed (f a) where
    sign = fmapRep sign
    abs = fmapRep abs

instance {-# Overlappable #-} (Foldable f, Naperian f, ExpField a) =>
    Normed (f a) a where
    size r = sqrt $ foldr (+) zero $ (**(one+one)) <$> r

instance {-# Overlappable #-} (Foldable f, Naperian f, Epsilon a) => Epsilon (f a) where
    nearZero f = and (fmapRep nearZero f)
    aboutEqual a b = and (liftR2 aboutEqual a b)

instance {-# Overlappable #-} (Foldable f, Naperian f, ExpField a) => Metric (f a) a where
    distance a b = size (a - b)

instance {-# Overlappable #-} (Naperian f, Integral a) => Integral (f a) where
    divMod a b = (d,m)
        where
          x = liftR2 divMod a b
          d = fmap fst x
          m = fmap snd x
