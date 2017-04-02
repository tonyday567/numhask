{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Metric structure
module NumHask.Algebra.Metric (
    -- * Metric
    BoundedField(..)
  , infinity
  , neginfinity
  , Metric(..)
  , Normed(..)
  , Signed(..)
  , Epsilon(..)
  , (≈)
  , QuotientField(..)
  ) where

import qualified Protolude as P
import Protolude (Double, Float, Int, Integer, ($), (<$>), Foldable(..), foldr, Bool(..), Ord(..), Eq(..), any)
import Data.Functor.Rep
import NumHask.Algebra.Ring
import NumHask.Algebra.Field
import NumHask.Algebra.Additive
import NumHask.Algebra.Exponential
import NumHask.Algebra.Multiplicative

-- | providing the concepts of infinity and NaN, thus moving away from error throwing
class (Field a) => BoundedField a where
    maxBound :: a
    maxBound = one/zero

    minBound :: a
    minBound = negate (one/zero)

    nan :: a
    nan = zero/zero

    isNaN :: a -> Bool

-- | prints as `Infinity`
infinity :: BoundedField a => a
infinity = maxBound

-- | prints as `-Infinity`
neginfinity :: BoundedField a => a
neginfinity = minBound

instance BoundedField Float where isNaN = P.isNaN
instance BoundedField Double where isNaN = P.isNaN
instance (Foldable r, Representable r, BoundedField a) =>
    BoundedField (r a) where
    isNaN a = any isNaN a

-- | abs and signnum are also warts on the standard 'Num' class, and are separated here to provide a cleaner structure.
class ( AdditiveUnital a
      , AdditiveGroup a
      , Multiplicative a
      ) => Signed a where
    sign :: a -> a
    abs :: a -> a

instance Signed Double where
    sign a = if a >= zero then one else negate one
    abs = P.abs
instance Signed Float where
    sign a = if a >= zero then one else negate one
    abs = P.abs
instance Signed Int where
    sign a = if a >= zero then one else negate one
    abs = P.abs
instance Signed Integer where
    sign a = if a >= zero then one else negate one
    abs = P.abs
instance (Representable r, Signed a) => Signed (r a) where
    sign = fmapRep sign
    abs = fmapRep abs

-- | Normed is a current wart on the NumHask api, causing all sorts of runaway constraint boiler-plate.
class Normed a b where
    size :: a -> b

instance Normed Double Double where size = P.abs
instance Normed Float Float where size = P.abs
instance Normed Int Int where size = P.abs
instance Normed Integer Integer where size = P.abs
instance (Foldable r, Representable r, ExpField a, ExpRing a) =>
    Normed (r a) a where
    size r = sqrt $ foldr (+) zero $ (**(one+one)) <$> r

-- | This should probably be split off into some sort of alternative Equality logic, but to what end?
class (AdditiveGroup a) => Epsilon a where
    nearZero :: a -> Bool
    aboutEqual :: a -> a -> Bool

infixl 4 ≈

-- | utf ???
(≈) :: (Epsilon a) => a -> a -> Bool
(≈) = aboutEqual

instance Epsilon Double where
    nearZero a = abs a <= (1e-12 :: Double)
    aboutEqual a b = nearZero $ a - b

instance Epsilon Float where
    nearZero a = abs a <= (1e-6 :: Float)
    aboutEqual a b = nearZero $ a - b

instance Epsilon Int where
    nearZero a = a == zero
    aboutEqual a b = nearZero $ a - b

instance Epsilon Integer where
    nearZero a = a == zero
    aboutEqual a b = nearZero $ a - b

instance (Foldable r, Representable r, Epsilon a) => Epsilon (r a) where
    nearZero a = any nearZero $ toList a
    aboutEqual a b = any P.identity $ liftR2 aboutEqual a b

-- | distance between numbers
class Metric a b where
    distance :: a -> a -> b

instance Metric Double Double where distance a b = abs (a - b)
instance Metric Float Float where distance a b = abs (a - b)
instance Metric Int Int where distance a b = abs (a - b)
instance Metric Integer Integer where distance a b = abs (a - b)

instance (P.Foldable r, Representable r, ExpField a) => Metric (r a) a where
    distance a b = size (a - b)

-- | quotient fields also explode constraints if they are polymorphed to emit general integrals
class (Ring a) => QuotientField a where
    round :: a -> Integer
    ceiling :: a -> Integer
    floor :: a -> Integer
    (^^) :: a -> Integer -> a

instance QuotientField Float where
    round = P.round
    ceiling = P.ceiling
    floor = P.floor
    (^^) = (P.^^)

instance QuotientField Double where
    round = P.round
    ceiling = P.ceiling
    floor = P.floor
    (^^) = (P.^^)
