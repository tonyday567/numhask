{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Interval
module NumHask.Data.Interval
  ( Interval(..)
  )
where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Analysis.Metric
import NumHask.Data.Integral

import Data.Bool (bool)
import Prelude
  hiding ( Num(..)
  , (**)
  , (/)
  , atan
  , cos
  , exp
  , log
  , negate
  , pi
  , recip
  , sin
  , sqrt
  , isNaN
  , floor
  , ceiling
  , fromIntegral
  , Integral
  , quotRem
  , divMod
  , properFraction
  , mod
  , truncate
  , acos
  , asin
  , sinh
  , cosh
  , tanh
  , acosh
  , asinh
  , atanh
  )
import Data.Data (Data)
import GHC.Generics (Generic, Generic1)

data Interval a =
  !a :.: !a | Empty
  deriving ( Eq
           , Show
           , Read
           , Data
           , Generic
           , Generic1
           , Functor
           , Foldable
           , Traversable
           )

instance (Magma (Sum a)) => Magma (Sum (Interval a)) where
  (Sum (l :.: u)) `magma` (Sum (l' :.: u')) =
    Sum $ (l + l') :.: (u + u')
  _ `magma` _ = Sum Empty

instance (Unital (Sum a)) => Unital (Sum (Interval a)) where
  unit = Sum (zero :.: zero)

instance (Associative (Sum a)) => Associative (Sum (Interval a))

instance (Commutative (Sum a)) => Commutative (Sum (Interval a))

instance (Invertible (Sum a)) => Invertible (Sum (Interval a)) where
  inv (Sum (l :.: u)) = Sum $ negate u :.: negate l
  inv _ = Sum Empty

instance (Ord a, Magma (Product a)) =>
  Magma (Product (Interval a)) where
  (Product (l :.: u)) `magma` (Product (l' :.: u')) =
    Product $ l'' :.: u'' where
    l'' = minimum [l * l', l * u', u * l', u * u']
    u'' = maximum [l * l', l * u', u * l', u * u']
  _ `magma` _ = Product Empty

instance (Ord a, Unital (Product a)) =>
  Unital (Product (Interval a)) where
  unit = Product $ one :.: one

instance (Ord a, Commutative (Product a)) =>
  Commutative (Product (Interval a))

instance (Ord a, Invertible (Product a)) =>
  Invertible (Product (Interval a)) where
  inv (Product (l :.: u)) = Product $ recip u :.: recip l
  inv _ = Product Empty

instance (Ord a, Associative (Product a)) =>
  Associative (Product (Interval a))

instance (Ord a, Multiplicative a) =>
  Absorbing (Product (Interval a)) where
  absorb = Product $ zero' :.: zero'

instance (Ord a, Distributive a) => Distributive (Interval a)

instance (Ord a, IntegralDomain a) => IntegralDomain (Interval a)

instance (Ord a, Field a) => Field (Interval a)

instance (Ord a, UpperBoundedField a) => UpperBoundedField (Interval a) where
  isNaN (l :.: u) = isNaN l || isNaN u
  isNaN Empty = True

instance (Ord a, LowerBoundedField a) => LowerBoundedField (Interval a)

instance (Ord a, ExpField a) => ExpField (Interval a) where
  exp = increasing exp
  log = increasing log

instance
  ( BoundedField a
  , QuotientField a Integer
  , FromInteger a
  , Ord a
  , TrigField a
  ) => TrigField (Interval a) where

  pi = pi :.: pi

  cos Empty = Empty
  cos (l :.: u)
    | width t >= pi = (-1) :.: 1
    | inf t >= pi = - cos (t - pi)
    | sup t <= pi = decreasing cos t
    | sup t <= 2 * pi = (-1) :.: cos ((pi * 2 - sup t) `min` inf t)
    | otherwise = (-1) :.: 1
    where
      tl = mod' l (pi * 2)
      tu = mod' u (pi * 2)
      t = tl :.: tu
      mod' a b = a - q * b
        where
          q = fromIntegral (truncate (a / b) :: Integer)
  sin x = cos (x - pi / 2)

  asin Empty = Empty
  asin (a :.: b)
    | b < -1 || a > 1 = Empty
    | otherwise =
      (bool (asin a) (- pi / 2) (a <= -1))
      :.:
      (bool (asin b) (- pi / 2) (b >= 1))

  acos Empty = Empty
  acos (a :.: b)
    | b < -1 || a > 1 = Empty
    | otherwise =
      (bool (acos b) zero (b >= 1))
      :.:
      (bool (acos b) pi (a < -1))

  atan = increasing atan
  sinh = increasing sinh

  cosh Empty = Empty
  cosh x@(a :.: b)
    | b < 0  = decreasing cosh x
    | a >= 0 = increasing cosh x
    | otherwise  = zero :.: (cosh $ bool b a (-a > b))

  tanh = increasing tanh

  asinh = increasing asinh

  acosh Empty = Empty
  acosh (a :.: b)
    | b < 1 = Empty
    | otherwise = lo :.: (acosh b)
    where lo | a <= 1 = 0
             | otherwise = acosh a

  atanh Empty = Empty
  atanh (a :.: b)
    | b < -1 || a > 1 = Empty
    | otherwise =
      (bool (atanh a) negInfinity (a <= - 1))
      :.:
      (bool (atanh b) infinity (b >= 1))

instance (Ord a, Integral a) => Integral (Interval a) where
  divMod (l :.: u) (l' :.: u') = (ld :.: ud, reg (lm :.: um)) where
    (ld, lm) = divMod l l'
    (ud, um) = divMod u u'
  divMod _ _ = (Empty, Empty)

  quotRem (l :.: u) (l' :.: u') = (ld :.: ud, reg (lm :.: um)) where
    (ld, lm) = quotRem l l'
    (ud, um) = quotRem u u'
  quotRem _ _ = (Empty, Empty)

instance (FromInteger a) => FromInteger (Interval a) where
  fromInteger a = fromInteger a :.: fromInteger a

instance (Unital (Sum a), Invertible (Sum a), Ord a, Signed a) => Signed (Interval a) where
  sign = increasing sign
  abs x@(a :.: b)
    | a >= zero = x
    | b <= zero = negate x
    | otherwise = zero :.: max (- a) b
  abs Empty = Empty

instance (UpperBoundedField a, Signed a, Ord a) => Metric (Interval a) a where
  distanceL1 a b = inf . abs $ (a - b)
  distanceL2 a b = inf . abs $ (a - b)
  distanceLp _ a b = inf . abs $ (a - b)

-- | The infimum (lower bound) of an interval
--
-- >>> inf (1.0 ... 20.0)
-- 1.0
--
inf :: (UpperBoundedField a) => Interval a -> a
inf (a :.: _) = a
inf _ = nan

-- | The supremum (upper bound) of an interval
--
-- >>> sup (1.0 ... 20.0)
-- 20.0
--
sup :: (UpperBoundedField a) => Interval a -> a
sup (_ :.: b) = b
sup _ = nan

-- | Calculate the width of an interval.
--
-- >>> width (1 :.: 20)
-- 19
--
width :: (UpperBoundedField a) => Interval a -> a
width (a :.: b) = b - a
width _ = nan

-- | lift a monotone increasing function over a given interval
increasing :: (a -> b) -> Interval a -> Interval b
increasing f (a :.: b) = (f a) :.: (f b)
increasing _ _ = Empty

-- | lift a monotone increasing function over a given interval
decreasing :: (a -> b) -> Interval a -> Interval b
decreasing f (a :.: b) = (f b) :.: (f a)
decreasing _ _ = Empty

reg :: (Ord a) => Interval a -> Interval a
reg (l :.: u)
  | u >= l = l :.: u
  | otherwise = u :.: l
reg _ = Empty

truncate :: (Ord a, QuotientField a b) => a -> b
truncate a = bool (ceiling a) (floor a) (a >= zero)
