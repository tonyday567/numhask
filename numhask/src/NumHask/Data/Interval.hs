{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Interval
module NumHask.Data.Interval
  ( Interval
  , Interval'(..)
  , whole
  , emptyInterval
  , singletonInterval
  , width
  , lower
  , upper
  , increasing
  , decreasing
  )
where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Algebra.Abstract.Module
import NumHask.Analysis.Metric
import NumHask.Data.Integral
import NumHask.Data.Complex

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
  , null
  )
import Data.Data (Data)
import GHC.Generics (Generic, Generic1)

data Interval a =
  I !a !a | S !a | Empty
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

class Interval' a where
  infix 3 ...
  (...) :: a -> a -> Interval a
  default (...) :: Ord a => a -> a -> Interval a
  (...) a b
    | a == b = S a
    | a < b = I a b
    | otherwise = I b a

  infixl 6 +/-
  (+/-) :: (Invertible (Sum a)) => a -> a -> Interval a
  a +/- b = a - b ... a + b

  -- | is a number contained within the interval
  member :: a -> Interval a -> Bool
  default member :: (Ord a) => a -> Interval a -> Bool
  member a (I l u) = l <= a && u >= a
  member a (S s) = a == s
  member _ Empty = False

instance Interval' Float
instance Interval' Double

instance (Ord a, Interval' a) => Interval' (Complex a) where

  (...) a@(ax :+ ay) b@(bx :+ by)
    | a == b = S a
    | otherwise = I x' y'
    where
      x' = min ax bx :+ min ay by
      y' = max ax bx :+ max ay by

  member (a :+ b) (I (la :+ lb) (ua :+ ub)) =
    member a (I la ua) &&
    member b (I lb ub)
  member a (S s) = a == s
  member _ Empty = False

  a +/- b = a - b ... a + b

whole :: (Interval' a, BoundedField a) => Interval a
whole = infinity ... negInfinity

-- | An empty interval
--
-- >>> empty
-- Empty
emptyInterval :: Interval a
emptyInterval = Empty

-- | A singleton point
--
-- >>> singletonInterval 1
-- 1 ... 1
singletonInterval :: a -> Interval a
singletonInterval s = S s

-- | The infimum (lower bound) of an interval
--
-- >>> lower (1.0 ... 20.0)
-- 1.0
--
lower :: Interval a -> Maybe a
lower (I a _) = Just a
lower (S s) = Just s
lower Empty = Nothing

-- | The supremum (upper bound) of an interval
--
-- >>> upper (1.0 ... 20.0)
-- 20.0
--
upper :: Interval a -> Maybe a
upper (I _ b) = Just b
upper (S s) = Just s
upper Empty = Nothing

-- | Calculate the width of an interval.
--
-- >>> width (1 ... 20)
-- 19
--
width :: (UpperBoundedField a) => Interval a -> Maybe a
width (I a b) = Just (b - a)
width (S _) = Just zero
width Empty = Nothing

-- | lift a monotone increasing function over a given interval
increasing :: (Interval' b) => (a -> b) -> Interval a -> Interval b
increasing f (I a b) = (f a) ... (f b)
increasing _ _ = Empty

-- | lift a monotone increasing function over a given interval
decreasing :: (Interval' b) => (a -> b) -> Interval a -> Interval b
decreasing f (I a b) = (f b) ... (f a)
decreasing _ _ = Empty

instance (Interval' a, Magma (Sum a)) => Magma (Sum (Interval a)) where
  (Sum (I l u)) `magma` (Sum (I l' u')) =
    Sum $ (l + l') ... (u + u')
  (Sum i) `magma` (Sum (S s)) = Sum $ i .+ s
  (Sum (S s)) `magma` (Sum i) = Sum $ s +. i
  (Sum Empty) `magma` x = x
  x `magma` (Sum Empty) = x

instance (Interval' a, Unital (Sum a)) => Unital (Sum (Interval a)) where
  unit = Sum (S zero)

instance (Interval' a, Associative (Sum a)) => Associative (Sum (Interval a))

instance (Interval' a, Commutative (Sum a)) => Commutative (Sum (Interval a))

instance (Interval' a, Invertible (Sum a)) => Invertible (Sum (Interval a)) where
  inv (Sum (I l u)) = Sum $ negate u ... negate l
  inv (Sum (S s)) = Sum $ S $ negate s
  inv (Sum Empty) = Sum Empty

instance {-# OVERLAPPABLE #-} (Ord a, Magma (Product a)) =>
  Magma (Product (Interval a)) where
  (Product (I l u)) `magma` (Product (I l' u')) =
    Product $ I l'' u'' where
    l'' = minimum [l * l', l * u', u * l', u * u']
    u'' = maximum [l * l', l * u', u * l', u * u']
  (Product i) `magma` (Product (S s)) = Product $ i .* s
  (Product (S s)) `magma` (Product i) = Product $ s *. i
  (Product Empty) `magma` x = x
  x `magma` (Product Empty) = x

instance
  ( Ord a
  , Magma (Product a)
  , Invertible (Sum a)
  ) =>
  Magma (Product (Interval (Complex a))) where
  (Product (I (lx :+ ly) (ux :+ uy))) `magma`
    (Product (I (lx' :+ ly') (ux' :+ uy'))) =
    Product $ I (lx'' :+ ly'') (ux'' :+ uy'') where
    (llx :+ lly) = (lx :+ ly) * (lx' :+ ly')
    (lux :+ luy) = (lx :+ ly) * (ux' :+ uy')
    (ulx :+ uly) = (ux :+ uy) * (lx' :+ ly')
    (uux :+ uuy) = (ux :+ uy) * (ux' :+ uy')
    lx'' = minimum [llx, lux, ulx, uux]
    ly'' = minimum [lly, luy, uly, uuy]
    ux'' = maximum [llx, lux, ulx, uux]
    uy'' = maximum [lly, luy, uly, uuy]
  (Product i) `magma` (Product (S s)) = Product $ i .* s
  (Product (S s)) `magma` (Product i) = Product $ s *. i
  (Product Empty) `magma` x = x
  x `magma` (Product Empty) = x

instance (Interval' a, Ord a, Unital (Product a)) =>
  Unital (Product (Interval a)) where
  unit = Product $ one ... one

instance (Ord a, Commutative (Product a)) =>
  Commutative (Product (Interval a))

instance {-# OVERLAPPABLE #-} (Interval' a, Ord a, BoundedField a, Invertible (Product a)) =>
  Invertible (Product (Interval a)) where
  inv (Product i@(I l u))
    | l < zero && u == zero = Product (negInfinity ... recip l)
    | l == zero && u > zero = Product (infinity ... recip l)
    | zero `member` i = Product whole
    | otherwise = Product (recip l ... recip u)
  inv (Product (S s)) = Product (S (recip s))
  inv (Product Empty) = Product Empty

instance (Interval' a, Ord a, BoundedField a, Invertible (Product a)) =>
  Invertible (Product (Interval (Complex a))) where
  inv (Product i@(I l@(la :+ lb) u@(ua :+ ub)))
    | la < zero && ua == zero = Product (negInfinity ... recip l)
    | lb < zero && ub == zero = Product (negInfinity ... recip l)
    | la == zero && ua > zero = Product (infinity ... recip l)
    | lb == zero && ub > zero = Product (infinity ... recip l)
    | zero `member` i = Product whole
    | otherwise = Product (recip l ... recip u)
  inv (Product (S s)) = Product (S (recip s))
  inv (Product Empty) = Product Empty

instance (Ord a, Associative (Product a)) =>
  Associative (Product (Interval a))

instance (Interval' a, Ord a, Multiplicative a) =>
  Absorbing (Product (Interval a)) where
  absorb = Product $ zero' ... zero'

instance (Interval' a, Ord a, Distributive a) => Distributive (Interval a)

instance (BoundedField a, Interval' a, Ord a) => IntegralDomain (Interval a)

instance (BoundedField a, Interval' a, Ord a) => Field (Interval a)

instance (BoundedField a, Interval' a, Ord a) => UpperBoundedField (Interval a) where
  isNaN (I l u) = isNaN l || isNaN u
  isNaN (S s) = isNaN s
  isNaN Empty = True

instance (BoundedField a, Interval' a, Ord a, LowerBoundedField a) => LowerBoundedField (Interval a)

instance (BoundedField a, Interval' a, Ord a, ExpField a) => ExpField (Interval a) where
  exp = increasing exp
  log = increasing log

instance
  ( BoundedField a
  , QuotientField a Integer
  , FromInteger a
  , Interval' a, Ord a
  , TrigField a
  ) => TrigField (Interval a) where

  pi = S pi

  cos Empty = Empty
  cos (S s) = S (cos s)
  cos (I l u) = cos' t
    where
      tl = mod' l (pi * 2)
      tu = mod' u (pi * 2)
      t = tl ... tu
      mod' a b = a - q * b
        where
          q = fromIntegral (truncate (a / b) :: Integer)
      cos' (I lower' upper')
        | (upper' - lower') >= pi = (-1) ... 1
        | lower' >= pi = - cos (t - pi)
        | upper' <= pi = decreasing cos t
        | upper' <= 2 * pi = (-1) ... cos ((pi * 2 - upper') `min` lower')
        | otherwise = (-1) ... 1
      cos' (S s)
        | s >= pi = S $ - cos (s - pi)
        | s <= pi = decreasing cos (S s)
        | s <= 2 * pi = (-1) ... cos ((pi * 2 - s) `min` s)
        | otherwise = (-1) ... 1
      cos' Empty = Empty
  sin x = cos (x - pi / 2)

  asin Empty = Empty
  asin (S s) = S (asin s)
  asin (I a b)
    | b < -1 || a > 1 = Empty
    | otherwise =
      (bool (asin a) (- pi / 2) (a <= -1))
      ...
      (bool (asin b) (- pi / 2) (b >= 1))

  acos Empty = Empty
  acos (S s) = S (acos s)
  acos (I a b)
    | b < -1 || a > 1 = Empty
    | otherwise =
      (bool (acos b) zero (b >= 1))
      ...
      (bool (acos b) pi (a < -1))

  atan = increasing atan
  sinh = increasing sinh

  cosh Empty = Empty
  cosh (S s) = S (cosh s)
  cosh x@(I a b)
    | b < 0  = decreasing cosh x
    | a >= 0 = increasing cosh x
    | otherwise  = zero ... (cosh $ bool b a (-a > b))

  tanh = increasing tanh

  asinh = increasing asinh

  acosh Empty = Empty
  acosh (S s) = S (acosh s)
  acosh (I a b)
    | b < 1 = Empty
    | otherwise = lo ... (acosh b)
    where lo | a <= 1 = 0
             | otherwise = acosh a

  atanh Empty = Empty
  atanh (S s) = S (atanh s)
  atanh (I a b)
    | b < -1 || a > 1 = Empty
    | otherwise =
      (bool (atanh a) negInfinity (a <= - 1))
      ...
      (bool (atanh b) infinity (b >= 1))

instance (Interval' a, Ord a, Integral a) => Integral (Interval a) where
  divMod (I l u) (I l' u') = (ld ... ud, lm ... um) where
    (ld, lm) = divMod l l'
    (ud, um) = divMod u u'
  divMod _ _ = (Empty, Empty)

  quotRem (I l u) (I l' u') = (ld ... ud, lm ... um) where
    (ld, lm) = quotRem l l'
    (ud, um) = quotRem u u'
  quotRem _ _ = (Empty, Empty)

instance (Interval' a, FromInteger a) => FromInteger (Interval a) where
  fromInteger a = fromInteger a ... fromInteger a

instance (Unital (Sum a), Invertible (Sum a), Interval' a, Ord a, Signed a) => Signed (Interval a) where
  sign = increasing sign
  abs x@(I a b)
    | a >= zero = x
    | b <= zero = negate x
    | otherwise = zero ... max (- a) b
  abs Empty = Empty
  abs (S s) = S (abs s)

instance (UpperBoundedField a, Signed a, Interval' a, Ord a) => Metric (Interval a) (Maybe a) where
  distanceL1 a b = lower . abs $ (a - b)
  distanceL2 a b = lower . abs $ (a - b)
  distanceLp _ a b = lower . abs $ (a - b)

instance (Magma (Sum a)) =>
  AdditiveModule Interval a where
  (.+) r s = fmap (s +) r
  (+.) s = fmap (s +)

instance (Invertible (Sum a)) =>
  AdditiveGroupModule Interval a where
  (.-) r s = fmap (s -) r
  (-.) s = fmap (s -)

instance (Magma (Product a)) =>
  Module Interval a where
  (.*) r s = fmap (s *) r
  (*.) s = fmap (s *)

instance (Invertible (Product a)) =>
  MultiplicativeGroupModule Interval a where
  (./) r s = fmap (s /) r
  (/.) s = fmap (s /)

