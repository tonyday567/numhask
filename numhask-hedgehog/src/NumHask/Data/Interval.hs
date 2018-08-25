{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Interval
module NumHask.Data.Interval
  ( Interval(..)
  , CanInterval(..)
  , eps
  , whole
  , emptyInterval
  , singletonInterval
  , width
  , lower
  , upper
  , above
  , below
  , increasing
  , decreasing
  )
where

import Data.Data (Data)
import GHC.Generics (Generic, Generic1)
import NumHask.Prelude as P

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

class CanInterval a where
  infix 3 ...
  (...) :: a -> a -> Interval a
  (...) a b = interval (a :| [b])

  infixl 6 +/-
  (+/-) :: (Subtractive a) => a -> a -> Interval a
  a +/- b = a - b ... a + b

  -- | is a number contained within the interval
  infix 5 =.=
  (=.=) :: a -> Interval a -> Bool
  default (=.=) :: (Ord a) => a -> Interval a -> Bool
  (=.=) a (I l u) = l <= a && u >= a
  (=.=) a (S s) = a == s
  (=.=) _ Empty = False

  -- | this differs from minimum in that the algorithm can produce a number not in the list
  lowest :: NonEmpty a -> a
  default lowest :: (Ord a) => NonEmpty a -> a
  lowest = minimum

  -- | this differs from maximum in that the algorithm can produce a number not in the list
  highest :: NonEmpty a -> a
  default highest :: (Ord a) => NonEmpty a -> a
  highest = maximum

  interval :: NonEmpty a -> Interval a
  interval (x :| []) = S x
  interval xs = I (lowest xs) (highest xs)

instance CanInterval Float
instance CanInterval Double
instance (Ord a, CanInterval a) => CanInterval (LogField a)

instance (Ord a, Subtractive a, CanInterval a) => CanInterval (Complex a) where

  (a :+ b) =.= (I (la :+ lb) (ua :+ ub)) =
    a =.= I la ua &&
    b =.= I lb ub
  a =.= (S s) = a == s
  _ =.= Empty = False

  lowest xs = minimum (realPart <$> xs) :+ minimum (imagPart <$> xs)

  highest xs = maximum (realPart <$> xs) :+ maximum (imagPart <$> xs)

-- | Create a small interval around a number.
-- >>> eps one (0.0 :: Float)
--
--
eps :: (CanInterval a, Epsilon a, Subtractive a, Multiplicative a)
  => a -> a -> Interval a
eps accuracy a = a +/- (accuracy * a * epsilon)

whole :: (CanInterval a, LowerBoundedField a, UpperBoundedField a) => Interval a
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
singletonInterval = S

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

-- | A number is higher than an interval
--
-- >>> above (1.0 ... 20.0) 21.0
-- True
--
above :: (Ord a) => Interval a -> a -> Bool
above (I _ u) x = x > u
above (S s) x = x > s
above Empty _ = True

-- | A number is lower than an interval
--
-- >>> below (1.0 ... 20.0) 0.0
-- True
--
below :: (Ord a) => Interval a -> a -> Bool
below (I l _) x = x < l
below (S s) x = x < s
below Empty _ = True

-- | Calculate the width of an interval.
--
-- >>> width (1 ... 20)
-- 19
--
width :: (UpperBoundedField a, Subtractive a) => Interval a -> Maybe a
width (I a b) = Just (b - a)
width (S _) = Just zero
width Empty = Nothing

-- | lift a monotone increasing function over a given interval
increasing :: (CanInterval b) => (a -> b) -> Interval a -> Interval b
increasing f (I a b) = f a ... f b
increasing _ _ = Empty

-- | lift a monotone increasing function over a given interval
decreasing :: (CanInterval b) => (a -> b) -> Interval a -> Interval b
decreasing f (I a b) = f b ... f a
decreasing _ _ = Empty

instance (CanInterval a, Additive a) => Additive (Interval a) where
  (I l u) + (I l' u') = (l + l') ... (u + u')
  i + (S s) = fmap (s+) i
  (S s) + i = fmap (s+) i
  Empty + x = x
  x + Empty = x

  zero = S zero

instance (CanInterval a, Subtractive a, Divisive a) => Subtractive (Interval a) where
  negate (I l u) = negate u ... negate l
  negate (S s) = S $ negate s
  negate Empty = Empty

instance (CanInterval a, Multiplicative a) =>
  Multiplicative (Interval a) where
  (I l u) * (I l' u') =
    interval (l * l' :| [ l * u', u * l', u * u'])
  i * (S s) = fmap (s*) i
  (S s) * i = fmap (s*) i
  Empty *  x = x
  x * Empty = x

  one = one ... one

instance (CanInterval a, Eq a, Epsilon a, LowerBoundedField a, UpperBoundedField a, Divisive a) =>
  Divisive (Interval a) where
  recip i@(I l u)
    | zero =.= i && not (epsilon =.= i) = negInfinity ... recip l
    | zero =.= i && not (negate epsilon =.= i) = infinity ... recip l
    | zero =.= i = whole
    | otherwise = recip l ... recip u
  recip (S s) = S (recip s)
  recip Empty = Empty

instance (CanInterval a, P.Distributive a) => P.Distributive (Interval a)

instance (LowerBoundedField a, UpperBoundedField a, CanInterval a, Epsilon a) =>
  IntegralDomain (Interval a)

instance (LowerBoundedField a, UpperBoundedField a, CanInterval a, Epsilon a) => Field (Interval a)

instance (LowerBoundedField a, UpperBoundedField a, CanInterval a, Epsilon a) =>
  UpperBoundedField (Interval a) where
  isNaN (I l u) = isNaN l || isNaN u
  isNaN (S s) = isNaN s
  isNaN Empty = True

instance (UpperBoundedField a, CanInterval a, Epsilon a, LowerBoundedField a)
  => LowerBoundedField (Interval a)

instance (LowerBoundedField a, UpperBoundedField a, CanInterval a, Epsilon a, ExpField a) =>
  ExpField (Interval a) where
  exp = increasing exp
  log = increasing log

instance
  ( LowerBoundedField a, UpperBoundedField a
  , QuotientField a Integer
  , FromInteger a
  , CanInterval a
  , Ord a
  , TrigField a
  , Epsilon a
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
      bool (asin a) (- pi / 2) (a <= -1)
      ...
      bool (asin b) (- pi / 2) (b >= 1)

  acos Empty = Empty
  acos (S s) = S (acos s)
  acos (I a b)
    | b < -1 || a > 1 = Empty
    | otherwise =
      bool (acos b) zero (b >= 1)
      ...
      bool (acos b) pi (a < -1)

  atan = increasing atan
  sinh = increasing sinh

  cosh Empty = Empty
  cosh (S s) = S (cosh s)
  cosh x@(I a b)
    | b < 0  = decreasing cosh x
    | a >= 0 = increasing cosh x
    | otherwise  = zero ... cosh (bool b a (-a > b))

  tanh = increasing tanh

  asinh = increasing asinh

  acosh Empty = Empty
  acosh (S s) = S (acosh s)
  acosh (I a b)
    | b < 1 = Empty
    | otherwise = lo ... acosh b
    where lo | a <= 1 = 0
             | otherwise = acosh a

  atanh Empty = Empty
  atanh (S s) = S (atanh s)
  atanh (I a b)
    | b < -1 || a > 1 = Empty
    | otherwise =
      bool (atanh a) negInfinity (a <= - 1)
      ...
      bool (atanh b) infinity (b >= 1)

instance (Distributive a, Subtractive a, CanInterval a, Ord a, Integral a) =>
  Integral (Interval a) where
  divMod (I l u) (I l' u') = (ld ... ud, lm ... um) where
    (ld, lm) = divMod l l'
    (ud, um) = divMod u u'
  divMod _ _ = (Empty, Empty)

  quotRem (I l u) (I l' u') = (ld ... ud, lm ... um) where
    (ld, lm) = quotRem l l'
    (ud, um) = quotRem u u'
  quotRem _ _ = (Empty, Empty)

instance (CanInterval a, FromInteger a) => FromInteger (Interval a) where
  fromInteger a = fromInteger a ... fromInteger a

instance (CanInterval a, Ord a, Subtractive a, Divisive a, Signed a)
  => Signed (Interval a) where
  sign = increasing sign
  abs x@(I a b)
    | a >= zero = x
    | b <= zero = negate x
    | otherwise = zero ... max (- a) b
  abs Empty = Empty
  abs (S s) = S (abs s)

instance (UpperBoundedField a, Signed a, Subtractive a, CanInterval a, Ord a)
  => Metric (Interval a) (Maybe a) where
  distanceL1 a b = lower . abs $ (a - b)
  distanceL2 a b = lower . abs $ (a - b)
  distanceLp _ a b = lower . abs $ (a - b)
