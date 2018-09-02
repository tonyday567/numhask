{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | A continuous number Interval
module NumHask.Data.Interval
  ( Interval(..)
  , gridSensible
  )
where

import Data.Bool (bool)
import Data.Data (Data)
import Data.Functor.Apply (Apply(..))
import Data.Functor.Classes
import Data.Semigroup.Foldable (Foldable1(..))
import Data.Semigroup.Traversable (Traversable1(..))
import GHC.Exts
import GHC.Generics (Generic, Generic1)
import NumHask.Algebra.Abstract as A
import NumHask.Analysis.Metric
import NumHask.Analysis.Space as S
import NumHask.Data.Integral
import NumHask.Data.Rational
import NumHask.Exception
import Prelude (Eq(..), Ord(..), Show, Read, Integer, Bool(..), Foldable, Functor, Traversable(..), Applicative, pure, (<*>), (.), ($), max, otherwise, (||), (&&), not, fmap, (++), (<$>), undefined, Semigroup(..), Monoid(..), zipWith, drop)

data Interval a =
  Interval !a !a | SingletonInterval !a | EmptyInterval
  deriving ( Eq
           , Show
           , Read
           , Data
           , Generic
           , Generic1
           , Functor
           , Foldable
           , Traversable
           , Foldable1
           )

instance Eq1 Interval where
    liftEq _ EmptyInterval EmptyInterval = True
    liftEq _ EmptyInterval _ = False
    liftEq _ _ EmptyInterval = False
    liftEq f (SingletonInterval s) (SingletonInterval s') = f s s'
    liftEq f (SingletonInterval s) (Interval a b) = f s a && f s b
    liftEq f (Interval a b) (SingletonInterval s) = f a s && f b s
    liftEq f (Interval a b) (Interval a' b') = f a a' && f b b'

instance Show1 Interval where
  liftShowsPrec _ _ _ EmptyInterval = (++) "Interval empty "
  liftShowsPrec sp _ d (SingletonInterval s) = showsUnaryWith sp "Interval" d s
  liftShowsPrec sp _ d (Interval a b) = showsBinaryWith sp sp "Interval" d a b

instance Apply Interval where
  EmptyInterval <.> _ = EmptyInterval
  _ <.> EmptyInterval = EmptyInterval
  SingletonInterval fs <.> x = fmap fs x
  (Interval fa fb) <.> (SingletonInterval s) = Interval (fa s) (fb s)
  (Interval fa fb) <.> (Interval a b) = Interval (fa a) (fb b)

instance Applicative Interval where
    pure = SingletonInterval
    (<*>) = (<.>)

instance Traversable1 Interval where
  traverse1 _ EmptyInterval = EmptyInterval `seq` undefined
  traverse1 f (SingletonInterval s) = SingletonInterval <$> f s
  traverse1 f (Interval a b) = Interval <$> f a <.> f b

instance (Eq a, Lattice a, Subtractive a) => Semigroup (Interval a) where
  (<>) = union

instance (Eq a, Lattice a, Subtractive a) => Monoid (Interval a) where
  mempty = EmptyInterval

instance (Eq a, Lattice a, Subtractive a) => Space (Interval a) where
  type Element (Interval a) = a

  lower (Interval l _) = l
  lower (SingletonInterval s) = s
  lower EmptyInterval = throw (NumHaskException "lower used on empty space")

  upper (Interval _ u) = u
  upper (SingletonInterval s) = s
  upper EmptyInterval = throw (NumHaskException "upper used on empty space")

  singleton = SingletonInterval
  nul = EmptyInterval
  isNul = (nul ==)

  union EmptyInterval b = b
  union a EmptyInterval = a
  union a b = Interval l u where
    l = lower a \/ lower b
    u = upper b /\ upper a

  intersection EmptyInterval _ = EmptyInterval
  intersection _ EmptyInterval = EmptyInterval
  intersection a b =
    case disjoint a b of
      True -> EmptyInterval
      False -> Interval l u where
        l = lower a /\ lower b
        u = upper a \/ upper b

instance (Eq a, FromInteger a, Lattice a, Subtractive a, Field a) => FieldSpace (Interval a) where
    type Grid (Interval a) = Int

    grid o s n = (+ bool zero (step/(one+one)) (o==MidPos)) <$> posns
      where
        posns = (lower s +) . (step *) . fromIntegral <$> [i0..i1]
        step = (/) (width s) (fromIntegral n)
        (i0,i1) = case o of
                    OuterPos -> (zero,n)
                    InnerPos -> (one,n - one)
                    LowerPos -> (zero,n - one)
                    UpperPos -> (one,n)
                    MidPos -> (zero,n - one)
    gridSpace r n = zipWith Interval ps (drop 1 ps)
      where
        ps = grid OuterPos r n

instance (Additive a, Space (Interval a)) => Additive (Interval a) where
  (Interval l u) + (Interval l' u') = (l + l') ... (u + u')
  i + (SingletonInterval s) = fmap (s+) i
  (SingletonInterval s) + i = fmap (s+) i
  EmptyInterval + x = x
  x + EmptyInterval = x

  zero = SingletonInterval zero

instance (Subtractive a, Divisive a, Space (Interval a)) => Subtractive (Interval a) where
  negate (Interval l u) = negate u ... negate l
  negate (SingletonInterval s) = SingletonInterval $ negate s
  negate EmptyInterval = EmptyInterval

instance (Multiplicative a, Space (Interval a)) =>
  Multiplicative (Interval a) where
  (Interval l u) * (Interval l' u') =
    space [l * l', l * u', u * l', u * u']
  i * (SingletonInterval s) = fmap (s*) i
  (SingletonInterval s) * i = fmap (s*) i
  EmptyInterval *  x = x
  x * EmptyInterval = x

  one = one ... one

instance (Eq a, Epsilon a, LowerBoundedField a, UpperBoundedField a, Divisive a, Space (Interval a)) =>
  Divisive (Interval a) where
  recip i@(Interval l u)
    | zero |.| i && not (epsilon |.| i) = negInfinity ... recip l
    | zero |.| i && not (negate epsilon |.| i) = infinity ... recip l
    | zero |.| i = whole
    | otherwise = recip l ... recip u
  recip (SingletonInterval s) = SingletonInterval (recip s)
  recip EmptyInterval = EmptyInterval

instance (A.Distributive a, Space (Interval a)) => A.Distributive (Interval a)

instance (LowerBoundedField a, UpperBoundedField a, Epsilon a, Space (Interval a)) =>
  IntegralDomain (Interval a)

instance (LowerBoundedField a, UpperBoundedField a, Epsilon a, Space (Interval a)) => Field (Interval a)

instance (LowerBoundedField a, UpperBoundedField a, Epsilon a, Space (Interval a)) =>
  UpperBoundedField (Interval a) where
  isNaN (Interval l u) = isNaN l || isNaN u
  isNaN (SingletonInterval s) = isNaN s
  isNaN EmptyInterval = True

instance (UpperBoundedField a, Epsilon a, LowerBoundedField a, Space (Interval a))
  => LowerBoundedField (Interval a)

instance (LowerBoundedField a, UpperBoundedField a, Epsilon a, ExpField a, Space (Interval a)) =>
  ExpField (Interval a) where
  exp = monotone exp
  log = monotone log

instance
  ( Lattice a
  , LowerBoundedField a
  , UpperBoundedField a
  , QuotientField a Integer
  , FromInteger a
  , Ord a
  , TrigField a
  , Epsilon a
  ) => TrigField (Interval a) where

  pi = SingletonInterval pi

  cos EmptyInterval = EmptyInterval
  cos (SingletonInterval s) = SingletonInterval (cos s)
  cos (Interval l u) = cos' t
    where
      tl = mod' l (pi * 2)
      tu = mod' u (pi * 2)
      t = tl ... tu
      mod' a b = a - q * b
        where
          q = fromIntegral (truncate (a / b) :: Integer)
      cos' (Interval lower' upper')
        | (upper' - lower') >= pi = (-1) ... 1
        | lower' >= pi = - cos (t - pi)
        | upper' <= pi = monotone cos t
        | upper' <= 2 * pi = (-1) ... cos ((pi * 2 - upper') `min` lower')
        | otherwise = (-1) ... 1
      cos' (SingletonInterval s)
        | s >= pi = SingletonInterval $ - cos (s - pi)
        | s <= pi = monotone cos (SingletonInterval s)
        | s <= 2 * pi = (-1) ... cos ((pi * 2 - s) `min` s)
        | otherwise = (-1) ... 1
      cos' EmptyInterval = EmptyInterval
  sin x = cos (x - pi / 2)

  asin EmptyInterval = EmptyInterval
  asin (SingletonInterval s) = SingletonInterval (asin s)
  asin (Interval a b)
    | b < -1 || a > 1 = EmptyInterval
    | otherwise =
      bool (asin a) (- pi / 2) (a <= -1)
      ...
      bool (asin b) (- pi / 2) (b >= 1)

  acos EmptyInterval = EmptyInterval
  acos (SingletonInterval s) = SingletonInterval (acos s)
  acos (Interval a b)
    | b < -1 || a > 1 = EmptyInterval
    | otherwise =
      bool (acos b) zero (b >= 1)
      ...
      bool (acos b) pi (a < -1)

  atan = monotone atan
  sinh = monotone sinh

  cosh EmptyInterval = EmptyInterval
  cosh (SingletonInterval s) = SingletonInterval (cosh s)
  cosh x@(Interval a b)
    | b < 0  = monotone cosh x
    | a >= 0 = monotone cosh x
    | otherwise  = zero ... cosh (bool b a (-a > b))

  tanh = monotone tanh

  asinh = monotone asinh

  acosh EmptyInterval = EmptyInterval
  acosh (SingletonInterval s) = SingletonInterval (acosh s)
  acosh (Interval a b)
    | b < 1 = EmptyInterval
    | otherwise = lo ... acosh b
    where lo | a <= 1 = 0
             | otherwise = acosh a

  atanh EmptyInterval = EmptyInterval
  atanh (SingletonInterval s) = SingletonInterval (atanh s)
  atanh (Interval a b)
    | b < -1 || a > 1 = EmptyInterval
    | otherwise =
      bool (atanh a) negInfinity (a <= - 1)
      ...
      bool (atanh b) infinity (b >= 1)

instance (Lattice a, A.Distributive a, Subtractive a, Ord a, Integral a) =>
  Integral (Interval a) where
  divMod (Interval l u) (Interval l' u') = (ld ... ud, lm ... um) where
    (ld, lm) = divMod l l'
    (ud, um) = divMod u u'
  divMod _ _ = (EmptyInterval, EmptyInterval)

  quotRem (Interval l u) (Interval l' u') = (ld ... ud, lm ... um) where
    (ld, lm) = quotRem l l'
    (ud, um) = quotRem u u'
  quotRem _ _ = (EmptyInterval, EmptyInterval)

instance (FromInteger a, Space (Interval a)) => FromInteger (Interval a) where
  fromInteger a = fromInteger a ... fromInteger a

instance (Ord a, Lattice a, Subtractive a, Divisive a, Signed a)
  => Signed (Interval a) where
  sign = monotone sign
  abs x@(Interval a b)
    | a >= zero = x
    | b <= zero = negate x
    | otherwise = zero ... max (- a) b
  abs EmptyInterval = EmptyInterval
  abs (SingletonInterval s) = SingletonInterval (abs s)

instance (Lattice a, UpperBoundedField a, Signed a, Subtractive a, Ord a)
  => Metric (Interval a) a where
  distanceL1 a b = lower . abs $ (a - b)
  distanceL2 a b = lower . abs $ (a - b)
  distanceLp _ a b = lower . abs $ (a - b)

binOp :: (Space (Interval a)) => (a -> a -> a) -> (Interval a -> Interval a -> Interval a)
binOp f (Interval al au) (Interval bl bu) = f al bl ... f au bu
binOp f (Interval l u) (SingletonInterval s) = f l s ... f u s
binOp f (SingletonInterval s) (Interval l u) = f l s ... f u s
binOp f (SingletonInterval s) (SingletonInterval s') = SingletonInterval (f s s')
binOp _ EmptyInterval i = i
binOp _ i EmptyInterval = i

instance (Eq a, Subtractive a, JoinSemiLattice a, MeetSemiLattice a) => JoinSemiLattice (Interval a) where
  (\/) = binOp (\/)

instance (Eq a, Subtractive a, JoinSemiLattice a, MeetSemiLattice a) => MeetSemiLattice (Interval a) where
  (/\) = binOp (/\)

instance (Ord a, Space (Interval a), Signed a, Divisive a, Epsilon a) => Epsilon (Interval a) where
  epsilon = zero +/- epsilon

  nearZero EmptyInterval = False
  nearZero (SingletonInterval s) = nearZero s
  nearZero (Interval l u) = nearZero l && nearZero u

stepSensible :: (Ord a, FromRatio a, FromInteger a, ExpField a, QuotientField a Integer) => Pos -> a -> Integer -> a
stepSensible tp span n =
    step + bool zero (step/two) (tp==MidPos)
  where
    step' = 10.0 ^^ (floor (logBase 10 (span/fromIntegral n)) :: Integer)
    err = fromIntegral n / span * step'
    step
      | err <= 0.15 = 10.0 * step'
      | err <= 0.35 = 5.0 * step'
      | err <= 0.75 = 2.0 * step'
      | otherwise = step'

gridSensible :: (Ord a, FromInteger a, FromRatio a, QuotientField a Integer, ExpField a) =>
    Pos -> Interval a -> Integer -> [a]
gridSensible _ EmptyInterval _ = []
gridSensible _ (SingletonInterval s) _ = [s]
gridSensible tp (Interval l u) n =
    (+ bool zero (step/two) (tp==MidPos)) <$> posns
  where
    posns = (first' +) . (step *) . fromIntegral <$> [i0..i1]
    span = u - l
    step = stepSensible tp span n
    first' = step * fromIntegral (ceiling (l/step) :: Integer)
    last' =  step * fromIntegral (floor   (u/step) :: Integer)
    n' = round ((last' - first')/step)
    (i0,i1) = case tp of
                OuterPos -> (0::Integer,n')
                InnerPos -> (1,n' - 1)
                LowerPos -> (0,n' - 1)
                UpperPos -> (1,n')
                MidPos -> (0,n' - 1)
