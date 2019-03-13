{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | An Space with no empty, a semigroup based on a convex hull union, and a monoid on a negative space.
module NumHask.Data.Range
  ( Range(..)
  , pattern Range
  , gridSensible
 ) where

import Data.Functor.Rep
import Data.Distributive as D
import Data.Bool (bool, not)
import Data.Functor.Apply (Apply(..))
import Data.Functor.Classes
import Data.Semigroup.Foldable (Foldable1(..))
import Data.Semigroup.Traversable (Traversable1(..))
import GHC.Exts
import GHC.Generics (Generic)
import NumHask.Algebra.Abstract as A
import NumHask.Analysis.Metric
import NumHask.Analysis.Space as S
import NumHask.Data.Integral
import NumHask.Data.Rational
import Prelude (Eq(..), Ord(..), Show(..), Integer, Bool(..), Foldable(..), Functor, Traversable(..), Applicative, pure, (<*>), (.), otherwise, (&&), fmap, (<$>), Semigroup(..), Monoid(..), zipWith, drop, filter, ($), id)

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XFlexibleContexts

-- | A continuous range over type a
--
-- >>> let a = Range (-1) 1
-- >>> a
-- Range -1 1
-- >>> fmap (+1) (Range 1 2)
-- Range 2 3
-- >>> one :: Range Double
-- Range -0.5 0.5
-- >>> zero :: Range Double
-- Range Infinity -Infinity

-- | as a Field instance
--
-- >>> Range 0 1 + zero
-- Range 0.0 1.0
-- >>> Range 0 1 + Range 2 3
-- Range 0.0 3.0
-- >>> Range 1 1 - one
-- Range 0.5 1.0
-- >>> Range 0 1 * one
-- Range 0.0 1.0
-- >>> Range 0 1 / one
-- Range 0.0 1.0
-- >>> abs (Range 1 0)
-- Range 0.0 1.0
-- >>> sign (Range 1 0) == negate one
-- True
--
-- Idempotent
--
-- >>> Range 0 2 + Range 0 2
-- Range 0.0 2.0
--
-- as a space instance
--
-- >>> NumHask.Space.project (Range 0 1) (Range 1 4) 0.5
-- 2.5
-- >>> NumHask.Space.grid NumHask.Space.OuterPos (Range 0 10) 5
-- [0.0,2.0,4.0,6.0,8.0,10.0]
-- >>> NumHask.Space.gridSpace (Range 0 1) 4
-- [Range 0.0 0.25,Range 0.25 0.5,Range 0.5 0.75,Range 0.75 1.0]
-- >>> gridSensible NumHask.Space.OuterPos (Range (-12.0) 23.0) 6
-- [-10.0,-5.0,0.0,5.0,10.0,15.0,20.0]

newtype Range a = Range' (a,a)
  deriving (Eq, Generic)

-- not sure if this is correct or needed
type role Range representational

-- | A tuple is the preferred concrete implementation of a Range, due to many libraries having substantial optimizations for tuples already (eg 'Vector').  'Pattern Synonyms' allow us to recover a constructor without the need for tuple syntax.
pattern Range :: a -> a -> Range a
pattern Range a b = Range' (a,b)
{-# COMPLETE Range#-}

instance (Show a) => Show (Range a) where
    show (Range a b) = "Range " <> show a <> " " <> show b

instance Eq1 Range where
    liftEq f (Range a b) (Range c d) = f a c && f b d

instance Show1 Range where
    liftShowsPrec sp _ d (Range' (a,b)) = showsBinaryWith sp sp "Range" d a b

instance Functor Range where
    fmap f (Range a b) = Range (f a) (f b)

instance Apply Range where
  Range fa fb <.> Range a b = Range (fa a) (fb b)

instance Applicative Range where
    pure a = Range a a
    (Range fa fb) <*> Range a b = Range (fa a) (fb b)

instance Foldable Range where
  foldMap f (Range a b) = f a `mappend` f b

instance Foldable1 Range

instance Traversable Range where
    traverse f (Range a b) = Range <$> f a <*> f b

instance Traversable1 Range where
    traverse1 f (Range a b) = Range <$> f a Data.Functor.Apply.<.> f b

instance D.Distributive Range where
  collect f x = Range (getL . f <$> x) (getR . f <$> x)
    where getL (Range l _) = l
          getR (Range _ r) = r

instance Representable Range where
  type Rep Range = Bool
  tabulate f = Range (f False) (f True)
  index (Range l _) False = l
  index (Range _ r) True = r

instance (JoinSemiLattice a) => JoinSemiLattice (Range a) where
  (\/) = liftR2 (\/)

instance (MeetSemiLattice a) => MeetSemiLattice (Range a) where
  (/\) = liftR2 (/\)

instance (BoundedLattice a) => BoundedJoinSemiLattice (Range a) where
  bottom = top >.< bottom

instance (BoundedLattice a) => BoundedMeetSemiLattice (Range a) where
  top = bottom >.< top

instance (Lattice a) => Space (Range a) where
  type Element (Range a) = a

  lower (Range l _) = l
  upper (Range _ u) = u

  (>.<) = Range

instance (Lattice a, Field a, Subtractive a, FromInteger a) => FieldSpace (Range a) where
    type Grid (Range a) = Int

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
    gridSpace r n = zipWith Range ps (drop 1 ps)
      where
        ps = grid OuterPos r n

-- | Monoid based on convex hull union
instance (BoundedLattice a) => Semigroup (Range a) where
  (<>) a b = getUnion (Union a <> Union b)

instance (BoundedLattice a) => Monoid (Range a) where
  mempty = getUnion mempty

-- | Numeric algebra based on Interval arithmetic
-- https://en.wikipedia.org/wiki/Interval_arithmetic
--
instance (Additive a, Lattice a) => Additive (Range a) where
  (Range l u) + (Range l' u') = space1 [l+l',u+u']
  zero = zero ... zero

instance (Subtractive a, Lattice a) => Subtractive (Range a) where
  negate (Range l u) = negate u ... negate l

instance (Multiplicative a, Lattice a) => Multiplicative (Range a) where
  (Range l u) * (Range l' u') =
    space1 [l * l', l * u', u * l', u * u']
  one = one ... one

instance (BoundedLattice a, Epsilon a, Divisive a) =>
  Divisive (Range a)
  where
  recip i@(Range l u)
    | zero |.| i && not (epsilon |.| i) = bottom ... recip l
    | zero |.| i && not (negate epsilon |.| i) = top ... recip l
    | zero |.| i = whole
    | otherwise = recip l ... recip u

instance (Multiplicative a, Subtractive a, Lattice a) => Signed (Range a) where
    sign (Range l u) = bool (negate one) one (u `joinLeq` l)
    abs (Range l u) = bool (u ... l) (l ... u) (u `joinLeq` l)

instance (FromInteger a, Lattice a) => FromInteger (Range a) where
    fromInteger x = fromInteger x ... fromInteger x

type instance Actor (Range a) = a

instance (Additive a) => AdditiveAction (Range a) where
    (.+) r s = fmap (s+) r
    (+.) s = fmap (s+)
instance (Subtractive a) => SubtractiveAction (Range a) where
    (.-) r s = fmap (\x -> x - s) r
    (-.) s = fmap (\x -> x - s)
instance (Multiplicative a) => MultiplicativeAction (Range a) where
    (.*) r s = fmap (s*) r
    (*.) s = fmap (s*)
instance (Divisive a) => DivisiveAction (Range a) where
    (./) r s = fmap (/ s) r
    (/.) s = fmap (/ s)

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

gridSensible :: (Ord a, JoinSemiLattice a, FromInteger a, FromRatio a, QuotientField a Integer, ExpField a, Epsilon a) =>
    Pos -> Bool -> Range a -> Integer -> [a]
gridSensible tp inside r@(Range l u) n =
    bool id (filter (`memberOf` r)) inside $
    (+ bool zero (step/two) (tp==MidPos)) <$> posns
  where
    posns = (first' +) . (step *) . fromIntegral <$> [i0..i1]
    span = u - l
    step = stepSensible tp span n
    first' = step * fromIntegral (floor (l/step + epsilon) :: Integer)
    last' =  step * fromIntegral (ceiling (u/step - epsilon) :: Integer)
    n' = round ((last' - first')/step)
    (i0,i1) = case tp of
                OuterPos -> (0::Integer,n')
                InnerPos -> (1,n' - 1)
                LowerPos -> (0,n' - 1)
                UpperPos -> (1,n')
                MidPos -> (0,n' - 1)
