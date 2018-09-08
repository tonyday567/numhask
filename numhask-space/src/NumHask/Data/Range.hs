{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | An Interval with no empty, and a monoid based on the convex hull union.
module NumHask.Data.Range
  ( Range(..)
  , pattern Range
  , HasRange
  , range
  , Interval
  , pattern Interval
  , Hull
  , pattern Hull
  , gridSensible
 ) where

-- import NumHask.Prelude as P hiding (width, lower, upper, Space(..), mid, Pos(..), grid)
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
import NumHask.Exception
import Prelude (Eq(..), Ord(..), Show(..), Integer, Bool(..), Foldable(..), Functor, Traversable(..), Applicative, pure, (<*>), (.), otherwise, (||), (&&), fmap, (<$>), Semigroup(..), Monoid(..), zipWith, drop)

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

type HasRange a = (Eq a, JoinSemiLattice a, MeetSemiLattice a, Subtractive a)

instance (JoinSemiLattice a) => JoinSemiLattice (Range a) where
  (\/) = liftR2 (\/)

instance (MeetSemiLattice a) => MeetSemiLattice (Range a) where
  (/\) = liftR2 (/\)

range :: (JoinSemiLattice a, MeetSemiLattice a) => a -> a -> Range a
range a b = Range (a\/b) (a/\b)

instance (HasRange a) => Space (Range a) where
  type Element (Range a) = a

  lower (Range l _) = l
  upper (Range _ u) = u

  singleton = pure

  union (Range l u) (Range l' u') = Range (l \/ l') (u /\ u')

  intersection a b = Range l u where
      l = lower a /\ lower b
      u = upper a \/ upper b

  (>.<) = Range

instance (HasRange a, Field a, FromInteger a) => FieldSpace (Range a) where
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


-- | Interval is a wrapper for interval maths
-- https://en.wikipedia.org/wiki/Interval_arithmetic
--
newtype Interval a = Interval' (Range a)
  deriving
    ( Eq
    , Show
    , Functor
    -- , Space
    , JoinSemiLattice
    , MeetSemiLattice
    )

pattern Interval :: a -> a -> Interval a
pattern Interval a b <- Interval' (Range' (a,b))
{-# COMPLETE Interval #-}

instance (HasRange a) => Space (Interval a) where
  type Element (Interval a) = a

  lower (Interval l _) = l
  upper (Interval _ u) = u

  singleton a = Interval' (Range a a)

  union (Interval l u) (Interval l' u') = Interval' (Range (l \/ l') (u /\ u')) 

  intersection a b = Interval' (Range l u) where
      l = lower a /\ lower b
      u = upper a \/ upper b

  (>.<) a b = Interval' (Range a b)

instance (Additive a, HasRange a) => Additive (Interval a) where
  (Interval l u) + (Interval l' u') = space1 [l+l',u+u']
  zero = zero ... zero

instance (HasRange a) => Subtractive (Interval a) where
  negate (Interval l u) = negate u ... negate l

instance (HasRange a, Multiplicative a) => Multiplicative (Interval a) where
  (Interval l u) * (Interval l' u') =
    space1 [l * l', l * u', u * l', u * u']
  one = one ... one

instance (HasRange a, Epsilon a, UpperBoundedField a, LowerBoundedField a, Divisive a) =>
  Divisive (Interval a)
  where
  recip i@(Interval l u)
    | zero |.| i && not (epsilon |.| i) = negInfinity ... recip l
    | zero |.| i && not (negate epsilon |.| i) = infinity ... recip l
    | zero |.| i = whole
    | otherwise = recip l ... recip u

instance (LowerBoundedField a, HasRange a) => Signed (Interval a) where
    sign (Interval l u) = bool (negate one) one (u `joinLeq` l)
    abs (Interval l u) = bool (u ... l) (l ... u) (u `joinLeq` l)

instance (HasRange a, FromInteger a) => FromInteger (Interval a) where
    fromInteger x = fromInteger x ... fromInteger x

instance (Additive a) => AdditiveAction Interval a where
    (.+) r s = fmap (s+) r
    (+.) s = fmap (s+)
instance (Subtractive a) => SubtractiveAction Interval a where
    (.-) r s = fmap (\x -> x - s) r
    (-.) s = fmap (\x -> x - s)
instance (Multiplicative a) => MultiplicativeAction Interval a where
    (.*) r s = fmap (s*) r
    (*.) s = fmap (s*)
instance (Divisive a) => DivisiveAction Interval a where
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

gridSensible :: (Ord a, FromInteger a, FromRatio a, QuotientField a Integer, ExpField a) =>
    Pos -> Interval a -> Integer -> [a]
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

-- | Hull is a wrapper for convex hull maths
newtype Hull a = Hull' (Range a)
  deriving
    ( Eq
    , Eq1
    , Show
    , Functor
    , Foldable
    , Traversable
    -- , Space
    -- , FieldSpace
    , JoinSemiLattice
    , MeetSemiLattice
    , Applicative
    )

instance (HasRange a) => Space (Hull a) where
  type Element (Hull a) = a

  lower (Hull l _) = l
  upper (Hull _ u) = u

  singleton a = Hull' (Range a a)

  union (Hull l u) (Hull l' u') = Hull' (Range (l \/ l') (u /\ u')) 

  intersection a b = Hull' (Range l u) where
      l = lower a /\ lower b
      u = upper a \/ upper b

  (>.<) a b = Hull' (Range a b)

instance (HasRange a, Field a, FromInteger a) => FieldSpace (Hull a) where
    type Grid (Hull a) = Int

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
    gridSpace r n = zipWith Hull ps (drop 1 ps)
      where
        ps = grid OuterPos r n

instance D.Distributive Hull where
  collect f x = Hull (getL . f <$> x) (getR . f <$> x)
    where getL (Hull l _) = l
          getR (Hull _ r) = r

instance Representable Hull where
  type Rep Hull = Bool
  tabulate f = Hull (f False) (f True)
  index (Hull l _) False = l
  index (Hull _ r) True = r

-- not sure if this is correct or needed
type role Hull representational

pattern Hull :: a -> a -> Hull a
pattern Hull a b = Hull' (Range' (a, b))
{-# COMPLETE Hull #-}

instance (LowerBoundedField a, UpperBoundedField a, HasRange a) => Additive (Hull a) where
    (+) = union
    zero = Hull infinity negInfinity

-- | doesn't pass the group laws but convenient non-the-less
-- > negate (negate a) == a
instance (LowerBoundedField a, UpperBoundedField a, HasRange a) => Subtractive (Hull a) where
    negate (Hull a b) = Hull b a

-- | times may well be some sort of affine projection lurking under the hood
-- > width one = one
-- > mid zero = zero
instance (LowerBoundedField a, UpperBoundedField a, HasRange a, Field a) =>
  Multiplicative (Hull a) where
    a * b = bool (Hull (m - r/two) (m + r/two)) zero (a == zero || b == zero)
        where
          m = mid a + mid b
          r = width a * width b

    one = Hull (negate half) half

instance (LowerBoundedField a, UpperBoundedField a, HasRange a, Field a, FromInteger a, Divisive a) => Divisive (Hull a)
  where
    recip a = case width a == zero of
      True  -> throw (NumHaskException "reciprocating a zero-width range")
      False -> Hull (m - r/two) (m + r/two)
        where
          m = negate (mid a)
          r = recip (width a)

instance (LowerBoundedField a, UpperBoundedField a, HasRange a, Field a) => Signed (Hull a) where
  sign (Hull l u) = bool (Hull half (negate half)) one (u `joinLeq` l)
  abs (Hull l u) = bool (Hull u l) (Hull l u) (u `joinLeq` l)

instance (LowerBoundedField a, UpperBoundedField a, HasRange a, Epsilon a) => Epsilon (Hull a) where
  epsilon = zero +/- epsilon
  nearZero (Hull l u) = nearZero l && nearZero u
