{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif

-- | a two-dimensional plane, implemented as a composite of a 'Pair' of 'Range's.
module NumHask.Data.Rect
  ( Rect(..)
  , pattern Rect
  , pattern Ranges
  , corners
  , projectRect
  ) where

import Data.Bool (bool)
import GHC.Exts
import GHC.Generics (Generic)
import Data.Distributive
import Data.Functor.Compose
import Data.Functor.Rep
import NumHask.Data.Pair
import Prelude (Eq(..), Show(..), Bool(..), Foldable(..), Functor, Traversable(..), Applicative, (.), (&&), fmap, (<$>), Semigroup(..), Monoid(..))
import NumHask.Data.Range
import NumHask.Data.Integral
import NumHask.Analysis.Space
import NumHask.Algebra.Abstract
import NumHask.Analysis.Metric
import NumHask.Algebra.Linear.Hadamard

-- $setup
-- >>> :set -XNoImplicitPrelude

-- | a 'Pair' of 'Ranges' that form a rectangle in what is often thought of as the XY plane.
--
-- >>> let a = Rect (-1) 1 (-2) 4
-- >>> a
-- Rect -1 1 -2 4
-- >>> let (Ranges x y) = a
-- >>> x
-- Range -1 1
-- >>> y
-- Range -2 4
-- >>> fmap (+1) (Rect 1 2 3 4)
-- Rect 2 3 4 5
-- >>> one :: Rect Double
-- Rect -0.5 0.5 -0.5 0.5
-- >>> zero :: Rect Double
-- Rect Infinity -Infinity Infinity -Infinity
--
-- as a Field instance
--
-- >>> Rect 0 1 2 3 + zero
-- Rect 0.0 1.0 2.0 3.0
-- >>> Rect 0 1 (-2) (-1) + Rect 2 3 (-5) 3
-- Rect 0.0 3.0 -5.0 3.0
-- >>> Rect 1 1 1 1 - one
-- Rect 0.5 1.0 0.5 1.0
-- >>> Rect 0 1 0 1 * one
-- Rect 0.0 1.0 0.0 1.0
-- >>> Rect 0 1 0 1 / one
-- Rect 0.0 1.0 0.0 1.0
-- >>> singleton (Pair 1.0 2.0) :: Rect Double
-- Rect 1.0 1.0 2.0 2.0
-- >>> abs (Rect 1 0 1 0)
-- Rect 0.0 1.0 0.0 1.0
-- >>> sign (Rect 1 0 1 0) == negate one
-- True
--
-- as a Space instance
--
-- >>> project (Rect 0 1 (-1) 0) (Rect 1 4 10 0) (Pair 0.5 1)
-- Pair 2.5 -10.0
-- >>> gridSpace (Rect 0 10 0 1) (Pair 2 2)
-- [Rect 0.0 5.0 0.0 0.5,Rect 0.0 5.0 0.5 1.0,Rect 5.0 10.0 0.0 0.5,Rect 5.0 10.0 0.5 1.0]
-- >>> grid MidPos (Rect 0 10 0 1) (Pair 2 2)
-- [Pair 2.5 0.25,Pair 2.5 0.75,Pair 7.5 0.25,Pair 7.5 0.75]
newtype Rect a =
  Rect' (Compose Pair Hull a)
  deriving (Eq, Functor, Applicative, Foldable, Traversable,
            Generic)

-- | pattern of Rect lowerx upperx lowery uppery
pattern Rect :: a -> a -> a -> a -> Rect a
pattern Rect a b c d = Rect' (Compose (Pair (Hull a b) (Hull c d)))
{-# COMPLETE Rect#-}

-- | pattern of Ranges xrange yrange
pattern Ranges :: Hull a -> Hull a -> Rect a
pattern Ranges a b = Rect' (Compose (Pair a b))
{-# COMPLETE Ranges#-}

instance (Show a) => Show (Rect a) where
  show (Rect a b c d) =
    "Rect " <> show a <> " " <> show b <> " " <> show c <> " " <> show d

instance Data.Distributive.Distributive Rect where
  collect f x =
    Rect (getA . f <$> x) (getB . f <$> x) (getC . f <$> x) (getD . f <$> x)
    where
      getA (Rect a _ _ _) = a
      getB (Rect _ b _ _) = b
      getC (Rect _ _ c _) = c
      getD (Rect _ _ _ d) = d
 
instance Representable Rect where
  type Rep Rect = (Bool, Bool)
  tabulate f =
    Rect (f (False, False)) (f (False, True)) (f (True, False)) (f (True, True))
  index (Rect a _ _ _) (False, False) = a
  index (Rect _ b _ _) (False, True) = b
  index (Rect _ _ c _) (True, False) = c
  index (Rect _ _ _ d) (True, True) = d

instance (HasRange a) => Semigroup (Rect a) where
  (<>) (Ranges x y) (Ranges x' y') = Ranges (x `union` x') (y `union` y')

type RectField a = (HasRange a, UpperBoundedField a, LowerBoundedField a)

instance (RectField a) => Monoid (Rect a) where
  mempty = Ranges m m
    where
      m = Hull infinity negInfinity

instance (HasRange a) => Space (Rect a) where
  type Element (Rect a) = Pair a

  union (Ranges a b) (Ranges c d) = Ranges (a `union` c) (b `union` d)
  -- intersection (Ranges a b) (Ranges c d) = Ranges (a `intersection` c) (b `intersection` d)

  lower (Rect l0 _ l1 _) = Pair l0 l1
  upper (Rect _ u0 _ u1) = Pair u0 u1

  singleton (Pair x y) = Rect x x y y

instance (HasRange a, Field a, FromInteger a) => FieldSpace (Rect a) where
    type Grid (Rect a) = Pair Int

    grid o s n = (+ bool zero (step/(one+one)) (o==MidPos)) <$> posns
      where
      posns =
        (lower s +) . (step *) . fmap fromIntegral <$>
        [Pair x y | x <- [x0 .. x1], y <- [y0 .. y1]]
      step = (/) (width s) (fromIntegral <$> n)
      (Pair x0 y0, Pair x1 y1) =
        case o of
          OuterPos -> (zero, n)
          InnerPos -> (one, n - one)
          LowerPos -> (zero, n - one)
          UpperPos -> (one, n)
          MidPos -> (zero, n - one)

    gridSpace (Ranges rX rY) (Pair stepX stepY) =
      [ Rect x (x + sx) y (y + sy)
      | x <- grid LowerPos rX stepX
      , y <- grid LowerPos rY stepY
      ]
      where
        sx = width rX / fromIntegral stepX
        sy = width rY / fromIntegral stepY

instance (LowerBoundedField a, UpperBoundedField a, HasRange a, Additive a) =>
         Additive (Rect a) where
  (Ranges x0 y0) + (Ranges x1 y1) = Ranges (x0 + x1) (y0 + y1)
  zero = Ranges zero zero

instance (LowerBoundedField a, UpperBoundedField a, HasRange a, Subtractive a) =>
         Subtractive (Rect a) where
  negate (Ranges x y) = Ranges (negate x) (negate y)

instance (LowerBoundedField a, UpperBoundedField a, HasRange a, Field a, FromInteger a) =>
         Multiplicative (Rect a) where
  (Ranges x0 y0) * (Ranges x1 y1) =
    Ranges (x0 * x1) (y0 * y1)
  one = Ranges one one

instance (LowerBoundedField a, UpperBoundedField a, HasRange a, Field a, Divisive a, FromInteger a) =>
         Divisive (Rect a) where
  recip (Ranges x y) = Ranges (recip x) (recip y)

instance (LowerBoundedField a, UpperBoundedField a, HasRange a, Field a, FromInteger a) =>
         Signed (Rect a) where
  sign (Ranges l u) = Ranges (sign l) (sign u)
  abs (Ranges l u) = Ranges (sign l * l) (sign u * u)

instance (JoinSemiLattice a) => JoinSemiLattice (Rect a) where
  (\/) = liftR2 (\/)

instance (MeetSemiLattice a) => MeetSemiLattice (Rect a) where
  (/\) = liftR2 (/\)

instance (LowerBoundedField a, UpperBoundedField a, HasRange a, Epsilon a) => Epsilon (Rect a) where
    epsilon = Ranges epsilon epsilon
    nearZero (Ranges a b) = nearZero a && nearZero b

-- | create a list of pairs representing the lower left and upper right cormners of a rectangle.
corners :: (HasRange a) => Rect a -> [Pair a]
corners r = [lower r, upper r]

-- | project a Rect from an old range to a new one
projectRect ::
     (HasRange a, Field a, FromInteger a)
  => Rect a
  -> Rect a
  -> Rect a
  -> Rect a
projectRect r0 r1 (Rect a b c d) = Rect a' b' c' d'
  where
    (Pair a' c') = project r0 r1 (Pair a c)
    (Pair b' d') = project r0 r1 (Pair b d)

instance (Multiplicative a) => HadamardMultiplication Rect a where
    (.*.) = liftR2 (*)
instance (Divisive a) => HadamardDivision Rect a where
    (./.) = liftR2 (/)

instance (Additive a) => AdditiveAction Rect a where
    (.+) r s = fmap (s+) r
    (+.) s = fmap (s+)
instance (Subtractive a) => SubtractiveAction Rect a where
    (.-) r s = fmap (\x -> x - s) r
    (-.) s = fmap (\x -> x - s)
instance (Multiplicative a) => MultiplicativeAction Rect a where
    (.*) r s = fmap (s*) r
    (*.) s = fmap (s*)
instance (Divisive a) => DivisiveAction Rect a where
    (./) r s = fmap (/ s) r
    (/.) s = fmap (/ s)

