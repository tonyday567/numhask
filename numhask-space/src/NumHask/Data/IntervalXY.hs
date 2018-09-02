{-# LANGUAGE CPP #-}
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

-- | a two-dimensional Interval, implemented as a composite of a 'Pair' of 'Interval's.
module NumHask.Data.IntervalXY
  ( IntervalXY(..)
  , pattern XY
  , pattern Intervals
  , corners
  , projectIntervalXY
  ) where

import Data.Bool (bool)
import GHC.Exts
import GHC.Generics (Generic)
import Data.Functor.Compose
import NumHask.Data.Pair
import Prelude (Eq(..), Show, Foldable, Functor, Traversable(..), Applicative, (.), (&&), fmap, (<$>), Semigroup(..), Monoid(..), show)
import NumHask.Data.Interval
import NumHask.Analysis.Space
import NumHask.Algebra.Abstract
import NumHask.Data.Integral

newtype IntervalXY a =
  IntervalXY' (Compose Pair Interval a)
  deriving (Eq, Functor, Applicative, Foldable, Traversable,
            Generic)

-- | pattern of XY lowerx upperx lowery uppery
pattern XY :: a -> a -> a -> a -> IntervalXY a
pattern XY a b c d = IntervalXY' (Compose (Pair (Interval a b) (Interval c d)))
{-# COMPLETE XY#-}

-- | pattern of Ranges xrange yrange
pattern Intervals :: Interval a -> Interval a -> IntervalXY a
pattern Intervals a b = IntervalXY' (Compose (Pair a b))
{-# COMPLETE Intervals#-}

instance (Show a) => Show (IntervalXY a) where
  show (XY a b c d) =
    "XY " <> show a <> " " <> show b <> " " <> show c <> " " <> show d

instance (Eq a, Lattice a, Subtractive a) => Space (IntervalXY a) where
  type Element (IntervalXY a) = Pair a
  nul = Intervals nul nul
  isNul (Intervals x y) = isNul x && isNul y

  union (Intervals a b) (Intervals c d) = Intervals (a `union` c) (b `union` d)
  intersection (Intervals a b) (Intervals c d) = Intervals (a `intersection` c) (b `intersection` d)

  lower (XY l0 _ l1 _) = Pair l0 l1
  upper (XY _ u0 _ u1) = Pair u0 u1

  singleton (Pair x y) = XY x x y y


instance (Eq a, FromInteger a, Lattice a, Subtractive a, Field a) => FieldSpace (IntervalXY a) where
    type Grid (IntervalXY a) = Pair Int

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

    gridSpace (Intervals rX rY) (Pair stepX stepY) =
      [ XY x (x + sx) y (y + sy)
      | x <- grid LowerPos rX stepX
      , y <- grid LowerPos rY stepY
      ]
      where
        sx = width rX / fromIntegral stepX
        sy = width rY / fromIntegral stepY

instance (Eq a, Lattice a, Subtractive a) => Semigroup (IntervalXY a) where
  (<>) = union

instance (Eq a, Lattice a, Subtractive a) => Monoid (IntervalXY a) where
  mempty = nul
  mappend = (<>)

-- | create a list of pairs representing the lower left and upper right cormners of a rectangle.
corners :: (Eq a, Lattice a, Subtractive a) => IntervalXY a -> [Pair a]
corners r = [lower r, upper r]

-- | project a IntervalXY from an old range to a new one
projectIntervalXY ::
     (Field a, Eq a, Lattice a, Subtractive a, FromInteger a)
  => IntervalXY a
  -> IntervalXY a
  -> IntervalXY a
  -> IntervalXY a
projectIntervalXY r0 r1 (XY a b c d) = XY a' b' c' d'
  where
    (Pair a' c') = project r0 r1 (Pair a c)
    (Pair b' d') = project r0 r1 (Pair b d)

instance (Additive a) => AdditiveAction IntervalXY a where
    (.+) r s = fmap (s+) r
    (+.) s = fmap (s+)
instance (Subtractive a) => SubtractiveAction IntervalXY a where
    (.-) r s = fmap (\x -> x - s) r
    (-.) s = fmap (\x -> x - s)
instance (Multiplicative a) => MultiplicativeAction IntervalXY a where
    (.*) r s = fmap (s*) r
    (*.) s = fmap (s*)
instance (Divisive a) => DivisiveAction IntervalXY a where
    (./) r s = fmap (/ s) r
    (/.) s = fmap (/ s)

