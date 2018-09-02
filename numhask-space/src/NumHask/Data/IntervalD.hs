{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif

-- | representation of a possibly discontinuous interval
module NumHask.Data.IntervalD
  ( IntervalD(..)
  , normalise
  ) where

import NumHask.Analysis.Space
import NumHask.Data.Interval
import NumHask.Algebra.Abstract
import NumHask.Algebra.Abstract.Lattice
import NumHask.Analysis.Metric
import Data.Bool (bool)
import GHC.Generics (Generic)
import Prelude (Eq(..), Ord(..), Show, Read, Integer, Bool(..), Foldable, Functor, Traversable(..), Applicative, pure, (<*>), (.), ($), max, otherwise, (||), (&&), not, fmap, (++), (<$>), undefined, Semigroup(..), Monoid(..), zipWith, drop, show, reverse)
import Data.List (sortBy, foldl')
import Data.Ord (comparing)

newtype IntervalD a = IntervalD [Interval a]
  deriving (Eq, Generic, Show, Functor, Foldable, Traversable)

normalise :: (Ord a, Lattice a, Subtractive a) =>
    IntervalD a -> IntervalD a
normalise (IntervalD rs) = IntervalD $ reverse $ foldl' step [] (sortBy (comparing lower) rs)
  where
    step [] a = [a]
    step (x:xs) a = (a `unify` x) <> xs

    unify a b = bool (bool [a,b] [b,a] (lower a < lower b)) [a + b] (not $ a `disjoint` b)

instance (Ord a, Lattice a, Subtractive a) => Additive (IntervalD a) where
    (IntervalD l0) + (IntervalD l1) = normalise $ IntervalD $ l0 <> l1
    zero = IntervalD []

instance (Divisive a, Ord a, Lattice a, Subtractive a) => Subtractive (IntervalD a) where
    negate (IntervalD rs) = normalise $ IntervalD $ negate <$> rs

instance (Ord a, Lattice a, Subtractive a, Eq a, Multiplicative a) => Multiplicative (IntervalD a) where
    (IntervalD a) * (IntervalD b) = normalise $ IntervalD $ (*) <$> a <*> b
    one = IntervalD [one]

instance (Multiplicative a, LowerBoundedField a, UpperBoundedField a, Epsilon a, Ord a, Lattice a, Subtractive a) => Divisive (IntervalD a) where
    recip (IntervalD rs) = normalise $ IntervalD $ recip <$> rs

