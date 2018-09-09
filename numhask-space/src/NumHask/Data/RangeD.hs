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
module NumHask.Data.RangeD
  ( RangeD(..)
  , normalise
  ) where

import NumHask.Analysis.Space
import NumHask.Data.Range
import NumHask.Algebra.Abstract
import NumHask.Analysis.Metric
import Data.Bool (bool)
import GHC.Generics (Generic)
import Prelude (Eq(..), Ord(..), Show, Read, Integer, Bool(..), Foldable, Functor, Traversable(..), Applicative, pure, (<*>), (.), ($), max, otherwise, (||), (&&), not, fmap, (++), (<$>), undefined, Semigroup(..), Monoid(..), zipWith, drop, show, reverse)
import Data.List (sortBy, foldl')
import Data.Ord (comparing)

newtype RangeD a = RangeD [Range a]
  deriving (Eq, Generic, Show, Functor, Foldable, Traversable)

normalise :: (Ord a, Lattice a, Subtractive a) =>
    RangeD a -> RangeD a
normalise (RangeD rs) = RangeD $ reverse $ foldl' step [] (sortBy (comparing lower) rs)
  where
    step [] a = [a]
    step (x:xs) a = (a `unify` x) <> xs

    unify a b = bool (bool [a,b] [b,a] (lower a < lower b)) [a + b] (not $ a `disjoint` b)

instance (Ord a, Lattice a, Subtractive a) => Additive (RangeD a) where
    (RangeD l0) + (RangeD l1) = normalise $ RangeD $ l0 <> l1
    zero = RangeD []

instance (Divisive a, Ord a, Lattice a, Subtractive a) => Subtractive (RangeD a) where
    negate (RangeD rs) = normalise $ RangeD $ negate <$> rs

instance (Ord a, Lattice a, Subtractive a, Eq a, Multiplicative a) => Multiplicative (RangeD a) where
    (RangeD a) * (RangeD b) = normalise $ RangeD $ (*) <$> a <*> b
    one = RangeD [one]

instance (Multiplicative a, BoundedLattice a, Epsilon a, Ord a, Subtractive a, Divisive a) => Divisive (RangeD a) where
    recip (RangeD rs) = normalise $ RangeD $ recip <$> rs

