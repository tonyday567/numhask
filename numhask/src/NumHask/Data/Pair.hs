{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | A Pair is *the* classical higher-kinded number but there is no canon.
module NumHask.Data.Pair
  ( Pair(..)
  , pattern Pair
  ) where

import qualified Prelude as P
import Prelude (Foldable, Traversable, Applicative, Monad, Functor(..), Semigroup(..), Monoid(..), Bounded(..), Eq(..), (<$>), (<*>), (&&), (||))
import GHC.Generics (Generic)
import Data.Functor.Classes
import NumHask.Algebra.Abstract
import NumHask.Data.Integral
import NumHask.Analysis.Metric
import NumHask.Data.Rational
import Text.Show

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XFlexibleContexts
--

-- | A pair of a's, implemented as a tuple, but api represented as a Pair of a's.
--
-- >>> fmap (+1) (Pair 1 2)
-- Pair 2 3
-- >>> pure one :: Pair Int
-- Pair 1 1
-- >>> (*) <$> Pair 1 2 <*> pure 2
-- Pair 2 4
-- >>> foldr (++) [] (Pair [1,2] [3])
-- [1,2,3]
-- >>> Pair "a" "pair" `mappend` pure " " `mappend` Pair "string" "mappended"
-- Pair "a string" "pair mappended"
--
-- As a Ring and Field class
-- 
-- >>> Pair 0 1 + zero
-- Pair 0 1
-- >>> Pair 0 1 + Pair 2 3
-- Pair 2 4
-- >>> Pair 1 1 - one
-- Pair 0 0
-- >>> Pair 0 1 * one
-- Pair 0 1
-- >>> Pair 0.0 1.0 / one
-- Pair 0.0 1.0
-- >>> Pair 11 12 `mod` (pure 6)
-- Pair 5 0
--
-- As an action
--
-- >>> Pair 1 2 .+ 3
-- Pair 4 5
--
newtype Pair a =
  Pair' (a, a)
  deriving (Eq, Generic)

-- | the preferred pattern
pattern Pair :: a -> a -> Pair a
pattern Pair a b = Pair' (a,b)
{-# COMPLETE Pair#-}

instance (Show a) => Show (Pair a) where
  show (Pair a b) = "Pair " <> Text.Show.show a <> " " <> Text.Show.show b

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Eq1 Pair where
  liftEq f (Pair a b) (Pair c d) = f a c && f b d

instance Show1 Pair where
  liftShowsPrec sp _ d (Pair' (a, b)) = showsBinaryWith sp sp "Pair" d a b

instance Applicative Pair where
  pure a = Pair a a
  (Pair fa fb) <*> Pair a b = Pair (fa a) (fb b)

instance Monad Pair where
  Pair a b >>= f = Pair a' b'
    where
      Pair a' _ = f a
      Pair _ b' = f b

instance Foldable Pair where
  foldMap f (Pair a b) = f a `mappend` f b

instance Traversable Pair where
  traverse f (Pair a b) = Pair <$> f a <*> f b

instance (Semigroup a) => Semigroup (Pair a) where
  (Pair a0 b0) <> (Pair a1 b1) = Pair (a0 <> a1) (b0 <> b1)

instance (Semigroup a, Monoid a) => Monoid (Pair a) where
  mempty = Pair mempty mempty
  mappend = (<>)

instance (Bounded a) => Bounded (Pair a) where
  minBound = Pair minBound minBound
  maxBound = Pair maxBound maxBound

unaryOp :: (a -> a) -> (Pair a -> Pair a)
unaryOp f (Pair a b) = Pair (f a) (f b)

binOp :: (a -> a -> a) -> (Pair a -> Pair a -> Pair a)
binOp (#) (Pair a0 b0) (Pair a1 b1) = Pair (a0 # a1) (b0 # b1)

-- numeric heirarchy
instance (Additive a) => Additive (Pair a) where
  (Pair a0 b0) + (Pair a1 b1) = Pair (a0 + a1) (b0 + b1)
  zero = Pair zero zero

instance (Subtractive a) => Subtractive (Pair a) where
  negate = unaryOp negate

instance (Multiplicative a) => Multiplicative (Pair a) where
  (Pair a0 b0) * (Pair a1 b1) = Pair (a0 * a1) (b0 * b1)
  one = Pair one one

instance (Divisive a) => Divisive (Pair a) where
  recip = unaryOp recip

instance (Integral a) => Integral (Pair a) where
  (Pair a0 b0) `divMod` (Pair a1 b1) = (Pair da db, Pair ma mb)
    where
      (da, ma) = a0 `divMod` a1
      (db, mb) = b0 `divMod` b1
  (Pair a0 b0) `quotRem` (Pair a1 b1) = (Pair da db, Pair ma mb)
    where
      (da, ma) = a0 `quotRem` a1
      (db, mb) = b0 `quotRem` b1

instance (Signed a) => Signed (Pair a) where
  sign = unaryOp sign
  abs = unaryOp abs

instance (ExpField a, Normed a a) =>
         Normed (Pair a) a where
  normL1 (Pair a b) = normL1 a + normL1 b
  normL2 (Pair a b) = sqrt (a ** (one + one) + b ** (one + one))

instance (Subtractive a, Epsilon a) => Epsilon (Pair a) where
  epsilon = Pair epsilon epsilon
  nearZero (Pair a b) = nearZero a && nearZero b

instance (ExpField a, Subtractive a, Normed a a) => Metric (Pair a) a where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)

instance (Distributive a) => Distributive (Pair a)

instance (Field a) => Field (Pair a)
instance (IntegralDomain a) => IntegralDomain (Pair a)

instance (ExpField a) => ExpField (Pair a) where
  exp = unaryOp exp
  log = unaryOp log

instance (UpperBoundedField a) => UpperBoundedField (Pair a)

instance (LowerBoundedField a) => LowerBoundedField (Pair a)

type instance Actor (Pair a) = a

instance (Additive a) => AdditiveAction (Pair a) where
    (.+) r s = fmap (s+) r
    (+.) s r = fmap (s+) r
instance (Subtractive a) => SubtractiveAction (Pair a) where
    (.-) r s = fmap (\x -> x - s) r
    (-.) s r = fmap (\x -> x - s) r
instance (Multiplicative a) => MultiplicativeAction (Pair a) where
    (.*) r s = fmap (s*) r
    (*.) s r = fmap (s*) r
instance (Divisive a) => DivisiveAction (Pair a) where
    (./) r s = fmap (/ s) r
    (/.) s r = fmap (/ s) r

instance (JoinSemiLattice a) => JoinSemiLattice (Pair a) where
  (\/) = binOp (\/)

instance (MeetSemiLattice a) => MeetSemiLattice (Pair a) where
  (/\) = binOp (/\)

instance (BoundedJoinSemiLattice a) => BoundedJoinSemiLattice (Pair a) where
  bottom = Pair bottom bottom

instance (BoundedMeetSemiLattice a) => BoundedMeetSemiLattice (Pair a) where
  top = Pair top top

instance (FromIntegral a b) => FromIntegral (Pair a) b where
  fromIntegral_ x = P.pure (fromIntegral_ x)

instance (FromRatio a b) => FromRatio (Pair a) b where
  fromRatio x = P.pure (fromRatio x)

instance (Normed a a) =>
  Normed (Pair a) (Pair a) where
  normL1 = fmap normL1
  normL2 = fmap normL2

instance (Subtractive a, Normed a a) =>
  Metric (Pair a) (Pair a) where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)
