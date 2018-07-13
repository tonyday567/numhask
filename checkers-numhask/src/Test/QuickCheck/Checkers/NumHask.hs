module Test.QuickCheck.Checkers.NumHask where

import Test.QuickCheck (Property, Gen, forAll, conjoin)
import Test.QuickCheck.Checkers

import NumHask.Algebra.Abstract

-- | Unital Magma
unital :: (Show a, Eq a, Unital a) => Gen a -> Property
unital gen =
  forAll gen $ \a -> conjoin [a `magma` unit `eq` a, unit `magma` a `eq` a]

-- * PLAYGROUND

-- -- funEq :: (Arbitrary t, Show t, Eq a) => (t -> a) -> (t -> a) -> Property
-- -- funEq f g = property $ \x -> f x == g x

-- funEq :: (Show t, Eq a) => Gen t -> (t -> a) -> (t -> a) -> Property
-- funEq gen f g =
--   forAll gen $ \x ->
--     f x `eq` g x

-- associative gen =
--   property $ \a ->
--     forAll (gen a)
