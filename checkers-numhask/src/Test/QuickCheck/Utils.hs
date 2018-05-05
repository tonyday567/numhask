-----------------------------------------------------------------------------
-- |
-- Module      :  Test.QuickCheck.Utils
-- Copyright   :  (c) Andy Gill 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- These are some general purpose utilities for use with QuickCheck.
--
-- Copied from Checkers, which in turn copied it from QuickCheck 1.2.0.0.  
-----------------------------------------------------------------------------
module Test.QuickCheck.Utils where

import Test.QuickCheck


isAssociativeBy :: (Show a, Testable prop)
                => (a -> a -> prop) -> Gen a -> (a -> a -> a) -> Property
isAssociativeBy (=~=) src (#) =
        forAll src $ \ a ->
        forAll src $ \ b ->
        forAll src $ \ c ->
        ((a # b) # c) =~= (a # (b # c))

isAssociative :: (Arbitrary a, Show a, Eq a) => (a -> a -> a) -> Property
isAssociative = isAssociativeBy (==) arbitrary

isCommutativeBy :: (Show a,Testable prop)
               => (b -> b -> prop) -> Gen a -> (a -> a -> b) -> Property
isCommutativeBy (=~=) src (#) =
        forAll src $ \ a ->
        forAll src $ \ b ->
        (a # b) =~= (b # a)

isCommutative :: (Arbitrary a, Show a, Eq b) => (a -> a -> b) -> Property
isCommutative = isCommutativeBy (==) arbitrary

isTotalOrder :: (Arbitrary a, Show a, Ord a) => a -> a -> Property
isTotalOrder x y =
    classify (x > y)  "less than" $
    classify (x == y) "equals" $
    classify (x < y)  "greater than" $
    x < y || x == y || x > y
