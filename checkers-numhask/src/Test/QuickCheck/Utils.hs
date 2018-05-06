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



-- * Associative
isAssociativeBy :: (Show a, Testable prop)
                => (a -> a -> prop) -> Gen a -> (a -> a -> a) -> Property
isAssociativeBy (=~=) src (#) =
        forAll src $ \ a ->
        forAll src $ \ b ->
        forAll src $ \ c ->
        ((a # b) # c) =~= (a # (b # c))

isAssociative :: (Arbitrary a, Show a, Eq a) => (a -> a -> a) -> Property
isAssociative = isAssociativeBy (==) arbitrary

-- * Commutative
isCommutativeBy :: (Show a,Testable prop)
               => (b -> b -> prop) -> Gen a -> (a -> a -> b) -> Property
isCommutativeBy (=~=) src (#) =
        forAll src $ \ a ->
        forAll src $ \ b ->
        (a # b) =~= (b # a)

isCommutative :: (Arbitrary a, Show a, Eq b) => (a -> a -> b) -> Property
isCommutative = isCommutativeBy (==) arbitrary


-- * Identity

-- | Right identity z 
--
-- a `op` z == a
rightIdentity
  :: (Show a, Eq a) => t -> (a -> t -> a) -> Gen a -> Property
rightIdentity z op gen = forAll gen $ \a ->
  a `op` z == a

-- | Left identity z 
--
-- z `op` a == a
leftIdentity
  :: (Show a, Eq a) => t -> (t -> a -> a) -> Gen a -> Property
leftIdentity z op gen = forAll gen $ \a ->
  z `op` a == a

-- | Identity z
--
-- z `op` a == a `op` z == a
identity :: (Show a, Eq a) => a -> (a -> a -> a) -> Gen a -> Property
identity z op gen = conjoin [
    leftIdentity z op gen
  , rightIdentity z op gen
                            ]

hasIdentity :: (Show a, Eq a, Arbitrary a) => a -> (a -> a -> a) -> Property
hasIdentity z op = identity z op arbitrary                    


-- * Ordering
isTotalOrder :: (Arbitrary a, Show a, Ord a) => a -> a -> Property
isTotalOrder x y =
    classify (x > y)  "less than" $
    classify (x == y) "equals" $
    classify (x < y)  "greater than" $
    x < y || x == y || x > y


-- * Generic combinators

-- | Combinator for a property relating two elements
binary :: (Show t, Testable prop) => Gen t -> (t -> t -> prop) -> Property
binary gen rel = forAll gen $ \a ->
  forAll gen $ \b -> rel a b

binary' :: (Show t, Testable prop, Arbitrary t) => (t -> t -> prop) -> Property
binary' = binary arbitrary

-- | Combinator for a property relating three elements
ternary :: (Show t, Testable prop) =>
     Gen t -> (t -> t -> t -> prop) -> Property
ternary gen rel = forAll gen $ \a ->
  forAll gen $ \b ->
    forAll gen $ \c -> rel a b c
