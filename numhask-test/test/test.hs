{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | testing IEEE numbers is a special kind of hell, and one that I reserve for days when I can hardly think, so please forgive the horrible hackery contained within this file.
--
-- This suite sometimes fails, having been hand-crafty towards balancing reasonably approximate equality versus unbounded failure (given enough trials).
module Main where

import NumHask.Laws
import NumHask.Prelude
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.Tasty (TestTree, defaultMain, testGroup)

instance Arbitrary Natural where
  arbitrary = fromInteger . abs <$> arbitrary

instance Arbitrary Rational where
  arbitrary = reduce <$> (fromInteger <$> arbitrary) <*>
    (fromInteger <$> arbitrary `suchThat` (>zero))

instance (Signed a, Arbitrary a, ExpField a) => Arbitrary (LogField a) where
  arbitrary = logField . abs <$> arbitrary

instance (Arbitrary a) => Arbitrary (Complex a) where
  arbitrary = (:+) <$> arbitrary <*> arbitrary

main :: IO ()
main = defaultMain tests
 
tests :: TestTree
tests = testGroup
  "NumHask"
  [ testGroup "Float" $
    testLawOf ([] :: [Float]) ([] :: [(Float, Float)]) <$> fieldIntervalLaws
  , testGroup "Double" $
    testLawOf ([] :: [Double]) ([] :: [(Double, Double)]) <$> fieldIntervalLaws
  , testGroup "Complex Float" $
    testLawOf ([] :: [Complex Float]) ([] :: [(Complex Float, Complex Float)]) <$>
    complexIntervalLaws
  , testGroup "Integer" $
    testLawOf ([] :: [Integer]) ([] :: [(Integer, Integer)]) <$> integralsUnboundedLaws
  , testsNatural
  , testGroup "Int" $
    testLawOf ([] :: [Int]) ([] :: [(Int, Int)]) <$> integralsBoundedLaws
  , testGroup "Int8" $
    testLawOf ([] :: [Int8]) ([] :: [(Int8, Int8)]) <$> integralsBoundedLaws
  , testGroup "Int16" $
    testLawOf ([] :: [Int16]) ([] :: [(Int16, Int16)]) <$> integralsBoundedLaws
  , testGroup "Int32" $
    testLawOf ([] :: [Int32]) ([] :: [(Int32, Int32)]) <$> integralsBoundedLaws
  , testGroup "Int64" $
    testLawOf ([] :: [Int64]) ([] :: [(Int64, Int64)]) <$> integralsBoundedLaws
  , testGroup "Word" $
    testLawOf ([] :: [Word]) ([] :: [(Word, Word)]) <$> integralsBoundedLaws
  , testGroup "Word8" $
    testLawOf ([] :: [Word8]) ([] :: [(Word8, Word8)]) <$> integralsBoundedLaws
  , testGroup "Word16" $
    testLawOf ([] :: [Word16]) ([] :: [(Word16, Word16)]) <$> integralsBoundedLaws
  , testGroup "Word32" $
    testLawOf ([] :: [Word32]) ([] :: [(Word32, Word32)]) <$> integralsBoundedLaws
  , testGroup "Word64" $
    testLawOf ([] :: [Word64]) ([] :: [(Word64, Word64)]) <$> integralsBoundedLaws
  , testGroup "Word" $
    testLawOf ([] :: [Word]) ([] :: [(Word, Word)]) <$> integralsBoundedLaws
  -- , testGroup "Complex Integer" $
  --   testLawOf ([] :: [Complex Integer]) ([] :: [(Complex Integer, Complex Integer)])
  --   <$> integralsUnboundedLaws
  , testGroup "Quotient Field Float" $
    testLawOf2 ([] :: [(Float, Integer)]) <$> quotientFieldLaws
  , testGroup "Quotient Field Double" $
    testLawOf2 ([] :: [(Double, Integer)]) <$> quotientFieldLaws
  , testsBool
  -- , testsComplexFloat
  , testsRational
  , testsLogFieldDouble
  ]

testsNatural :: TestTree
testsNatural = testGroup
  "Natural"
  [ testGroup "Additive" $ testLawOf1 ([] :: [Natural]) <$> additiveLaws
  , testGroup "Multiplicative"
  $ testLawOf1 ([] :: [Natural])
  <$> multiplicativeLaws
  , testGroup "Distributive" $ testLawOf1 ([] :: [Natural]) <$> distributiveLaws
  , testGroup "Naturalegral" $ testLawOf1 ([] :: [Natural]) <$> integralLaws
  , testGroup "Signed" $ testLawOf1 ([] :: [Natural]) <$> signedLaws
  , testGroup "Normed" $ testLawOf2 ([] :: [(Natural, Natural)]) <$> normedLaws
  ]

testsBool :: TestTree
testsBool = testGroup
  "Bool"
  [ testGroup "Idempotent" $ testLawOf1 ([] :: [Bool]) <$> idempotentLaws
  , testGroup "Additive" $ testLawOf1 ([] :: [Bool]) <$> additiveLaws
  , testGroup "Multiplicative" $ testLawOf1 ([] :: [Bool]) <$> multiplicativeLaws
  , testGroup "Distributive" $ testLawOf1 ([] :: [Bool]) <$> distributiveLaws
  ]

{-
testsComplexFloat :: TestTree
testsComplexFloat = testGroup
  "Complex Float"
  [ testGroup "Additive - Associative Fail"
  $ testLawOf1 ([] :: [Complex Float])
  <$> additiveLawsFail
  , testGroup "Additive Group"
  $ testLawOf1 ([] :: [Complex Float])
  <$> additiveGroupLaws
  , testGroup "Multiplicative - Associative Fail"
  $ testLawOf1 ([] :: [Complex Float])
  <$> multiplicativeLawsFail
  , testGroup "MultiplicativeGroup"
  $ testLawOf1 ([] :: [Complex Float])
  <$> multiplicativeGroupLaws_
  , testGroup "Distributive - Fail"
  $ testLawOf1 ([] :: [Complex Float])
  <$> distributiveLawsFail
    -- there is no way to define Ord (Complex a). Is there a a way to test it?
    -- , testGroup "Exponential Field" $
    --   testLawOf2 ([] :: [(Complex Float, Float)]) <$> expFieldLaws
  , testGroup "Normed"
  $ testLawOf2 ([] :: [(Complex Float, Float)])
  <$> normedLaws
  -- , testGroup "Metric"
  -- $ testLawOf2 ([] :: [(Complex Float, Float)])
  -- <$> metricRationalLaws
  , testGroup "Involutive Ring"
  $ testLawOf1 ([] :: [Complex Float])
  <$> involutiveRingLaws
  ]
-}

testsRational :: TestTree
testsRational = testGroup
  "Rational"
  [ testGroup "Additive - Associative"
  $ testLawOf1 ([] :: [Rational])
  <$> additiveLaws
  , testGroup "Additive Group"
  $ testLawOf1 ([] :: [Rational])
  <$> additiveGroupLaws
  , testGroup "Multiplicative - Associative"
  $ testLawOf1 ([] :: [Rational])
  <$> multiplicativeLaws
  , testGroup "MultiplicativeGroup"
  $ testLawOf1 ([] :: [Rational])
  <$> multiplicativeGroupLaws_
  , testGroup "Distributive" $ testLawOf1 ([] :: [Rational]) <$> distributiveLaws
  , testGroup "Signed" $ testLawOf1 ([] :: [Rational]) <$> signedLaws
  , testGroup "Normed"
  $ testLawOf2 ([] :: [(Rational, Rational)])
  <$> normedLaws
  -- , testGroup "Metric"
  -- $ testLawOf2 ([] :: [(Rational, Rational)])
  -- <$> metricRationalLaws
  , testGroup "Rational" $ testLawOf1 ([] :: [Rational]) <$> rationalLaws
  , testGroup "Distributive" $ testLawOf1 ([] :: [Int]) <$> distributiveLaws
  -- , testGroup "Metric" $ testLawOf2 ([] :: [(Int, Int)]) <$>
  --   metricIntegralLaws
  , testGroup "Normed or maxBound" $ testLawOf2 ([] :: [(Int, Int)]) <$> normedBoundedLaws
  ]

testsLogFieldDouble :: TestTree
testsLogFieldDouble = testGroup
  "LogField Double"
  [ testGroup "Additive - Associative Fail"
  $ testLawOf1 ([] :: [LogField Double])
  <$> additiveLawsFail
  , testGroup "Multiplicative - Associative Fail"
  $ testLawOf1 ([] :: [LogField Double])
  <$> multiplicativeLawsFail
  , testGroup "MultiplicativeGroup"
  $ testLawOf1 ([] :: [LogField Double])
  <$> multiplicativeGroupLaws_
  , testGroup "Distributive - Fail"
  $ testLawOf1 ([] :: [LogField Double])
  <$> distributiveLawsFail
  ]
