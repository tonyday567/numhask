{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | testing IEEE numbers is a special kind of hell, and one that I reserve for days when I can hardly think, so please forgive the horrible hackery contained within this file.
--
-- This suite sometimes fails, having been hand-crafty towards balancing reasonably approximate equality versus unbounded failure (given enough trials).
module Main where

import NumHask.Laws
import NumHask.Laws.Interval
import NumHask.Prelude
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.Tasty (TestTree, defaultMain, testGroup, localOption)
import Test.Tasty.QuickCheck

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
  , testGroup "Complex Float" $ localOption (QuickCheckTests 1000) .
    testLawOf ([] :: [Complex Float]) ([] :: [(Complex Float, Complex Float)]) <$>
    complexIntervalLaws (10.0 :+ 5.0) 10.0
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
  , testGroup "Quotient Field Float" $
    testLawOf2 ([] :: [(Float, Integer)]) <$> quotientFieldLaws
  , testGroup "Quotient Field Double" $
    testLawOf2 ([] :: [(Double, Integer)]) <$> quotientFieldLaws
  , testsBool
  , testsRational
  -- FIXME: awaiting a proper instance for (-)
  -- , testGroup "LogField Double" $
  --   testLawOf ([] :: [LogField Double]) ([] :: [(LogField Double, LogField Double)]) <$> logFieldLaws
  ]

testsNatural :: TestTree
testsNatural = testGroup
  "Natural"
  [ testGroup "Additive" $ testLawOf1 ([] :: [Natural]) <$> additiveLaws
  , testGroup "Multiplicative"
  $ testLawOf1 ([] :: [Natural])
  <$> multiplicativeLaws
  , testGroup "Distributive" $ testLawOf1 ([] :: [Natural]) <$> distributiveLaws
  , testGroup "Integral" $ testLawOf1 ([] :: [Natural]) <$> integralLaws
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

testsRational :: TestTree
testsRational = testGroup
  "Rational"
  [ testGroup "Additive"
  $ testLawOf1 ([] :: [Rational])
  <$> additiveLaws
  , testGroup "Subtractive"
  $ testLawOf1 ([] :: [Rational])
  <$> subtractiveLaws
  , testGroup "Multiplicative"
  $ testLawOf1 ([] :: [Rational])
  <$> multiplicativeLaws
  , testGroup "Divisive"
  $ testLawOf1 ([] :: [Rational])
  <$> divisiveLaws
  , testGroup "Distributive" $ testLawOf1 ([] :: [Rational]) <$> distributiveLaws
  , testGroup "Signed" $ testLawOf1 ([] :: [Rational]) <$> signedLaws
  , testGroup "Normed"
  $ testLawOf2 ([] :: [(Rational, Rational)])
  <$> normedLaws
  , testGroup "Metric"
  $ testLawOf2 ([] :: [(Rational, Rational)])
  <$> metricLaws
  , testGroup "Rational" $ testLawOf1 ([] :: [Rational]) <$> rationalLaws
  ]

logFieldLaws
  :: (
    )
  => [Laws (LogField Double) (LogField Double)]
logFieldLaws = 
  (Arity1 <$> additiveIntervalLaws (logField 10.0)) <>
  (Arity1 <$> multiplicativeIntervalLaws (logField 10.0)) <>
  (Arity1 <$> divisiveIntervalLaws (logField 10.0)) <>
  (Arity1 <$> distributiveIntervalLaws (logField 10.0))

