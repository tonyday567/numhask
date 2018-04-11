{-# OPTIONS_GHC -Wall #-}

-- | testing IEEE numbers is a special kind of hell, and one that I reserve for days when I can hardly think, so please forgive the horrible hackery contained within this file.
--
-- This suite sometimes fails, having been hand-crafty towards balancing reasonably approximate equality versus unbounded failure (given enough trials).
module Main where

import NumHask.Prelude
import NumHask.Laws

import Test.DocTest
import Test.Tasty
       (TestTree, defaultMain, testGroup)

main :: IO ()
main = do
  doctest ["src/NumHask/Examples.hs"]
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "NumHask"
    [ testsInt
    , testsFloat
    , testsBool
    , testsComplexFloat
    , testsComplexInvolutive
    ]

testsInt :: TestTree
testsInt =
  testGroup
    "Int"
    [ testGroup "Additive" $ testLawOf ([] :: [Int]) <$> additiveLaws
    , testGroup "Additive Group" $ testLawOf ([] :: [Int]) <$> additiveGroupLaws
    , testGroup "Multiplicative" $
      testLawOf ([] :: [Int]) <$> multiplicativeLaws
    , testGroup "Distribution" $ testLawOf ([] :: [Int]) <$> distributionLaws
    , testGroup "Integral" $ testLawOf ([] :: [Int]) <$> integralLaws
    , testGroup "Signed" $ testLawOf ([] :: [Int]) <$> signedLaws
    ]

testsFloat :: TestTree
testsFloat =
  testGroup
    "Float"
    [ testGroup "Additive - Associative Fail" $
      testLawOf ([] :: [Float]) <$> additiveLawsFail
    , testGroup "Additive Group" $
      testLawOf ([] :: [Float]) <$> additiveGroupLaws
    , testGroup "Multiplicative - Associative Fail" $
      testLawOf ([] :: [Float]) <$> multiplicativeLawsFail
    , testGroup "MultiplicativeGroup" $
      testLawOf ([] :: [Float]) <$> multiplicativeGroupLaws
    , testGroup "Distribution - Fail" $
      testLawOf ([] :: [Float]) <$> distributionLawsFail
    , testGroup "Signed" $ testLawOf ([] :: [Float]) <$> signedLaws
    , testGroup "Bounded Field" $
      testLawOf ([] :: [Float]) <$> boundedFieldFloatLaws
    , testGroup "Metric" $ testLawOf2 ([] :: [(Float, Float)]) <$> metricFloatLaws
    , testGroup "Quotient Field" $
      testLawOf ([] :: [Float]) <$> quotientFieldLaws
    , testGroup "Exponential Field" $ testLawOf ([] :: [Float]) <$> expFieldLaws
    ]

testsBool :: TestTree
testsBool =
  testGroup
    "Bool"
    [ testGroup "Idempotent" $ testLawOf ([] :: [Bool]) <$> idempotentLaws
    , testGroup "Additive" $ testLawOf ([] :: [Bool]) <$> additiveLaws
    , testGroup "Multiplicative" $
      testLawOf ([] :: [Bool]) <$> multiplicativeLaws
    , testGroup "Distribution" $ testLawOf ([] :: [Bool]) <$> distributionLaws
    ]

testsComplexFloat :: TestTree
testsComplexFloat =
  testGroup
    "Complex Float"
    [ testGroup "Additive - Associative Fail" $
      testLawOf ([] :: [Complex Float]) <$> additiveLawsFail
    , testGroup "Additive Group" $
      testLawOf ([] :: [Complex Float]) <$> additiveGroupLaws
    , testGroup "Multiplicative - Associative Fail" $
      testLawOf ([] :: [Complex Float]) <$> multiplicativeLawsFail
    , testGroup "MultiplicativeGroup" $
      testLawOf ([] :: [Complex Float]) <$> multiplicativeGroupLaws
    , testGroup "Distribution - Fail" $
      testLawOf ([] :: [Complex Float]) <$> distributionLawsFail
    , testGroup "Exponential Field" $
      testLawOf ([] :: [Complex Float]) <$> expFieldComplexLooseLaws 10
    , testGroup "Metric" $
      testLawOf2 ([] :: [(Complex Float, Float)]) <$> metricComplexFloatLaws
    ]

testsComplexInvolutive :: TestTree
testsComplexInvolutive =
  testGroup
    "Complex Involutive (Float)"
    [ testGroup "Involutive Ring" $
      -- fixme: why doesn't Complex Int work here?
      testLawOf ([] :: [Complex Float]) <$> involutiveRingLaws
    ]


