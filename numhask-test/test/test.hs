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
  [ testsInt
  , testsInt8
  , testsInt16
  , testsInt32
  , testsInt64
  , testsWord
  , testsWord8
  , testsWord16
  , testsWord32
  , testsWord64
  , testsNatural
  , testsFloat
  , testsDouble
  , testsBool
  , testsComplexFloat
  , testsRational
  , testsLogFieldDouble
  ]

testsInt :: TestTree
testsInt = testGroup
  "Int"
  [ testGroup "Additive" $ testLawOf ([] :: [Int]) <$> additiveLaws
  , testGroup "Additive Group" $ testLawOf ([] :: [Int]) <$> additiveGroupLaws
  , testGroup "Multiplicative" $ testLawOf ([] :: [Int]) <$> multiplicativeLaws
  , testGroup "Distributive" $ testLawOf ([] :: [Int]) <$> distributiveLaws
  , testGroup "Integral" $ testLawOf ([] :: [Int]) <$> integralLaws
  , testGroup "Signed" $ testLawOf ([] :: [Int]) <$> signedLaws
  , testGroup "Metric" $ testLawOf2 ([] :: [(Int, Int)]) <$> metricIntegralLaws
  , testGroup "Normed or maxBound"
  $ testLawOf2 ([] :: [(Int, Int)])
  <$> normedBoundedLaws
  ]

testsInteger :: TestTree
testsInteger = testGroup
  "Integer"
  [ testGroup "Integrals" $ testLawOf ([] :: [Integer]) <$> integralsLaws
  , testGroup "Metric"
  $ testLawOf2 ([] :: [(Integer, Integer)])
  <$> metricIntegralLaws
  , testGroup "Normed" $ testLawOf2 ([] :: [(Integer, Integer)]) <$> normedLaws
  ]

testsInt8 :: TestTree
testsInt8 = testGroup
  "Int8"
  [ testGroup "Integrals" $ testLawOf ([] :: [Int8]) <$> integralsLaws
  , testGroup "Metric"
  $ testLawOf2 ([] :: [(Int8, Int8)])
  <$> metricIntegralBoundedLaws
  , testGroup "Normed or maxBound"
  $ testLawOf2 ([] :: [(Int8, Int8)])
  <$> normedBoundedLaws
  ]

testsInt16 :: TestTree
testsInt16 = testGroup
  "Int16"
  [ testGroup "Integrals" $ testLawOf ([] :: [Int16]) <$> integralsLaws
  , testGroup "Metric"
  $ testLawOf2 ([] :: [(Int16, Int16)])
  <$> metricIntegralBoundedLaws
  , testGroup "Normed or maxBound"
  $ testLawOf2 ([] :: [(Int16, Int16)])
  <$> normedBoundedLaws
  ]

testsInt32 :: TestTree
testsInt32 = testGroup
  "Int32"
  [ testGroup "Integrals" $ testLawOf ([] :: [Int32]) <$> integralsLaws
  ,  testGroup "Metric"
  $ testLawOf2 ([] :: [(Int32, Int32)])
  <$> metricIntegralBoundedLaws
  , testGroup "Normed or maxBound"
  $ testLawOf2 ([] :: [(Int32, Int32)])
  <$> normedBoundedLaws
  ]

testsInt64 :: TestTree
testsInt64 = testGroup
  "Int64"
  [ testGroup "Integrals" $ testLawOf ([] :: [Int64]) <$> integralsLaws
  ,  testGroup "Metric"
  $ testLawOf2 ([] :: [(Int64, Int64)])
  <$> metricIntegralBoundedLaws
  , testGroup "Normed or maxBound"
  $ testLawOf2 ([] :: [(Int64, Int64)])
  <$> normedBoundedLaws
  ]

testsWord :: TestTree
testsWord = testGroup
  "Word"
  [ testGroup "Additive" $ testLawOf ([] :: [Word]) <$> additiveLaws
  , testGroup "Multiplicative" $ testLawOf ([] :: [Word]) <$> multiplicativeLaws
  , testGroup "Distributive" $ testLawOf ([] :: [Word]) <$> distributiveLaws
  , testGroup "Integral" $ testLawOf ([] :: [Word]) <$> integralLaws
  , testGroup "Signed" $ testLawOf ([] :: [Word]) <$> signedLaws
  , testGroup "Metric"
  $ testLawOf2 ([] :: [(Word, Word)])
  <$> metricIntegralBoundedLaws
  , testGroup "Normed or maxBound"
  $ testLawOf2 ([] :: [(Word, Word)])
  <$> normedBoundedLaws
  ]

testsWord8 :: TestTree
testsWord8 = testGroup
  "Word8"
  [ testGroup "Additive" $ testLawOf ([] :: [Word8]) <$> additiveLaws
  , testGroup "Multiplicative"
  $ testLawOf ([] :: [Word8])
  <$> multiplicativeLaws
  , testGroup "Distributive" $ testLawOf ([] :: [Word8]) <$> distributiveLaws
  , testGroup "Integral" $ testLawOf ([] :: [Word8]) <$> integralLaws
  , testGroup "Signed" $ testLawOf ([] :: [Word8]) <$> signedLaws
  , testGroup "Metric"
  $ testLawOf2 ([] :: [(Word8, Word8)])
  <$> metricIntegralBoundedLaws
  , testGroup "Normed or maxBound"
  $ testLawOf2 ([] :: [(Word8, Word8)])
  <$> normedBoundedLaws
  ]

testsWord16 :: TestTree
testsWord16 = testGroup
  "Word16"
  [ testGroup "Additive" $ testLawOf ([] :: [Word16]) <$> additiveLaws
  , testGroup "Multiplicative"
  $ testLawOf ([] :: [Word16])
  <$> multiplicativeLaws
  , testGroup "Distributive" $ testLawOf ([] :: [Word16]) <$> distributiveLaws
  , testGroup "Integral" $ testLawOf ([] :: [Word16]) <$> integralLaws
  , testGroup "Signed" $ testLawOf ([] :: [Word16]) <$> signedLaws
  , testGroup "Metric"
  $ testLawOf2 ([] :: [(Word16, Word16)])
  <$> metricIntegralBoundedLaws
  , testGroup "Normed or maxBound"
  $ testLawOf2 ([] :: [(Word16, Word16)])
  <$> normedBoundedLaws
  ]

testsWord32 :: TestTree
testsWord32 = testGroup
  "Word32"
  [ testGroup "Additive" $ testLawOf ([] :: [Word32]) <$> additiveLaws
  , testGroup "Multiplicative"
  $ testLawOf ([] :: [Word32])
  <$> multiplicativeLaws
  , testGroup "Distributive" $ testLawOf ([] :: [Word32]) <$> distributiveLaws
  , testGroup "Integral" $ testLawOf ([] :: [Word32]) <$> integralLaws
  , testGroup "Signed" $ testLawOf ([] :: [Word32]) <$> signedLaws
  , testGroup "Metric"
  $ testLawOf2 ([] :: [(Word32, Word32)])
  <$> metricIntegralBoundedLaws
  , testGroup "Normed or maxBound"
  $ testLawOf2 ([] :: [(Word32, Word32)])
  <$> normedBoundedLaws
  ]

testsWord64 :: TestTree
testsWord64 = testGroup
  "Word64"
  [ testGroup "Additive" $ testLawOf ([] :: [Word64]) <$> additiveLaws
  , testGroup "Multiplicative"
  $ testLawOf ([] :: [Word64])
  <$> multiplicativeLaws
  , testGroup "Distributive" $ testLawOf ([] :: [Word64]) <$> distributiveLaws
  , testGroup "Integral" $ testLawOf ([] :: [Word64]) <$> integralLaws
  , testGroup "Signed" $ testLawOf ([] :: [Word64]) <$> signedLaws
  , testGroup "Metric"
  $ testLawOf2 ([] :: [(Word64, Word64)])
  <$> metricIntegralBoundedLaws
  , testGroup "Normed or maxBound"
  $ testLawOf2 ([] :: [(Word64, Word64)])
  <$> normedBoundedLaws
  ]

testsNatural :: TestTree
testsNatural = testGroup
  "Natural"
  [ testGroup "Additive" $ testLawOf ([] :: [Natural]) <$> additiveLaws
  , testGroup "Multiplicative"
  $ testLawOf ([] :: [Natural])
  <$> multiplicativeLaws
  , testGroup "Distributive" $ testLawOf ([] :: [Natural]) <$> distributiveLaws
  , testGroup "Naturalegral" $ testLawOf ([] :: [Natural]) <$> integralLaws
  , testGroup "Signed" $ testLawOf ([] :: [Natural]) <$> signedLaws
  , testGroup "Normed" $ testLawOf2 ([] :: [(Natural, Natural)]) <$> normedLaws
  ]

testsFloat :: TestTree
testsFloat = testGroup
  "Float"
  [ testGroup "Additive - Associative Fail"
  $ testLawOf ([] :: [Float])
  <$> additiveLawsFail
  , testGroup "Additive Group" $ testLawOf ([] :: [Float]) <$> additiveGroupLaws
  , testGroup "Multiplicative - Associative Fail"
  $ testLawOf ([] :: [Float])
  <$> multiplicativeLawsFail
  , testGroup "MultiplicativeGroup"
  $ testLawOf ([] :: [Float])
  <$> multiplicativeGroupLaws_
  , testGroup "Distributive - Fail"
  $ testLawOf ([] :: [Float])
  <$> distributiveLawsFail
  , testGroup "Signed" $ testLawOf ([] :: [Float]) <$> signedLaws
  , testGroup "Normed" $ testLawOf2 ([] :: [(Float, Float)]) <$> normedLaws
  , testGroup "Metric"
  $ testLawOf2 ([] :: [(Float, Float)])
  <$> metricRationalLaws
  , testGroup "Upper Bounded Field"
  $ testLawOf ([] :: [Float])
  <$> upperBoundedFieldLaws
  , testGroup "Lower Bounded Field"
  $ testLawOf ([] :: [Float])
  <$> lowerBoundedFieldLaws
  , testGroup "Quotient Field"
  $ testLawOf2 ([] :: [(Float, Integer)])
  <$> quotientFieldLaws
  , testGroup "Exponential Field"
  $ testLawOf2 ([] :: [(Float, Float)])
  <$> expFieldLaws
  , testGroup "Rational" $ testLawOf ([] :: [Float]) <$> rationalLaws
  ]

testsDouble :: TestTree
testsDouble = testGroup
  "Double"
  [ testGroup "Additive - Associative Fail"
  $ testLawOf ([] :: [Double])
  <$> additiveLawsFail
  , testGroup "Additive Group"
  $ testLawOf ([] :: [Double])
  <$> additiveGroupLaws
  , testGroup "Multiplicative - Associative Fail"
  $ testLawOf ([] :: [Double])
  <$> multiplicativeLawsFail
  , testGroup "MultiplicativeGroup"
  $ testLawOf ([] :: [Double])
  <$> multiplicativeGroupLaws_
  , testGroup "Distributive - Fail"
  $ testLawOf ([] :: [Double])
  <$> distributiveLawsFail
  , testGroup "Signed" $ testLawOf ([] :: [Double]) <$> signedLaws
  , testGroup "Normed" $ testLawOf2 ([] :: [(Double, Double)]) <$> normedLaws
  , testGroup "Metric"
  $ testLawOf2 ([] :: [(Double, Double)])
  <$> metricRationalLaws
  , testGroup "Upper Bounded Field"
  $ testLawOf ([] :: [Double])
  <$> upperBoundedFieldLaws
  , testGroup "Lower Bounded Field"
  $ testLawOf ([] :: [Double])
  <$> lowerBoundedFieldLaws
  , testGroup "Quotient Field"
  $ testLawOf2 ([] :: [(Double, Integer)])
  <$> quotientFieldLaws
  , testGroup "Exponential Field"
  $ testLawOf2 ([] :: [(Double, Double)])
  <$> expFieldLaws
  , testGroup "Rational" $ testLawOf ([] :: [Double]) <$> rationalLaws
  ]

testsBool :: TestTree
testsBool = testGroup
  "Bool"
  [ testGroup "Idempotent" $ testLawOf ([] :: [Bool]) <$> idempotentLaws
  , testGroup "Additive" $ testLawOf ([] :: [Bool]) <$> additiveLaws
  , testGroup "Multiplicative" $ testLawOf ([] :: [Bool]) <$> multiplicativeLaws
  , testGroup "Distributive" $ testLawOf ([] :: [Bool]) <$> distributiveLaws
  ]

testsComplexFloat :: TestTree
testsComplexFloat = testGroup
  "Complex Float"
  [ testGroup "Additive - Associative Fail"
  $ testLawOf ([] :: [Complex Float])
  <$> additiveLawsFail
  , testGroup "Additive Group"
  $ testLawOf ([] :: [Complex Float])
  <$> additiveGroupLaws
  , testGroup "Multiplicative - Associative Fail"
  $ testLawOf ([] :: [Complex Float])
  <$> multiplicativeLawsFail
  , testGroup "MultiplicativeGroup"
  $ testLawOf ([] :: [Complex Float])
  <$> multiplicativeGroupLaws_
  , testGroup "Distributive - Fail"
  $ testLawOf ([] :: [Complex Float])
  <$> distributiveLawsFail
    -- there is no way to define Ord (Complex a). Is there a a way to test it?
    -- , testGroup "Exponential Field" $
    --   testLawOf2 ([] :: [(Complex Float, Float)]) <$> expFieldLaws
  , testGroup "Normed"
  $ testLawOf2 ([] :: [(Complex Float, Float)])
  <$> normedLaws
  , testGroup "Metric"
  $ testLawOf2 ([] :: [(Complex Float, Float)])
  <$> metricRationalLaws
  , testGroup "Involutive Ring"
  $ testLawOf ([] :: [Complex Float])
  <$> involutiveRingLaws
  ]

testsRational :: TestTree
testsRational = testGroup
  "Rational"
  [ testGroup "Additive - Associative"
  $ testLawOf ([] :: [Rational])
  <$> additiveLaws
  , testGroup "Additive Group"
  $ testLawOf ([] :: [Rational])
  <$> additiveGroupLaws
  , testGroup "Multiplicative - Associative"
  $ testLawOf ([] :: [Rational])
  <$> multiplicativeLaws
  , testGroup "MultiplicativeGroup"
  $ testLawOf ([] :: [Rational])
  <$> multiplicativeGroupLaws_
  , testGroup "Distributive" $ testLawOf ([] :: [Rational]) <$> distributiveLaws
  , testGroup "Signed" $ testLawOf ([] :: [Rational]) <$> signedLaws
  , testGroup "Normed"
  $ testLawOf2 ([] :: [(Rational, Rational)])
  <$> normedLaws
  , testGroup "Metric"
  $ testLawOf2 ([] :: [(Rational, Rational)])
  <$> metricRationalLaws
  , testGroup "Rational" $ testLawOf ([] :: [Rational]) <$> rationalLaws
  ]

    --  testGroup "Distributive" $ testLawOf ([] :: [Int]) <$> distributiveLaws
    -- , testGroup "Metric" $ testLawOf2 ([] :: [(Int, Int)]) <$>
    --   metricIntegralLaws
    -- , testGroup "Normed or maxBound" $ testLawOf2 ([] :: [(Int, Int)]) <$> normedBoundedLaws

testsLogFieldDouble :: TestTree
testsLogFieldDouble = testGroup
  "LogField Double"
  [ testGroup "Additive - Associative Fail"
  $ testLawOf ([] :: [LogField Double])
  <$> additiveLawsFail
  , testGroup "Multiplicative - Associative Fail"
  $ testLawOf ([] :: [LogField Double])
  <$> multiplicativeLawsFail
  , testGroup "MultiplicativeGroup"
  $ testLawOf ([] :: [LogField Double])
  <$> multiplicativeGroupLaws_
  , testGroup "Distributive - Fail"
  $ testLawOf ([] :: [LogField Double])
  <$> distributiveLawsFail
  ]
