{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import NumHask.Prelude
import NumHask.Laws
import NumHask.Array

import Test.DocTest
import Test.Tasty
       (TestTree, defaultMain, testGroup, localOption)
import Test.Tasty.QuickCheck

main :: IO ()
main = do
  putStrLn ("Array DocTest" :: Text)
  doctest ["src/NumHask/Array.hs"]
  putStrLn ("Example DocTest" :: Text)
  doctest ["src/NumHask/Array/Example.hs"]
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "NumHask"
    [ testsVInt
    , testsMInt
    , testsVFloat
    , testsMFloat
    ]

testsVInt :: TestTree
testsVInt =
  testGroup
    "Vector [] 6 Int"
    [ testGroup "Additive" $ testLawOf ([] :: [Vector [] 6 Int]) <$> additiveLaws
    , testGroup "Additive Group" $
      testLawOf ([] :: [Vector [] 6 Int]) <$> additiveGroupLaws
    , testGroup "Multiplicative" $
      testLawOf ([] :: [Vector [] 6 Int]) <$> multiplicativeLaws
    , testGroup "Distribution" $
      testLawOf ([] :: [Vector [] 6 Int]) <$> distributionLaws
    , testGroup "Additive Module" $
      testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> additiveModuleLaws
    , testGroup "Additive Group Module" $
      testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> additiveGroupModuleLaws
    , testGroup "Multiplicative Module" $
      testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> multiplicativeModuleLaws
    , testGroup "Hilbert" $
      testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> hilbertLaws
    , testGroup "Tensor product" $
      testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> tensorProductLaws
    , testGroup "Additive Basis" $
      testLawOf ([] :: [Vector [] 6 Int]) <$> additiveBasisLaws
    , testGroup "Additive Group Basis" $
      testLawOf ([] :: [Vector [] 6 Int]) <$> additiveGroupBasisLaws
    , testGroup "Multiplicative Basis" $
      testLawOf ([] :: [Vector [] 6 Int]) <$> multiplicativeBasisLaws
    ]

testsMInt :: TestTree
testsMInt =
  testGroup
    "Matrix [] 4 3 Int"
    [ testGroup "Additive" $ testLawOf ([] :: [Matrix [] 4 3 Int]) <$> additiveLaws
    , testGroup "Additive Group" $
      testLawOf ([] :: [Matrix [] 4 3 Int]) <$> additiveGroupLaws
    , testGroup "Multiplicative (square only)" $
      testLawOf ([] :: [Matrix [] 3 3 Int]) <$> multiplicativeMonoidalLaws
    , testGroup "Additive Module" $
      testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> additiveModuleLaws
    , testGroup "Additive Group Module" $
      testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> additiveGroupModuleLaws
    , testGroup "Multiplicative Module" $
      testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> multiplicativeModuleLaws
    , testGroup "Hilbert" $
      testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> hilbertLaws
    , testGroup "Tensor product" $
      testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> tensorProductLaws
    , testGroup "Additive Basis" $
      testLawOf ([] :: [Matrix [] 4 3 Int]) <$> additiveBasisLaws
    , testGroup "Additive Group Basis" $
      testLawOf ([] :: [Matrix [] 4 3 Int]) <$> additiveGroupBasisLaws
    , testGroup "Multiplicative Basis" $
      testLawOf ([] :: [Matrix [] 4 3 Int]) <$> multiplicativeBasisLaws
    ]

testsVFloat :: TestTree
testsVFloat =
  testGroup
    "Vector 6 Float"
    [ testGroup "MultiplicativeGroup" $
      testLawOf ([] :: [Vector [] 6 Float]) <$> multiplicativeGroupLaws_
    , testGroup "Signed" $ testLawOf ([] :: [Vector [] 6 Float]) <$> signedLaws
    , testGroup "Normed" $
      testLawOf2 ([] :: [(Vector [] 6 Float, Float)]) <$> normedLaws
    , testGroup "Metric" $
      testLawOf2 ([] :: [(Vector [] 6 Float, Float)]) <$> metricRationalLaws
    , testGroup "Exponential Field" $
      testLawOf ([] :: [Vector [] 6 Float]) <$> expFieldContainerLaws
    , testGroup "Multiplicative Group Module" $
      localOption (QuickCheckTests 1000) .
      testLawOf2 ([] :: [(Vector [] 6 Float, Float)]) <$>
      multiplicativeGroupModuleLawsFail
    , testGroup "Multiplicative Group Basis" $
      testLawOf ([] :: [Vector [] 6 Float]) <$> multiplicativeGroupBasisLaws
    ]

testsMFloat :: TestTree
testsMFloat =
  testGroup
    "Matrix [] 4 3 Float"
    [ testGroup "Multiplicative Group Module" $
      localOption (QuickCheckTests 1000) .
      testLawOf2 ([] :: [(Matrix [] 4 3 Float, Float)]) <$>
      multiplicativeGroupModuleLawsFail
    , testGroup "Multiplicative Group Basis" $
      testLawOf ([] :: [Matrix [] 4 3 Float]) <$> multiplicativeGroupBasisLaws
    ]
