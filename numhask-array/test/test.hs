{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import NumHask.Prelude
import NumHask.Laws
import NumHask.Array

-- import Test.DocTest
import Test.Tasty
-- import Test.Tasty.QuickCheck

main :: IO ()
main = do
  putStrLn ("Array DocTest turned off" :: Text)
  -- doctest ["src/NumHask/Array.hs"]
  putStrLn ("Example DocTest turned off" :: Text)
  -- doctest ["src/NumHask/Array/Example.hs"]
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
      testLawOf ([] :: [Vector [] 6 Int]) <$> multiplicationLaws
    , testGroup "Distribution" $
      testLawOf ([] :: [Vector [] 6 Int]) <$> distributionLaws
    , testGroup "Additive Module" $
      testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> additiveModuleLaws
    , testGroup "Additive Group Module" $
      testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> additiveGroupModuleLaws
    -- , testGroup "Multiplicative Module" $
    --  testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> multiplicationModuleLaws
    -- , testGroup "Hilbert" $
    --  testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> hilbertLaws
    -- , testGroup "Tensor product" $
    --   testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> tensorProductLaws
    , testGroup "Additive Basis" $
      testLawOf ([] :: [Vector [] 6 Int]) <$> additiveBasisLaws
    -- , testGroup "Additive Group Basis" $
    --   testLawOf ([] :: [Vector [] 6 Int]) <$> additiveGroupBasisLaws
    -- FIXME: no instance (Applicative (Array [] '[6]))
    -- , testGroup "Multiplication Basis" $
    --  testLawOf ([] :: [Vector [] 6 Int]) <$> multiplicationBasisLaws
    ]

testsMInt :: TestTree
testsMInt =
  testGroup
    "Matrix [] 4 3 Int"
    [ testGroup "Additive" $ testLawOf ([] :: [Matrix [] 4 3 Int]) <$> additiveLaws
    , testGroup "Additive Group" $
      testLawOf ([] :: [Matrix [] 4 3 Int]) <$> additiveGroupLaws
    -- FIXME: reinstate monoidal laws
    -- , testGroup "Multiplication (square only)" $
    --  testLawOf ([] :: [Matrix [] 3 3 Int]) <$> multiplicationMonoidalLaws
    , testGroup "Additive Module" $
      testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> additiveModuleLaws
    , testGroup "Additive Group Module" $
      testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> additiveGroupModuleLaws
    -- , testGroup "Multiplication Module" $
    --   testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> multiplicationModuleLaws
    -- , testGroup "Hilbert" $
    --   testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> hilbertLaws
    -- , testGroup "Tensor product" $
    --   testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> tensorProductLaws
    , testGroup "Additive Basis" $
      testLawOf ([] :: [Matrix [] 4 3 Int]) <$> additiveBasisLaws
    -- , testGroup "Additive Group Basis" $
    --   testLawOf ([] :: [Matrix [] 4 3 Int]) <$> additiveGroupBasisLaws
    -- , testGroup "Multiplication Basis" $
    --  testLawOf ([] :: [Matrix [] 4 3 Int]) <$> multiplicationBasisLaws
    ]

testsVFloat :: TestTree
testsVFloat =
  testGroup
    "Vector 6 Float"
    [ testGroup "MultiplicationGroup" $
      testLawOf ([] :: [Vector [] 6 Float]) <$> multiplicationGroupLaws_
    , testGroup "Signed" $ testLawOf ([] :: [Vector [] 6 Float]) <$> signedLaws
    , testGroup "Normed" $
      testLawOf2 ([] :: [(Vector [] 6 Float, Float)]) <$> normedLaws
    , testGroup "Metric" $
      testLawOf2 ([] :: [(Vector [] 6 Float, Float)]) <$> metricRationalLaws
    , testGroup "Exponential Field" $
      testLawOf ([] :: [Vector [] 6 Float]) <$> expFieldContainerLaws
    -- , testGroup "Multiplication Group Module" $
    --   localOption (QuickCheckTests 1000) .
    --   testLawOf2 ([] :: [(Vector [] 6 Float, Float)]) <$>
    --   multiplicationGroupModuleLawsFail

    -- FIXME: no instance (Applicative (Array [] '[6]))
    -- , testGroup "Multiplication Group Basis" $
    --   testLawOf ([] :: [Vector [] 6 Float]) <$> multiplicationGroupBasisLaws
    ]

testsMFloat :: TestTree
testsMFloat =
  testGroup
    "Matrix [] 4 3 Float"
    [ -- testGroup "Multiplication Group Module" $
      -- localOption (QuickCheckTests 1000) .
      -- testLawOf2 ([] :: [(Matrix [] 4 3 Float, Float)]) <$>
      -- multiplicationGroupModuleLawsFail
    -- , testGroup "Multiplication Group Basis" $
      -- testLawOf ([] :: [Matrix [] 4 3 Float]) <$> multiplicationGroupBasisLaws
    ]
