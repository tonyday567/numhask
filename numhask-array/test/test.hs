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
    [ testGroup "Additive" $ testLawOf ([] :: [Vector [] 6 Int]) <$> additionLaws
    , testGroup "Additive Group" $
      testLawOf ([] :: [Vector [] 6 Int]) <$> additionGroupLaws
    , testGroup "Multiplicative" $
      testLawOf ([] :: [Vector [] 6 Int]) <$> multiplicationLaws
    , testGroup "Distribution" $
      testLawOf ([] :: [Vector [] 6 Int]) <$> distributionLaws
    , testGroup "Additive Module" $
      testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> additionModuleLaws
    , testGroup "Additive Group Module" $
      testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> additionGroupModuleLaws
    -- , testGroup "Multiplicative Module" $
    --  testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> multiplicationModuleLaws
    -- , testGroup "Hilbert" $
    --  testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> hilbertLaws
    -- , testGroup "Tensor product" $
    --   testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> tensorProductLaws
    , testGroup "Additive Basis" $
      testLawOf ([] :: [Vector [] 6 Int]) <$> additionBasisLaws
    -- , testGroup "Addition Group Basis" $
    --   testLawOf ([] :: [Vector [] 6 Int]) <$> additionGroupBasisLaws
    -- FIXME: no instance (Applicative (Array [] '[6]))
    -- , testGroup "Multiplication Basis" $
    --  testLawOf ([] :: [Vector [] 6 Int]) <$> multiplicationBasisLaws
    ]

testsMInt :: TestTree
testsMInt =
  testGroup
    "Matrix [] 4 3 Int"
    [ testGroup "Addition" $ testLawOf ([] :: [Matrix [] 4 3 Int]) <$> additionLaws
    , testGroup "Addition Group" $
      testLawOf ([] :: [Matrix [] 4 3 Int]) <$> additionGroupLaws
    -- FIXME: reinstate monoidal laws
    -- , testGroup "Multiplication (square only)" $
    --  testLawOf ([] :: [Matrix [] 3 3 Int]) <$> multiplicationMonoidalLaws
    , testGroup "Addition Module" $
      testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> additionModuleLaws
    , testGroup "Addition Group Module" $
      testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> additionGroupModuleLaws
    -- , testGroup "Multiplication Module" $
    --   testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> multiplicationModuleLaws
    -- , testGroup "Hilbert" $
    --   testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> hilbertLaws
    -- , testGroup "Tensor product" $
    --   testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> tensorProductLaws
    , testGroup "Addition Basis" $
      testLawOf ([] :: [Matrix [] 4 3 Int]) <$> additionBasisLaws
    -- , testGroup "Addition Group Basis" $
    --   testLawOf ([] :: [Matrix [] 4 3 Int]) <$> additionGroupBasisLaws
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
