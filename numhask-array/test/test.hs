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
    [ testGroup "Additive" $ testLawOf1 ([] :: [Vector [] 6 Int]) <$> additiveLaws
    , testGroup "Additive Group" $
      testLawOf1 ([] :: [Vector [] 6 Int]) <$> additiveGroupLaws
    , testGroup "Multiplicative" $
      testLawOf1 ([] :: [Vector [] 6 Int]) <$> multiplicativeLaws
    , testGroup "Distributive" $
      testLawOf1 ([] :: [Vector [] 6 Int]) <$> distributiveLaws
    , testGroup "Additive Module" $
      testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> additiveModuleLaws
    , testGroup "Additive Group Module" $
      testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> additiveGroupModuleLaws
    -- , testGroup "Multiplicative Module" $
    --  testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> multiplicativeModuleLaws
    -- , testGroup "Hilbert" $
    --  testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> hilbertLaws
    -- , testGroup "Tensor product" $
    --   testLawOf2 ([] :: [(Vector [] 6 Int, Int)]) <$> tensorProductLaws
    -- FIXME: no instance (Applicative (Array [] '[6]))
    -- , testGroup "Multiplicative Basis" $
    --  testLawOf1 ([] :: [Vector [] 6 Int]) <$> multiplicativeBasisLaws
    ]

testsMInt :: TestTree
testsMInt =
  testGroup
    "Matrix [] 4 3 Int"
    [ testGroup "Additive" $ testLawOf1 ([] :: [Matrix [] 4 3 Int]) <$> additiveLaws
    , testGroup "Additive Group" $
      testLawOf1 ([] :: [Matrix [] 4 3 Int]) <$> additiveGroupLaws
    -- FIXME: reinstate monoidal laws
    -- , testGroup "Multiplicative (square only)" $
    --  testLawOf1 ([] :: [Matrix [] 3 3 Int]) <$> multiplicativeMonoidalLaws
    , testGroup "Additive Module" $
      testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> additiveModuleLaws
    , testGroup "Additive Group Module" $
      testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> additiveGroupModuleLaws
    -- , testGroup "Multiplicative Module" $
    --   testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> multiplicativeModuleLaws
    -- , testGroup "Hilbert" $
    --   testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> hilbertLaws
    -- , testGroup "Tensor product" $
    --   testLawOf2 ([] :: [(Matrix [] 4 3 Int, Int)]) <$> tensorProductLaws
    -- , testGroup "Multiplicative Basis" $
    --  testLawOf1 ([] :: [Matrix [] 4 3 Int]) <$> multiplicativeBasisLaws
    ]

testsVFloat :: TestTree
testsVFloat =
  testGroup
    "Vector 6 Float"
    [ -- testGroup "MultiplicativeGroup" $
      -- testLawOf1 ([] :: [Vector [] 6 Float]) <$> multiplicativeGroupLaws
      testGroup "Signed" $ testLawOf1 ([] :: [Vector [] 6 Float]) <$> signedLaws
    , testGroup "Norm" $
      testLawOf2 ([] :: [(Vector [] 6 Float, Float)]) <$> normedLaws
    -- , testGroup "Metric" $
    --   testLawOf2 ([] :: [(Vector [] 6 Float, Float)]) <$> metricRationalLaws
    , testGroup "Exponential Field" $
      testLawOf1 ([] :: [Vector [] 6 Float]) <$> expFieldContainerLaws
    -- , testGroup "Multiplicative Group Module" $
    --   localOption (QuickCheckTests 1000) .
    --   testLawOf2 ([] :: [(Vector [] 6 Float, Float)]) <$>
    --   multiplicativeGroupModuleLawsFail

    -- FIXME: no instance (Applicative (Array [] '[6]))
    -- , testGroup "Multiplicative Group Basis" $
    --   testLawOf1 ([] :: [Vector [] 6 Float]) <$> multiplicativeGroupBasisLaws
    ]

testsMFloat :: TestTree
testsMFloat =
  testGroup
    "Matrix [] 4 3 Float"
    [ -- testGroup "Multiplicative Group Module" $
      -- localOption (QuickCheckTests 1000) .
      -- testLawOf2 ([] :: [(Matrix [] 4 3 Float, Float)]) <$>
      -- multiplicativeGroupModuleLawsFail
    -- , testGroup "Multiplicative Group Basis" $
      -- testLawOf1 ([] :: [Matrix [] 4 3 Float]) <$> multiplicativeGroupBasisLaws
    ]
