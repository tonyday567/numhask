{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import NumHask.Prelude

import Test.Tasty (TestName, TestTree, testGroup, defaultMain, localOption)
import Test.Tasty.QuickCheck
import Test.DocTest

main :: IO ()
main = do
    doctest ["src/NumHask/Examples.hs"]
    defaultMain tests

data LawArity a =
    Nonary Bool |
    Unary (a -> Bool) |
    Binary (a -> a -> Bool) |
    Ternary (a -> a -> a -> Bool) |
    Ornary (a -> a -> a -> a -> Bool) |
    Failiary (a -> Property)

data LawArity2 a b =
    Unary2 (a -> Bool) |
    Binary2 (a -> b -> Bool) |
    Ternary2 (a -> a -> b -> Bool) |
    Ternary2' (a -> b -> b -> Bool) |
    Failiary2 (a -> Property)

type Law a = (TestName, LawArity a)

type Law2 a b = (TestName, LawArity2 a b)

testLawOf  :: (Arbitrary a, Show a) => [a] -> Law a -> TestTree
testLawOf _ (name, Nonary f) = testProperty name f
testLawOf _ (name, Unary f) = testProperty name f
testLawOf _ (name, Binary f) = testProperty name f
testLawOf _ (name, Ternary f) = testProperty name f
testLawOf _ (name, Ornary f) = testProperty name f
testLawOf _ (name, Failiary f) = testProperty name f

testLawOf2  :: (Arbitrary a, Show a, Arbitrary b, Show b) =>
    [(a,b)] -> Law2 a b -> TestTree
testLawOf2 _ (name, Unary2 f) = testProperty name f
testLawOf2 _ (name, Binary2 f) = testProperty name f
testLawOf2 _ (name, Ternary2 f) = testProperty name f
testLawOf2 _ (name, Ternary2' f) = testProperty name f
testLawOf2 _ (name, Failiary2 f) = testProperty name f

tests :: TestTree
tests =
    testGroup "NumHask"
    [ testsInt
    , testsFloat
    , testsBool
    , testsVInt
    , testsVFloat
    , testsMInt
    , testsMFloat
    , testsNInt
    , testsNShow
    , testsComplexFloat
    ]

testsInt :: TestTree
testsInt = testGroup "Int"
    [ testGroup "Additive" $ testLawOf ([]::[Int]) <$>
      additiveLaws
    , testGroup "Additive Group" $ testLawOf ([]::[Int]) <$>
      additiveGroupLaws
    , testGroup "Multiplicative" $ testLawOf ([]::[Int]) <$>
      multiplicativeLaws
    , testGroup "Distribution" $ testLawOf ([]::[Int])
      <$> distributionLaws
    , testGroup "Integral" $ testLawOf ([]::[Int]) <$>
      integralLaws
    , testGroup "Signed" $ testLawOf ([]::[Int]) <$>
      signedLaws
    ]

testsFloat :: TestTree
testsFloat = testGroup "Float"
    [ testGroup "Additive - Associative Fail" $ testLawOf ([]::[Float]) <$>
      additiveLawsFail
    , testGroup "Additive Group" $ testLawOf ([]::[Float]) <$>
      additiveGroupLaws
    , testGroup "Multiplicative - Associative Fail" $
      testLawOf ([]::[Float]) <$>
      multiplicativeLawsFail
    , testGroup "MultiplicativeGroup" $ testLawOf ([]::[Float]) <$>
      multiplicativeGroupLaws
    , testGroup "Distribution - Fail" $ testLawOf ([]::[Float]) <$>
      distributionLawsFail
    , testGroup "Signed" $ testLawOf ([]::[Float]) <$>
      signedLaws
    , testGroup "Bounded Field" $ testLawOf ([]::[Float]) <$>
      boundedFieldLaws
    , testGroup "Metric" $ testLawOf ([]::[Float]) <$> metricFloatLaws
    , testGroup "Quotient Field" $ testLawOf ([]::[Float]) <$>
      quotientFieldLaws
    , testGroup "Exponential Field" $ testLawOf ([]::[Float]) <$> expFieldLaws
    ]

testsBool :: TestTree
testsBool = testGroup "Bool"
    [ testGroup "Idempotent" $ testLawOf ([]::[Bool]) <$>
      idempotentLaws
    , testGroup "Additive" $ testLawOf ([]::[Bool]) <$>
      additiveLaws
    , testGroup "Multiplicative" $ testLawOf ([]::[Bool]) <$>
      multiplicativeLaws
    , testGroup "Distribution" $ testLawOf ([]::[Bool])
      <$> distributionLaws
    ]

testsComplexFloat :: TestTree
testsComplexFloat = testGroup "Complex Float"
    [ testGroup "Additive - Associative Fail" $ testLawOf ([]::[Complex Float]) <$>
      additiveLawsFail
    , testGroup "Additive Group" $ testLawOf ([]::[Complex Float]) <$>
      additiveGroupLaws
    , testGroup "Multiplicative - Associative Fail" $
      testLawOf ([]::[Complex Float]) <$>
      multiplicativeLawsFail
    , testGroup "MultiplicativeGroup" $ testLawOf ([]::[Complex Float]) <$>
      multiplicativeGroupLaws
    , testGroup "Distribution - Fail" $ testLawOf ([]::[Complex Float]) <$>
      distributionLawsFail
    -- , testGroup "Bounded Field" $ testLawOf ([]::[Complex Float]) <$>
    --   boundedFieldLaws
    -- , testGroup "Exponential Field" $ testLawOf ([]::[Complex Float]) <$> expFieldLaws
    , testGroup "Metric" $ testLawOf ([]::[Complex Float]) <$> metricComplexFloatLaws
    ]

testsVInt :: TestTree
testsVInt = testGroup "Vector 6 Int"
    [ testGroup "Additive" $ testLawOf ([]::[Vector 6 Int]) <$>
      additiveLaws
    , testGroup "Additive Group" $ testLawOf ([]::[Vector 6 Int]) <$>
      additiveGroupLaws
    , testGroup "Multiplicative" $ testLawOf ([]::[Vector 6 Int]) <$>
      multiplicativeLaws
    , testGroup "Distribution" $ testLawOf ([]::[Vector 6 Int])
      <$> distributionLaws
    , testGroup "Additive Module" $ testLawOf2 ([]::[(Vector 6 Int, Int)]) <$>
      additiveModuleLaws
    , testGroup "Additive Group Module" $ testLawOf2 ([]::[(Vector 6 Int, Int)]) <$>
      additiveGroupModuleLaws
    , testGroup "Multiplicative Module" $ testLawOf2 ([]::[(Vector 6 Int, Int)]) <$>
      multiplicativeModuleLaws
    , testGroup "Additive Basis" $ testLawOf ([]::[Vector 6 Int]) <$>
      additiveBasisLaws
    , testGroup "Additive Group Basis" $ testLawOf ([]::[Vector 6 Int]) <$>
      additiveGroupBasisLaws
    , testGroup "Multiplicative Basis" $ testLawOf ([]::[Vector 6 Int]) <$>
      multiplicativeBasisLaws
    ]

testsMInt :: TestTree
testsMInt = testGroup "Matrix 4 3 Int"
    [ testGroup "Additive" $ testLawOf ([]::[Matrix 4 3 Int]) <$>
      additiveLaws
    , testGroup "Additive Group" $ testLawOf ([]::[Matrix 4 3 Int]) <$>
      additiveGroupLaws
    , testGroup "Multiplicative" $ testLawOf ([]::[Matrix 4 3 Int]) <$>
      multiplicativeLaws
    , testGroup "Distribution" $ testLawOf ([]::[Matrix 4 3 Int])
      <$> distributionLaws
    , testGroup "Additive Module" $ testLawOf2 ([]::[(Matrix 4 3 Int, Int)]) <$>
      additiveModuleLaws
    , testGroup "Additive Group Module" $ testLawOf2 ([]::[(Matrix 4 3 Int, Int)]) <$>
      additiveGroupModuleLaws
    , testGroup "Multiplicative Module" $ testLawOf2 ([]::[(Matrix 4 3 Int, Int)]) <$>
      multiplicativeModuleLaws
    , testGroup "Additive Basis" $ testLawOf ([]::[Matrix 4 3 Int]) <$>
      additiveBasisLaws
    , testGroup "Additive Group Basis" $ testLawOf ([]::[Matrix 4 3 Int]) <$>
      additiveGroupBasisLaws
    , testGroup "Multiplicative Basis" $ testLawOf ([]::[Matrix 4 3 Int]) <$>
      multiplicativeBasisLaws
    ]

testsNInt :: TestTree
testsNInt = testGroup "Tensor [2,3,2] Int"
    [ testGroup "Additive" $ testLawOf ([]::[Tensor [2,3,2] Int]) <$>
      additiveLaws
    , testGroup "Additive Group" $ testLawOf ([]::[Tensor [2,3,2] Int]) <$>
      additiveGroupLaws
    , testGroup "Multiplicative" $ testLawOf ([]::[Tensor [2,3,2] Int]) <$>
      multiplicativeLaws
    , testGroup "Distribution" $ testLawOf ([]::[Tensor [2,3,2] Int])
      <$> distributionLaws
    , testGroup "Additive Module" $ testLawOf2 ([]::[(Tensor [2,3,2] Int, Int)]) <$>
      additiveModuleLaws
    , testGroup "Additive Group Module" $ testLawOf2 ([]::[(Tensor [2,3,2] Int, Int)]) <$>
      additiveGroupModuleLaws
    , testGroup "Multiplicative Module" $ testLawOf2 ([]::[(Tensor [2,3,2] Int, Int)]) <$>
      multiplicativeModuleLaws
    , testGroup "Additive Basis" $ testLawOf ([]::[Tensor [2,3,2] Int]) <$>
      additiveBasisLaws
    , testGroup "Additive Group Basis" $ testLawOf ([]::[Tensor [2,3,2] Int]) <$>
      additiveGroupBasisLaws
    , testGroup "Multiplicative Basis" $ testLawOf ([]::[Tensor [2,3,2] Int]) <$>
      multiplicativeBasisLaws
    ]

testsNShow :: TestTree
testsNShow = testGroup "NRep Int"
    [ testProperty "ok arbitrary" (const True :: SomeTensor Int -> Bool)
    ]

testsVFloat :: TestTree
testsVFloat = testGroup "Vector 6 Float"
    [ testGroup "Additive - Associative" $
      localOption (QuickCheckTests 1000) . testLawOf ([]::[Vector 6 Float]) <$>
      additiveLawsFail
    , testGroup "Additive Group" $
      testLawOf ([]::[Vector 6 Float]) <$>
      additiveGroupLaws
    , testGroup "Multiplicative - Associative" $
      localOption (QuickCheckTests 1000) . testLawOf ([]::[Vector 6 Float]) <$>
      multiplicativeLawsFail
    , testGroup "MultiplicativeGroup" $ testLawOf ([]::[Vector 6 Float]) <$>
      multiplicativeGroupLaws
    , testGroup "Distribution" $
      localOption (QuickCheckTests 1000) . testLawOf ([]::[Vector 6 Float]) <$>
      distributionLawsFail
    , testGroup "Signed" $ testLawOf ([]::[Vector 6 Float]) <$>
      signedLaws
    , testGroup "Metric" $ testLawOf ([]::[Vector 6 Float]) <$>
      metricNaperianFloatLaws
    , testGroup "Exponential Field" $ testLawOf ([]::[Vector 6 Float]) <$>
      expFieldNaperianLaws
    , testGroup "Additive Module" $ localOption (QuickCheckTests 1000) .
      testLawOf2 ([]::[(Vector 6 Float, Float)]) <$>
      additiveModuleLawsFail
    , testGroup "Additive Group Module" $ localOption (QuickCheckTests 1000) .
      testLawOf2 ([]::[(Vector 6 Float, Float)]) <$>
      additiveGroupModuleLawsFail
    , testGroup "Multiplicative Module" $ localOption (QuickCheckTests 1000) .
      testLawOf2 ([]::[(Vector 6 Float, Float)]) <$>
      multiplicativeModuleLawsFail
    , testGroup "Multiplicative Group Module" $ localOption (QuickCheckTests 1000) .
      testLawOf2 ([]::[(Vector 6 Float, Float)]) <$>
      multiplicativeGroupModuleLawsFail
    , testGroup "Additive Basis" $ localOption (QuickCheckTests 1000) .
      testLawOf ([]::[Vector 6 Float]) <$>
      additiveBasisLawsFail
    , testGroup "Additive Group Basis" $ testLawOf ([]::[Vector 6 Float]) <$>
      additiveGroupBasisLaws
    , testGroup "Multiplicative Basis" $ localOption (QuickCheckTests 1000) .
      testLawOf ([]::[Vector 6 Float]) <$>
      multiplicativeBasisLawsFail
    , testGroup "Multiplicative Group Basis" $ testLawOf ([]::[Vector 6 Float]) <$>
      multiplicativeGroupBasisLaws
    , testGroup "Banach" $ testLawOf2 ([]::[(Vector 6 Float, Float)]) <$>
      banachLaws
    ]

testsMFloat :: TestTree
testsMFloat = testGroup "Matrix 4 3 Float"
    [ testGroup "Additive - Associative - Failure" $
      localOption (QuickCheckTests 1000) . testLawOf ([]::[Matrix 4 3 Float]) <$>
      additiveLawsFail
    , testGroup "Additive Group" $ testLawOf ([]::[Matrix 4 3 Float]) <$>
      additiveGroupLaws
    , testGroup "Multiplicative - Associative Failure" $
      localOption (QuickCheckTests 1000) . testLawOf ([]::[Matrix 4 3 Float]) <$>
      multiplicativeLawsFail
    , testGroup "MultiplicativeGroup" $ testLawOf ([]::[Matrix 4 3 Float]) <$>
      multiplicativeGroupLaws
    , testGroup "Distribution - Fail" $
      localOption (QuickCheckTests 1000) . testLawOf ([]::[Matrix 4 3 Float]) <$>
      distributionLawsFail
    , testGroup "Signed" $ testLawOf ([]::[Matrix 4 3 Float]) <$>
      signedLaws
    , testGroup "Metric" $ testLawOf ([]::[Matrix 4 3 Float]) <$>
      metricNaperianFloatLaws
    , testGroup "Exponential Field" $ testLawOf ([]::[Matrix 4 3 Float]) <$>
      expFieldNaperianLaws
    , testGroup "Additive Module" $
      localOption (QuickCheckTests 1000) .
      testLawOf2 ([]::[(Matrix 4 3 Float, Float)]) <$>
      additiveModuleLawsFail
    , testGroup "Additive Group Module" $
      localOption (QuickCheckTests 1000) .
      testLawOf2 ([]::[(Matrix 4 3 Float, Float)]) <$>
      additiveGroupModuleLawsFail
    , testGroup "Multiplicative Module" $
      localOption (QuickCheckTests 1000) .
      testLawOf2 ([]::[(Matrix 4 3 Float, Float)]) <$>
      multiplicativeModuleLawsFail
    , testGroup "Multiplicative Group Module" $
      localOption (QuickCheckTests 1000) .
      testLawOf2 ([]::[(Matrix 4 3 Float, Float)]) <$>
      multiplicativeGroupModuleLawsFail
    , testGroup "Additive Basis" $
      localOption (QuickCheckTests 1000) .
      testLawOf ([]::[Matrix 4 3 Float]) <$>
      additiveBasisLawsFail
    , testGroup "Additive Group Basis" $
      localOption (QuickCheckTests 1000) .
      testLawOf ([]::[Matrix 4 3 Float]) <$>
      additiveGroupBasisLaws
    , testGroup "Multiplicative Basis" $ localOption (QuickCheckTests 1000) .
      testLawOf ([]::[Matrix 4 3 Float]) <$>
      multiplicativeBasisLawsFail
    , testGroup "Multiplicative Group Basis" $ testLawOf ([]::[Matrix 4 3 Float]) <$>
      multiplicativeGroupBasisLaws
    ]

idempotentLaws ::
    ( Eq a
    , Additive a
    , Multiplicative a
    ) => [Law a]
idempotentLaws =
    [ ( "idempotent: a + a == a"
      , Unary (\a -> a + a == a))
    , ( "idempotent: a * a == a"
      , Unary (\a -> a * a == a))
    ]
 
additiveLaws ::
    ( Eq a
    , Additive a
    ) => [Law a]
additiveLaws =
    [ ( "associative: (a + b) + c = a + (b + c)"
      , Ternary (\a b c -> (a + b) + c == a + (b + c)))
    , ("left id: zero + a = a", Unary (\a -> zero + a == a))
    , ("right id: a + zero = a", Unary (\a -> a + zero == a))
    , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
    ]

additiveLawsApprox ::
    ( Eq a
    , Additive a
    , Epsilon a
    ) => [Law a]
additiveLawsApprox =
    [ ( "associative: (a + b) + c ≈ a + (b + c)"
      , Ternary (\a b c -> (a + b) + c ≈ a + (b + c)))
    , ("left id: zero + a = a", Unary (\a -> zero + a == a))
    , ("right id: a + zero = a", Unary (\a -> a + zero == a))
    , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
    ]

additiveLawsFail ::
    ( Eq a
    , Additive a
    , Show a
    , Arbitrary a
    ) => [Law a]
additiveLawsFail =
    [ ( "associative: (a + b) + c = a + (b + c)"
      , Failiary $ expectFailure . (\a b c -> (a + b) + c == a + (b + c)))
    , ("left id: zero + a = a", Unary (\a -> zero + a == a))
    , ("right id: a + zero = a", Unary (\a -> a + zero == a))
    , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
    ]

additiveGroupLaws ::
    ( Eq a
    , AdditiveGroup a
    ) => [Law a]
additiveGroupLaws =
    [ ("minus: a - a = zero", Unary (\a -> (a - a) == zero))
    , ("negate minus: negate a == zero - a", Unary (\a -> negate a == zero - a))
    , ("negate cancel: negate a + a == zero", Unary (\a -> negate a + a == zero))
    ]

multiplicativeLaws ::
    ( Eq a
    , Multiplicative a
    ) => [Law a]
multiplicativeLaws =
    [ ( "associative: (a * b) * c = a * (b * c)"
      , Ternary (\a b c -> (a * b) * c == a * (b * c)))
    , ("left id: one * a = a", Unary (\a -> one * a == a))
    , ("right id: a * one = a", Unary (\a -> a * one == a))
    , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
    ]

multiplicativeLawsApprox ::
    ( Eq a
    , Epsilon a
    , Multiplicative a
    ) => [Law a]
multiplicativeLawsApprox =
    [ ("associative: (a * b) * c ≈ a * (b * c)"
      , Ternary (\a b c -> (a * b) * c ≈ a * (b * c)))
    , ("left id: one * a = a", Unary (\a -> one * a == a))
    , ("right id: a * one = a", Unary (\a -> a * one == a))
    , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
    ]

multiplicativeLawsFail ::
    ( Eq a
    , Show a
    , Arbitrary a
    , Multiplicative a
    ) => [Law a]
multiplicativeLawsFail =
    [ ("associative: (a * b) * c = a * (b * c)"
      , Failiary $ expectFailure . (\a b c -> (a * b) * c == a * (b * c)))
    , ("left id: one * a = a", Unary (\a -> one * a == a))
    , ("right id: a * one = a", Unary (\a -> a * one == a))
    , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
    ]

multiplicativeGroupLaws ::
    ( Epsilon a
    , Eq a
    , MultiplicativeGroup a
    ) => [Law a]
multiplicativeGroupLaws =
    [ ( "divide: a == zero || a / a ≈ one", Unary (\a -> a == zero || (a / a) ≈ one))
    , ( "recip divide: recip a == one / a", Unary (\a -> a == zero || recip a == one / a))
    , ( "recip left: a == zero || recip a * a ≈ one"
      , Unary (\a -> a == zero || recip a * a ≈ one))
    , ( "recip right: a == zero || a * recip a ≈ one"
      , Unary (\a -> a == zero || a * recip a ≈ one))
    ]

distributionLaws ::
    ( Eq a
    , Distribution a
    ) => [Law a]
distributionLaws =
    [ ("annihilation: a * zero == zero", Unary (\a -> a `times` zero == zero))
    , ("left distributivity: a * (b + c) == a * b + a * c"
      , Ternary (\a b c -> a `times` (b + c) == a `times` b + a `times` c))
    , ("right distributivity: (a + b) * c == a * c + b * c"
      , Ternary (\a b c -> (a + b) `times` c == a `times` c + b `times` c))
    ]

distributionLawsApprox ::
    ( Epsilon a
    , Eq a
    , Distribution a
    ) => [Law a]
distributionLawsApprox =
    [ ("annihilation: a * zero == zero", Unary (\a -> a `times` zero == zero))
    , ("left distributivity: a * (b + c) ≈ a * b + a * c"
      , Ternary (\a b c -> a `times` (b + c) ≈ a `times` b + a `times` c))
    , ("right distributivity: (a + b) * c ≈ a * c + b * c"
      , Ternary (\a b c -> (a + b) `times` c ≈ a `times` c + b `times` c))
    ]

distributionLawsFail ::
    ( Show a
    , Arbitrary a
    , Epsilon a
    , Eq a
    , Distribution a
    ) => [Law a]
distributionLawsFail =
    [ ("annihilation: a * zero == zero", Unary (\a -> a `times` zero == zero))
    , ("left distributivity: a * (b + c) = a * b + a * c"
    , Failiary $ expectFailure .
      (\a b c -> a `times` (b + c) == a `times` b + a `times` c))
    , ("right distributivity: (a + b) * c = a * c + b * c"
    , Failiary $ expectFailure . (\a b c -> (a + b) `times` c == a `times` c + b `times` c))
    ]

signedLaws ::
    ( Eq a
    , Signed a
    ) => [Law a]
signedLaws =
    [ ("sign a * abs a == a", Unary (\a -> sign a `times` abs a == a))
    ]

integralLaws ::
    ( Eq a
    , Integral a
    , FromInteger a
    , ToInteger a
    ) => [Law a]
integralLaws =
    [ ( "integral divmod: b == zero || b * (a `div` b) + (a `mod` b) == a"
      , Binary (\a b -> b == zero || b `times` (a `div` b) + (a `mod` b) == a))
    , ( "fromIntegral a = a"
      , Unary (\a -> fromIntegral a == a))
    ]

boundedFieldLaws ::
    ( Eq a
    , BoundedField a
    ) => [Law a]
boundedFieldLaws =
    [ ("infinity laws"
      , Unary (\a ->
                  ((one :: Float)/zero + infinity == infinity) &&
                  (infinity + a == infinity) &&
                  isNaN ((infinity :: Float) - infinity) &&
                  isNaN ((infinity :: Float) / infinity) &&
                  isNaN (nan + a) &&
                  (zero :: Float)/zero /= nan))
    ]

prettyPositive :: (Epsilon a, Ord a) => a -> Bool
prettyPositive a = not (nearZero a) && a > zero

kindaPositive :: (Epsilon a, Ord a) => a -> Bool
kindaPositive a = nearZero a || a > zero

metricNaperianFloatLaws ::
    ( Naperian r
    , Metric (r Float) Float
    , Foldable r
    ) => [Law (r Float)]
metricNaperianFloatLaws =
    [ ( "positive"
      , Binary (\a b -> distance a b >= (zero::Float)))
    , ( "zero if equal"
      , Unary (\a -> distance a a == (zero::Float)))
    , ( "associative"
      , Binary (\a b -> distance a b ≈ (distance b a :: Float)))
    , ( "triangle rule - sum of distances > distance"
      , Ternary
        (\a b c ->
            kindaPositive
            (distance a c + distance b c - (distance a b :: Float)) &&
            kindaPositive
            (distance a b + distance b c - (distance a c :: Float)) &&
            kindaPositive
            (distance a b + distance a c - (distance b c :: Float))))
    ]

metricFloatLaws ::
    ( 
    ) => [Law Float]
metricFloatLaws =
    [ ( "positive"
      , Binary (\a b -> (distance a b :: Float) >= zero))
    , ("zero if equal"
      , Unary (\a -> (distance a a :: Float) == zero))
    , ( "associative"
      , Binary (\a b -> (distance a b :: Float) ≈ (distance b a :: Float)))
    , ( "triangle rule - sum of distances > distance"
      , Ternary (\a b c ->
                   (abs a > 10.0) ||
                   (abs b > 10.0) ||
                   (abs c > 10.0) ||
                   kindaPositive (distance a c + distance b c - (distance a b :: Float)) &&
                   kindaPositive (distance a b + distance b c - (distance a c :: Float)) &&
                   kindaPositive (distance a b + distance a c - (distance b c :: Float))))
    ]

metricComplexFloatLaws ::
    ( 
    ) => [Law (Complex Float)]
metricComplexFloatLaws =
    [ ( "positive"
      , Binary (\a b -> (distance a b :: Float) >= zero))
    ,
      ("zero if equal"
      , Unary (\a -> (distance a a :: Float) == zero))
    , ( "associative"
      , Binary (\a b -> (distance a b :: Float) ≈ (distance b a :: Float)))
    , ( "triangle rule - sum of distances > distance"
      , Ternary (\a b c ->
                   (size a > (10.0 :: Float)) ||
                   (size b > (10.0 :: Float)) ||
                   (size c > (10.0 :: Float)) ||
                   kindaPositive (distance a c + distance b c - (distance a b :: Float)) &&
                   kindaPositive (distance a b + distance b c - (distance a c :: Float)) &&
                   kindaPositive (distance a b + distance a c - (distance b c :: Float))))

      ]

quotientFieldLaws ::
    ( Ord a
    , Field a
    , QuotientField a
    , FromInteger a
    ) => [Law a]
quotientFieldLaws =
    [ ("x-1 < floor <= x <= ceiling < x+1"
      , Unary (\a ->
                  ((a - one) < fromIntegral (floor a)) &&
                  (fromIntegral (floor a) <= a) &&
                  (a <= fromIntegral (ceiling a)) &&
                  (fromIntegral (ceiling a) < a + one)))
    , ("round == floor (x + 1/2)"
      , Unary (\a -> round a == floor (a + one/(one+one))
              ))
    ]

expFieldLaws ::
    ( ExpField a
    , Epsilon a
    , Fractional a
    , Ord a
    ) => [Law a]
expFieldLaws =
    [ ("sqrt . (**2) ≈ id"
      , Unary (\a -> not (prettyPositive a) || (a > 10.0) ||
                    (sqrt . (**(one+one)) $ a) ≈ a &&
                    ((**(one+one)) . sqrt $ a) ≈ a))
    , ("log . exp ≈ id"
      , Unary (\a -> not (prettyPositive a) || (a > 10.0) ||
                    (log . exp $ a) ≈ a &&
                    (exp . log $ a) ≈ a))
    , ("for +ive b, a != 0,1: a ** logBase a b ≈ b"
      , Binary (\a b ->
                  ( not (prettyPositive b) ||
                    not (nearZero (a - zero)) ||
                    (a == one) ||
                    (a == zero && nearZero (logBase a b)) ||
                    (a ** logBase a b ≈ b))))
    ]

expFieldNaperianLaws ::
    ( Naperian r
    , Additive (r a)
    , ExpField (r a)
    , Foldable r
    , ExpField a
    , Epsilon a
    , Epsilon (r a)
    , Fractional a
    , Ord a
    ) => [Law (r a)]
expFieldNaperianLaws =
    [ ("sqrt . (**2) ≈ id"
      , Unary (\a -> not (all prettyPositive a) || any (>10.0) a ||
                    (sqrt . (**(one+one)) $ a) ≈ a &&
                    ((**(one+one)) . sqrt $ a) ≈ a))
    , ("log . exp ≈ id"
      , Unary (\a -> not (all prettyPositive a) || any (>10.0) a ||
                    (log . exp $ a) ≈ a &&
                    (exp . log $ a) ≈ a))
    , ("for +ive b, a != 0,1: a ** logBase a b ≈ b"
      , Binary (\a b ->
                  ( not (all prettyPositive b) ||
                    not (all nearZero a) ||
                    all (==one) a ||
                    (all (==zero) a && all nearZero (logBase a b)) ||
                    (a ** logBase a b ≈ b))))
    ]

additiveModuleLaws ::
    ( Eq (r a)
    , Naperian r
    , Additive (r a)
    , Epsilon a
    , Epsilon (r a)
    , Foldable r
    , AdditiveModule r a
    ) => [Law2 (r a) a]
additiveModuleLaws =
    [ 
      ("additive module associative: (a + b) .+ c ≈ a + (b .+ c)"
        , Ternary2 (\a b c -> (a + b) .+ c ≈ a + (b .+ c)))
    , ("additive module commutative: (a + b) .+ c ≈ (a .+ c) + b"
        , Ternary2 (\a b c -> (a + b) .+ c ≈ (a .+ c) + b))
    , ("additive module unital: a .+ zero == a"
        , Unary2 (\a -> a .+ zero == a))
    , ("module additive equivalence: a .+ b ≈ b +. a"
        , Binary2 (\a b -> a .+ b ≈ b +. a))
    ]

additiveModuleLawsFail ::
    ( Eq (r a)
    , Show a
    , Arbitrary a
    , Naperian r
    , Show (r a)
    , Arbitrary (r a)
    , Epsilon a
    , Additive (r a)
    , AdditiveModule r a
    ) => [Law2 (r a) a]
additiveModuleLawsFail =
    [ 
      ("additive module associative: (a + b) .+ c == a + (b .+ c)"
        , Failiary2 $ expectFailure . (\a b c -> (a + b) .+ c == a + (b .+ c)))
    , ("additive module commutative: (a + b) .+ c == (a .+ c) + b"
        , Failiary2 $ expectFailure . (\a b c -> (a + b) .+ c == (a .+ c) + b))
    , ("additive module unital: a .+ zero == a"
        , Unary2 (\a -> a .+ zero == a))
    , ("module additive equivalence: a .+ b == b +. a"
        , Binary2 (\a b -> a .+ b == b +. a))
    ]

additiveGroupModuleLaws ::
    ( Eq (r a)
    , Epsilon a
    , Epsilon (r a)
    , Foldable r
    , Naperian r
    , Additive (r a)
    , AdditiveGroup (r a)
    , AdditiveGroupModule r a
    ) => [Law2 (r a) a]
additiveGroupModuleLaws =
    [ 
      ("additive group module associative: (a + b) .- c ≈ a + (b .- c)"
        , Ternary2 (\a b c -> (a + b) .- c ≈ a + (b .- c)))
    , ("additive group module commutative: (a + b) .- c ≈ (a .- c) + b"
        , Ternary2 (\a b c -> (a + b) .- c ≈ (a .- c) + b))
    , ("additive group module unital: a .- zero == a"
        , Unary2 (\a -> a .- zero == a))
    , ("additive group module basis unital: a .- zero ≈ pureRep a"
        , Binary2 (\a b -> b -. (a-a) ≈ pureRep b))
    , ("module additive group equivalence: a .- b ≈ negate b +. a"
        , Binary2 (\a b -> a .- b ≈ negate b +. a))
    ]

additiveGroupModuleLawsFail ::
    ( Eq (r a)
    , Show a
    , Arbitrary a
    , Show (r a)
    , Arbitrary (r a)
    , Epsilon a
    , Epsilon (r a)
    , Foldable r
    , Naperian r
    , Additive (r a)
    , AdditiveGroup (r a)
    , AdditiveGroupModule r a
    ) => [Law2 (r a) a]
additiveGroupModuleLawsFail =
    [ 
      ("additive group module associative: (a + b) .- c == a + (b .- c)"
        , Failiary2 $ expectFailure . (\a b c -> (a + b) .- c == a + (b .- c)))
    , ("additive group module commutative: (a + b) .- c == (a .- c) + b"
        , Failiary2 $ expectFailure . (\a b c -> (a + b) .- c == (a .- c) + b))
    , ("additive group module unital: a .- zero == a"
        , Unary2 (\a -> a .- zero == a))
    , ("additive group module basis unital: a .- zero == pureRep a"
        , Binary2 (\a b -> b -. (a-a) == pureRep b))
    , ("module additive group equivalence: a .- b ≈  negate b +. a"
        , Binary2 (\a b -> a .- b ≈ negate b +. a))
    ]

multiplicativeModuleLaws ::
    ( Eq (r a)
    , Epsilon a
    , Epsilon (r a)
    , Foldable r
    , Naperian r
    , Additive (r a)
    , Multiplicative (r a)
    , AdditiveModule r a
    , MultiplicativeModule r a
    ) => [Law2 (r a) a]
multiplicativeModuleLaws =
    [ ("multiplicative module associative: (a * b) .* c ≈ a * (b .* c)"
        , Ternary2 (\a b c -> (a * b) .* c ≈ a * (b .* c)))
    , ("multiplicative module commutative: (a * b) .* c ≈ (a .* c) * b"
        , Ternary2 (\a b c -> (a * b) .* c ≈ a * (b .* c)))
    , ("multiplicative module unital: a .* one == a"
        , Unary2 (\a -> a .* one == a))
    , ("module right distribution: (a + b) .* c ≈ (a .* c) + (b .* c)"
        , Ternary2 (\a b c -> (a + b) .* c ≈ (a .* c) + (b .* c)))
    , ("module left distribution: c *. (a + b) ≈ (c *. a) + (c *. b)"
        , Ternary2 (\a b c -> c *. (a + b) ≈ (c *. a) + (c *. b)))
    , ("annihilation: a .* zero == zero", Unary2 (\a -> a .* zero == zero))
    , ("module multiplicative equivalence: a .* b ≈ b *. a"
        , Binary2 (\a b -> a .* b ≈ b *. a))
    ]

multiplicativeModuleLawsFail ::
    ( Eq (r a)
    , Epsilon a
    , Epsilon (r a)
    , Show a
    , Arbitrary a
    , Show (r a)
    , Arbitrary (r a)
    , Foldable r
    , Naperian r
    , Additive (r a)
    , Multiplicative (r a)
    , AdditiveModule r a
    , MultiplicativeModule r a
    ) => [Law2 (r a) a]
multiplicativeModuleLawsFail =
    [ ("multiplicative module associative: (a * b) .* c == a * (b .* c)"
        , Failiary2 $ expectFailure . (\a b c -> (a * b) .* c == a * (b .* c)))
    , ("multiplicative module commutative: (a * b) .* c == (a .* c) * b"
        , Failiary2 $ expectFailure . (\a b c -> (a * b) .* c == a * (b .* c)))
    , ("multiplicative module unital: a .* one == a"
        , Unary2 (\a -> a .* one == a))
    , ("module right distribution: (a + b) .* c == (a .* c) + (b .* c)"
        , Failiary2 $ expectFailure . (\a b c -> (a + b) .* c == (a .* c) + (b .* c)))
    , ("module left distribution: c *. (a + b) == (c *. a) + (c *. b)"
        , Failiary2 $ expectFailure . (\a b c -> c *. (a + b) == (c *. a) + (c *. b)))
    , ("annihilation: a .* zero == zero", Unary2 (\a -> a .* zero == zero))
    , ("module multiplicative equivalence: a .* b ≈ b *. a"
        , Binary2 (\a b -> a .* b ≈ b *. a))
    ]

multiplicativeGroupModuleLaws ::
    ( Eq (r a)
    , Eq a
    , Epsilon a
    , Epsilon (r a)
    , Foldable r
    , Naperian r
    , AdditiveUnital (r a)
    , Multiplicative (r a)
    , MultiplicativeGroup (r a)
    , MultiplicativeGroupModule r a
    ) => [Law2 (r a) a]
multiplicativeGroupModuleLaws =
    [ 
      ("multiplicative group module associative: (a * b) ./ c ≈ a * (b ./ c)"
        , Ternary2 (\a b c -> c==zero || (a * b) ./ c ≈ a * (b ./ c)))
    , ("multiplicative group module commutative: (a * b) ./ c ≈ (a ./ c) * b"
        , Ternary2 (\a b c -> c==zero || (a * b) ./ c ≈ (a ./ c) * b))
    , ("multiplicative group module unital: a ./ one == a"
        , Unary2 (\a -> nearZero a || a ./ one == a))
    , ("multiplicative group module basis unital: a /. one ≈ pureRep a"
        , Binary2 (\a b -> a==zero || b /. (a/a) ≈ pureRep b))
    , ("module multiplicative group equivalence: a ./ b ≈ recip b *. a"
        , Binary2 (\a b -> b==zero || a ./ b ≈ recip b *. a))
    ]

multiplicativeGroupModuleLawsFail ::
    ( Eq a
    , Show a
    , Arbitrary a
    , Eq (r a)
    , Show (r a)
    , Arbitrary (r a)
    , Epsilon a
    , Epsilon (r a)
    , Foldable r
    , Naperian r
    , AdditiveUnital (r a)
    , Multiplicative (r a)
    , MultiplicativeGroup (r a)
    , MultiplicativeGroupModule r a
    ) => [Law2 (r a) a]
multiplicativeGroupModuleLawsFail =
    [ 
      ("multiplicative group module associative: (a * b) ./ c == a * (b ./ c)"
        , Failiary2 $ expectFailure .
          (\a b c -> c==zero || (a * b) ./ c == a * (b ./ c)))
    , ("multiplicative group module commutative: (a * b) ./ c == (a ./ c) * b"
        , Failiary2 $ expectFailure .
          (\a b c -> c==zero || (a * b) ./ c == (a ./ c) * b))
    , ("multiplicative group module unital: a ./ one == a"
        , Unary2 (\a -> nearZero a || a ./ one == a))
    , ("multiplicative group module basis unital: a /. one == pureRep a"
        , Failiary2 $ expectFailure .
          (\a b -> a==zero || b /. (a/a) == pureRep b))
    , ("module multiplicative group equivalence: a ./ b ≈ recip b *. a"
        , Binary2 (\a b -> b==zero || a ./ b ≈ recip b *. a))
    ]

additiveBasisLaws ::
    ( Eq (r a)
    , Foldable r
    , Epsilon a
    , Epsilon (r a)
    , Naperian r
    , AdditiveUnital (r a)
    , AdditiveBasis r a
    ) => [Law (r a)]
additiveBasisLaws =
    [ ( "associative: (a .+. b) .+. c ≈ a .+. (b .+. c)"
      , Ternary (\a b c -> (a .+. b) .+. c ≈ a .+. (b .+. c)))
    , ("left id: zero .+. a = a", Unary (\a -> zero .+. a == a))
    , ("right id: a .+. zero = a", Unary (\a -> a .+. zero == a))
    , ("commutative: a .+. b == b .+. a", Binary (\a b -> a .+. b == b .+. a))
    ]

additiveBasisLawsFail ::
    ( Eq (r a)
    , Arbitrary (r a)
    , Show (r a)
    , Foldable r
    , Epsilon a
    , Naperian r
    , Epsilon (r a)
    , AdditiveUnital (r a)
    , AdditiveBasis r a
    ) => [Law (r a)]
additiveBasisLawsFail =
    [ ( "associative: (a .+. b) .+. c ≈ a .+. (b .+. c)"
      , Failiary $ expectFailure .
        (\a b c -> (a .+. b) .+. c ≈ a .+. (b .+. c)))
    , ("left id: zero .+. a = a", Unary (\a -> zero .+. a == a))
    , ("right id: a .+. zero = a", Unary (\a -> a .+. zero == a))
    , ("commutative: a .+. b == b .+. a", Binary (\a b -> a .+. b == b .+. a))
    ]

additiveGroupBasisLaws ::
    ( Eq (r a)
    , AdditiveGroupBasis r a
    ) => [Law (r a)]
additiveGroupBasisLaws =
    [ ("minus: a .-. a = pureRep zero", Unary (\a -> (a .-. a) == pureRep zero))
    ]

multiplicativeBasisLaws ::
    ( Eq (r a)
    , Naperian r
    , Multiplicative (r a)
    , MultiplicativeBasis r a
    ) => [Law (r a)]
multiplicativeBasisLaws =
    [ ("associative: (a .*. b) .*. c == a .*. (b .*. c)"
      , Ternary (\a b c -> (a .*. b) .*. c == a .*. (b .*. c)))
    , ("left id: one .*. a = a", Unary (\a -> one .*. a == a))
    , ("right id: a .*. one = a", Unary (\a -> a .*. one == a))
    , ("commutative: a .*. b == b .*. a", Binary (\a b -> a .*. b == b * a))
    ]

multiplicativeBasisLawsFail ::
    ( Eq (r a)
    , Show (r a)
    , Arbitrary (r a)
    , Naperian r
    , Multiplicative (r a)
    , MultiplicativeBasis r a
    ) => [Law (r a)]
multiplicativeBasisLawsFail =
    [ ("associative: (a .*. b) .*. c == a .*. (b .*. c)"
      , Failiary $ expectFailure . (\a b c -> (a .*. b) .*. c == a .*. (b .*. c)))
    , ("left id: one .*. a = a", Unary (\a -> one .*. a == a))
    , ("right id: a .*. one = a", Unary (\a -> a .*. one == a))
    , ("commutative: a .*. b == b .*. a", Binary (\a b -> a .*. b == b * a))
    ]

multiplicativeGroupBasisLaws ::
    ( Eq (r a)
    , Epsilon a
    , Epsilon (r a)
    , Foldable r
    , Naperian r
    , MultiplicativeGroupBasis r a
    ) => [Law (r a)]
multiplicativeGroupBasisLaws =
    [ ("minus: a ./. a ≈ pureRep one", Unary (\a -> a==pureRep zero || (a ./. a) ≈ pureRep one))
    ]

banachLaws ::
    ( Eq (r a)
    , Epsilon b
    , MultiplicativeGroup b
    , Banach r a
    , Normed (r a) b
    ) => [Law2 (r a) b]
banachLaws =
    [ -- Banach
      ( "size (normalize a) ≈ one"
      , Binary2 (\a b -> a==pureRep zero || size (normalize a) ≈ (b/b)))
    ]
