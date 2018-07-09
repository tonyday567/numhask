{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module NumHask.Laws
  ( LawArity(..)
  , LawArity2(..)
  , Law
  , Law2
  , testLawOf
  , testLawOf2
  , idempotentLaws
  , additionLaws
  , additionLaws_
  , additionLawsFail
  , additionGroupLaws
  , multiplicationLaws
  , multiplicationLawsFail
  , multiplicationGroupLaws
  , multiplicationGroupLaws_
  , distributionLaws
  , distributionLawsFail
  , integralLaws
  , rationalLaws
  , signedLaws
  , normedLaws
  , normedBoundedLaws
  , metricIntegralLaws
  , metricIntegralBoundedLaws
  , metricRationalLaws
  , upperBoundedFieldLaws
  , lowerBoundedFieldLaws
  , quotientFieldLaws 
  , expFieldLaws
  , additionBasisLaws
  , additionGroupBasisLaws
  , multiplicationBasisLaws
  , multiplicationGroupBasisLaws
  , additionModuleLaws
  , additionGroupModuleLaws
  , multiplicationModuleLaws
  , multiplicationGroupModuleLawsFail
  , expFieldContainerLaws
  , tensorProductLaws
  , banachLaws
  , hilbertLaws
  , semiringLaws
  , ringLaws
  , starSemiringLaws
  , involutiveRingLaws
  , integralsLaws
  ) where

import NumHask.Prelude
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty (TestName, TestTree)

smallRational :: (FromRatio a) => a
smallRational = 10.0

smallRationalPower :: (FromRatio a) => a
smallRationalPower = 6.0

smallIntegralPower :: (FromInteger a) => a
smallIntegralPower = 6

-- | unification of law equations
data LawArity a
  = Nonary Bool
  | Unary (a -> Bool)
  | Binary (a -> a -> Bool)
  | Ternary (a -> a -> a -> Bool)
  | Ornary (a -> a -> a -> a -> Bool)
  | Failiary (a -> Property)

type Law a = (TestName, LawArity a)

-- | unification of law equations with 2 types
data LawArity2 a b
  = Unary10 (a -> Bool)
  | Unary01 (b -> Bool)
  | Binary11 (a -> b -> Bool)
  | Binary20 (a -> a -> Bool)
  | Ternary21 (a -> a -> b -> Bool)
  | Ternary12 (a -> b -> b -> Bool)
  | Ternary30 (a -> a -> a -> Bool)
  | Quad31 (a -> a -> a -> b -> Bool)
  | Quad22 (a -> a -> b -> b -> Bool)
  | Failiary2 (a -> Property)

type Law2 a b = (TestName, LawArity2 a b)

testLawOf :: (Arbitrary a, Show a) => [a] -> Law a -> TestTree
testLawOf _ (name, Nonary f) = testProperty name f
testLawOf _ (name, Unary f) = testProperty name f
testLawOf _ (name, Binary f) = testProperty name f
testLawOf _ (name, Ternary f) = testProperty name f
testLawOf _ (name, Ornary f) = testProperty name f
testLawOf _ (name, Failiary f) = testProperty name f

testLawOf2 ::
     (Arbitrary a, Show a, Arbitrary b, Show b)
  => [(a, b)]
  -> Law2 a b
  -> TestTree
testLawOf2 _ (name, Unary10 f) = testProperty name f
testLawOf2 _ (name, Unary01 f) = testProperty name f
testLawOf2 _ (name, Binary11 f) = testProperty name f
testLawOf2 _ (name, Binary20 f) = testProperty name f
testLawOf2 _ (name, Ternary21 f) = testProperty name f
testLawOf2 _ (name, Ternary12 f) = testProperty name f
testLawOf2 _ (name, Ternary30 f) = testProperty name f
testLawOf2 _ (name, Quad22 f) = testProperty name f
testLawOf2 _ (name, Quad31 f) = testProperty name f
testLawOf2 _ (name, Failiary2 f) = testProperty name f

-- idempotent
idempotentLaws :: (Eq a, Addition a, Multiplication a) => [Law a]
idempotentLaws =
  [ ("idempotent: a + a == a", Unary (\a -> a + a == a))
  , ("idempotent: a * a == a", Unary (\a -> a * a == a))
  ]

-- | addition
additionLaws :: (Eq a, Addition a) => [Law a]
additionLaws =
  [ ( "associative: (a + b) + c = a + (b + c)"
    , Ternary (\a b c -> (a + b) + c == a + (b + c)))
  , ("left id: zero + a = a", Unary (\a -> zero + a == a))
  , ("right id: a + zero = a", Unary (\a -> a + zero == a))
  , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
  ]

-- | addition with approximate association equality
additionLaws_ :: (Epsilon a, Addition a) => [Law a]
additionLaws_ =
  [ ( "associative: (a + b) + c ≈ a + (b + c)"
    , Ternary (\a b c -> (a + b) + c ~= a + (b + c)))
  , ("left id: zero + a = a", Unary (\a -> zero + a == a))
  , ("right id: a + zero = a", Unary (\a -> a + zero == a))
  , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
  ]

-- | addition laws with a failure on association
additionLawsFail :: (Eq a, Addition a, Show a, Arbitrary a) => [Law a]
additionLawsFail =
  [ ( "associative: (a + b) + c = a + (b + c)"
    , Failiary $ expectFailure . (\a b c -> (a + b) + c == a + (b + c)))
  , ("left id: zero + a = a", Unary (\a -> zero + a == a))
  , ("right id: a + zero = a", Unary (\a -> a + zero == a))
  , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
  ]

additionGroupLaws :: (Eq a, AbelianGroup (Sum a)) => [Law a]
additionGroupLaws =
  [ ("minus: a - a = zero", Unary (\a -> (a - a) == zero))
  , ("negate minus: negate a == zero - a", Unary (\a -> negate a == zero - a))
  , ( "negate left cancel: negate a + a == zero"
    , Unary (\a -> negate a + a == zero))
  , ( "negate right cancel: negate a + a == zero"
    , Unary (\a -> a + negate a == zero))
  ]

-- multiplication
multiplicationLaws :: (Eq a, Multiplication a) => [Law a]
multiplicationLaws =
  [ ( "associative: (a * b) * c = a * (b * c)"
    , Ternary (\a b c -> (a * b) * c == a * (b * c)))
  , ("left id: one * a = a", Unary (\a -> one * a == a))
  , ("right id: a * one = a", Unary (\a -> a * one == a))
  , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
  ]

multiplicationLawsFail ::
     (Eq a, Show a, Arbitrary a, Multiplication a) => [Law a]
multiplicationLawsFail =
  [ ( "associative: (a * b) * c = a * (b * c)"
    , Failiary $ expectFailure . (\a b c -> (a * b) * c == a * (b * c)))
  , ("left id: one * a = a", Unary (\a -> one * a == a))
  , ("right id: a * one = a", Unary (\a -> a * one == a))
  , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
  ]

multiplicationGroupLaws :: (Eq a, Unital (Sum a), Absorbing (Product a), AbelianGroup (Product a)) => [Law a]
multiplicationGroupLaws =
  [ ( "divide: a == zero || a / a == one"
    , Unary (\a -> a == zero || (a / a) == one))
  , ( "recip divide: recip a == one / a"
    , Unary (\a -> a == zero || recip a == one / a))
  , ( "recip left: a == zero || recip a * a == one"
    , Unary (\a -> a == zero || recip a * a == one))
  , ( "recip right: a == zero || a * recip a == one"
    , Unary (\a -> a == zero || a * recip a == one))
  ]
 
multiplicationGroupLaws_ :: (Epsilon a, Absorbing (Product a), AbelianGroup (Product a)) => [Law a]
multiplicationGroupLaws_ =
  [ ( "divide: a == zero || a / a ~= one"
    , Unary (\a -> a == zero || (a / a) ~= one))
  , ( "recip divide: recip a == one / a"
    , Unary (\a -> a == zero || recip a == one / a))
  , ( "recip left: a == zero || recip a * a ~= one"
    , Unary (\a -> a == zero || recip a * a ~= one))
  , ( "recip right: a == zero || a * recip a ~= one"
    , Unary (\a -> a == zero || a * recip a ~= one))
  ]

-- distribution
distributionLaws :: (Eq a, Distributive a) => [Law a]
distributionLaws =
  [ ( "left annihilation: a * zero == zero"
    , Unary (\a -> a `times` zero == zero))
  , ( "right annihilation: zero * a == zero"
    , Unary (\a -> zero `times` a == zero))
  , ( "left distributivity: a * (b + c) == a * b + a * c"
    , Ternary (\a b c -> a `times` (b + c) == a `times` b + a `times` c))
  , ( "right distributivity: (a + b) * c == a * c + b * c"
    , Ternary (\a b c -> (a + b) `times` c == a `times` c + b `times` c))
  ]

distributionLawsFail ::
     (Show a, Arbitrary a, Epsilon a, Distributive a) => [Law a]
distributionLawsFail =
  [ ( "left annihilation: a * zero == zero"
    , Unary (\a -> a `times` zero == zero))
  , ( "right annihilation: a * zero == zero"
    , Unary (\a -> zero `times` a == zero))
  , ( "left distributivity: a * (b + c) = a * b + a * c"
    , Failiary $
      expectFailure . (\a b c -> a `times` (b + c) == a `times` b + a `times` c))
  , ( "right distributivity: (a + b) * c = a * c + b * c"
    , Failiary $
      expectFailure . (\a b c -> (a + b) `times` c == a `times` c + b `times` c))
  ]

-- integral
integralLaws :: (Eq a, Integral a, FromInteger a, ToInteger a) => [Law a]
integralLaws =
  [ ( "integral divmod: b == zero || b * (a `div` b) + (a `mod` b) == a"
    , Binary (\a b -> b == zero || b `times` (a `div` b) + (a `mod` b) == a))
  , ("fromIntegral a = a", Unary (\a -> fromIntegral a == a))
  ]

-- rational
rationalLaws :: (Eq a, FromRatio a, ToRatio a) => [Law a]
rationalLaws =
  [ ("fromRational a = a", Unary (\a -> fromRational a == a))
  ]

-- metric
signedLaws :: (Eq a, Signed a) => [Law a]
signedLaws = [("sign a * abs a == a", Unary (\a -> sign a `times` abs a == a))]

normedLaws :: forall a b. (Ord b, Unital (Sum a), Unital (Sum b), Unital (Product b), Normed a b) =>
  [Law2 a b]
normedLaws =
  [ ("positive", Binary11 (\a p -> p < (one :: b) || (normLp p a :: b) >= (zero :: b)))
  , ("preserves zero"
    , Binary11 (\_ p -> p < (one :: b) || (normLp p (zero :: a) :: b) == (zero :: b)) )
  ]

normedBoundedLaws :: forall a b. (Eq a, Bounded a, Ord b, Unital (Sum a), Unital (Sum b), Unital (Product b), Normed a b) =>
  [Law2 a b]
normedBoundedLaws =
  [ ("positive or non-minBound", Binary11 (\a p -> a == minBound || p < (one :: b) || (normLp p a :: b) >= (zero :: b)))
  , ("preserves zero"
    , Binary11 (\_ p -> p < (one :: b) || (normLp p (zero :: a) :: b) == (zero :: b)) )
  ]

metricIntegralLaws :: forall a b. (FromInteger b, Ord b, Signed b, Epsilon b, Metric a b, AbelianGroup (Sum b)) =>
  [Law2 a b]
metricIntegralLaws =
  [ ("Lp: positive",
     Ternary21 (\a b p -> p < one || distanceLp p a b >= zero))
  , ("Lp: zero if equal"
    , Binary11 (\a p -> p < one || distanceLp p a a == zero))
  , ( "Lp: associative"
    , Ternary21 (\a b p ->
                  p < one ||
                  p > (smallIntegralPower :: b) ||
                 distanceLp p a b ~= distanceLp p b a))
  , ( "Lp: triangle rule - sum of distances > distance"
    , Quad31
        (\a b c p ->
           (p < one) ||
           not
             (veryNegative
                (distanceLp p a c + distanceLp p b c - distanceLp p a b)) &&
           not
             (veryNegative
                (distanceLp p a b + distanceLp p b c - distanceLp p a c)) &&
           not
             (veryNegative
                (distanceLp p a b + distanceLp p a c - distanceLp p b c))))
  ]

-- triangle rule doesn't apply to bounded Integrals
metricIntegralBoundedLaws :: forall a b. (FromInteger b, Bounded b, Ord b, Signed b, Epsilon b, Metric a b) =>
  [Law2 a b]
metricIntegralBoundedLaws =
  [ ("Lp: positive",
     Ternary21 (\a b p -> p < one || distanceLp p a b >= zero || distanceLp p a b == minBound))
  , ("Lp: zero if equal"
    , Binary11 (\a p -> p < one || distanceLp p a a == zero))
  , ( "Lp: associative"
    , Ternary21 (\a b p ->
                  p < one ||
                  p > (smallIntegralPower :: b) ||
                 distanceLp p a b ~= distanceLp p b a))
  ]


metricRationalLaws :: forall a b. (FromRatio b, Ord b, Signed b, Epsilon b, Metric a b, Normed a b, Addition a, AbelianGroup (Sum b)) =>
  [Law2 a b]
metricRationalLaws =
  [ ("Lp: positive",
     Ternary21 (\a b p -> p < one || distanceLp p a b >= zero))
  , ("Lp: zero if equal"
    , Binary11 (\a p -> p < one || distanceLp p a a == zero))
  , ( "Lp: associative"
    , Ternary21 (\a b p ->
                  p < one ||
                  p > (smallRationalPower :: b) ||
                 distanceLp p a b ~= distanceLp p b a))
  , ( "Lp: triangle rule - sum of distances > distance"
    , Quad31
        (\a b c p ->
           (p < one) ||
           (normL1 a > (smallRational :: b)) ||
           (normL1 b > (smallRational :: b)) ||
           (normL1 c > (smallRational :: b)) ||
           not
             (veryNegative
                (distanceLp p a c + distanceLp p b c - distanceLp p a b)) &&
           not
             (veryNegative
                (distanceLp p a b + distanceLp p b c - distanceLp p a c)) &&
           not
             (veryNegative
                (distanceLp p a b + distanceLp p a c - distanceLp p b c))))
  ]

-- bounded fields
upperBoundedFieldLaws :: forall a. (Eq a, UpperBoundedField a) => [Law a]
upperBoundedFieldLaws =
  [ ( "upper bounded field (infinity) laws"
    , Unary
        (\a ->
           ((one ::a) / zero + infinity == infinity) &&
           (infinity + a == infinity) &&
           (zero :: a) / zero /= nan))
  ]

lowerBoundedFieldLaws :: forall a. (Eq a, LowerBoundedField a) => [Law a]
lowerBoundedFieldLaws =
  [ ( "lower bounded field (negative infinity) laws"
    , Unary
        (\a ->
           (negate (one ::a) / zero == negInfinity) &&
           ((negInfinity :: a) + negInfinity == negInfinity) &&
           (negInfinity + a == negInfinity)))
  ]

quotientFieldLaws :: (Field a, QuotientField a Integer, FromInteger a, Ord a) => [Law2 a Integer]
quotientFieldLaws =
  [ ( "a - one < floor a <= a <= ceiling a < a + one"
    , Unary10
      (\a ->
        ((a - one) < (fromInteger (floor a)))
          && (fromInteger (floor a) <= a)
          && (a <= fromInteger (ceiling a))
          && (fromInteger (ceiling a) < a + one)
      )
    )
  , ( "round a == floor (a + one/(one+one))"
    , Unary10
      (\a -> case even ((floor $ a + one / (one + one)) :: Integer) of
        True  -> ((round a :: Integer) == (floor $ a + (one / (one + one))))
        False -> ((round a :: Integer) == (ceiling $ a - (one / (one + one))))
      )
    )
  ] where
    sign' a
      | (floor a :: Integer) < 0 = -1
      | otherwise = 1

expFieldLaws :: forall a b.
     (FromInteger b, Unital (Sum b), ExpField a, Normed a b, Epsilon a, Ord a, Ord b) => [Law2 a b]
expFieldLaws =
  [ ( "sqrt . (**(one+one)) ~= id"
    , Unary10
        (\a ->
           not (a > (zero :: a)) ||
           (normL1 a > (10 :: b)) ||
           (sqrt . (** (one + one)) $ a) ~= a &&
           ((** (one + one)) . sqrt $ a) ~= a))
  , ( "log . exp ~= id"
    , Unary10
        (\a ->
           not (a > (zero :: a)) ||
           (normL1 a > (10 :: b)) || (log . exp $ a) ~= a && (exp . log $ a) ~= a))
  , ( "for +ive b, a != 0,1: a ** logBase a b ~= b"
    , Binary20
        (\a b ->
           (not (normL1 b > (zero :: b)) ||
            not (nearZero (a - zero)) ||
            (a == one) ||
            (a == zero && nearZero (logBase a b)) || (a ** logBase a b ~= b))))
  ]

expFieldContainerLaws ::
     ( ExpField (r a)
     , Foldable r
     , ExpField a
     , Epsilon a
     , Signed a
     , FromRatio a
     , Epsilon (r a)
     , Ord a
     )
  => [Law (r a)]
expFieldContainerLaws =
  [ ( "sqrt . (**2) ~= id"
    , Unary
        (\a ->
           not (all veryPositive a) ||
           any (> smallRational) a ||
           (sqrt . (** (one + one)) $ a) ~= a &&
           ((** (one + one)) . sqrt $ a) ~= a))
  , ( "log . exp ~= id"
    , Unary
        (\a ->
           not (all veryPositive a) ||
           any (> smallRational) a || (log . exp $ a) ~= a && (exp . log $ a) ~= a))
  , ( "for +ive b, a != 0,1: a ** logBase a b ~= b"
    , Binary
        (\a b ->
           (not (all veryPositive b) ||
            not (all nearZero a) ||
            all (== one) a ||
            (all (== zero) a && all nearZero (logBase a b)) ||
            (a ** logBase a b ~= b))))
  ]

-- module
additionModuleLaws ::
     (Epsilon a, Epsilon (r a), AdditiveModule r a, Addition (r a)) => [Law2 (r a) a]
additionModuleLaws =
  [ ( "addition module associative: (a + b) .+ c ~= a + (b .+ c)"
    , Ternary21 (\a b c -> (a + b) .+ c ~= a + (b .+ c)))
  , ( "addition module commutative: (a + b) .+ c ~= (a .+ c) + b"
    , Ternary21 (\a b c -> (a + b) .+ c ~= (a .+ c) + b))
  , ("addition module unital: a .+ zero == a", Unary10 (\a -> a .+ zero == a))
  , ( "module addition equivalence: a .+ b ~= b +. a"
    , Binary11 (\a b -> a .+ b ~= b +. a))
  ]

additionGroupModuleLaws ::
     (Epsilon a, Epsilon (r a), AdditiveGroupModule r a, Addition (r a))
  => [Law2 (r a) a]
additionGroupModuleLaws =
  [ ( "addition group module associative: (a + b) .- c ~= a + (b .- c)"
    , Ternary21 (\a b c -> (a + b) .- c ~= a + (b .- c)))
  , ( "addition group module commutative: (a + b) .- c ~= (a .- c) + b"
    , Ternary21 (\a b c -> (a + b) .- c ~= (a .- c) + b))
  , ( "addition group module unital: a .- zero == a"
    , Unary10 (\a -> a .- zero == a))
  , ( "module addition group equivalence: a .- b ~= negate b +. a"
    , Binary11 (\a b -> a .- b ~= negate b +. a))
  ]

multiplicationModuleLaws ::
     (Epsilon a, Epsilon (r a), Module r a, Addition (r a))
  => [Law2 (r a) a]
multiplicationModuleLaws =
  [ ( "multiplication module unital: a .* one == a"
    , Unary10 (\a -> a .* one == a))
  , ( "module right distribution: (a + b) .* c ~= (a .* c) + (b .* c)"
    , Ternary21 (\a b c -> (a + b) .* c ~= (a .* c) + (b .* c)))
  , ( "module left distribution: c *. (a + b) ~= (c *. a) + (c *. b)"
    , Ternary21 (\a b c -> c *. (a + b) ~= (c *. a) + (c *. b)))
  , ("annihilation: a .* zero == zero", Unary10 (\a -> a .* zero == zero))
  , ( "module multiplication equivalence: a .* b ~= b *. a"
    , Binary11 (\a b -> a .* b ~= b *. a))
  ]

multiplicationGroupModuleLawsFail ::
     ( Epsilon a
     , Epsilon (r a)
     , MultiplicativeGroupModule r a
     )
  => [Law2 (r a) a]
multiplicationGroupModuleLawsFail =
  [ ( "multiplication group module unital: a ./ one == a"
    , Unary10 (\a -> nearZero a || a ./ one == a))
  , ( "module multiplication group equivalence: a ./ b ~= recip b *. a"
    , Binary11 (\a b -> b == zero || a ./ b ~= recip b *. a))
  ]

banachLaws ::
     ( Foldable r
     , Epsilon (r a)
     , Banach r a
     , Signed a
     , FromRatio a
     , Ord a
     , Applicative r
     )
  => [Law2 (r a) a]
banachLaws =
  [ ( "L1: normalize a .* norm a ~= one"
    , Unary10
        (\a ->
           a == pure zero ||
           (any ((> smallRational) . abs) a || (normalizeL1 a .* normL1 a) ~= a)))
    , ( "L2: normalize a .* norm a ~= one"
    , Unary10
        (\a ->
           a == pure zero ||
           (any ((> smallRational) . abs) a || (normalizeL2 a .* normL2 a) ~= a)))
{-
    , ( "Lp: normalizeLp a p .* normLp a p ~= one"
    , Binary11
        (\a p ->
           a == pure zero ||
           (any ((> smallRational) . normL1) a || (normalizeLp p a .* normLp p a) ~= a)))
-}
  ]

hilbertLaws ::
    ( Module r a
    , Epsilon a
    , Epsilon (r a)
    , Hilbert r a
    , Addition (r a))
  => [Law2 (r a) a]
hilbertLaws =
  [ ("commutative a <.> b ~= b <.> a", Ternary21 (\a b _ -> a <.> b ~= b <.> a))
  , ( "distributive over addition a <.> (b + c) == a <.> b + a <.> c"
    , Ternary30 (\a b c -> a <.> (b + c) ~= a <.> b + a <.> c))
  , ( "bilinear a <.> (s *. b + c) == s * (a <.> b) + a <.> c"
    , Quad31 (\a b c s -> a <.> (s *. b + c) == s * (a <.> b) + a <.> c))
  , ( "scalar multiplication (s0 *. a) <.> (s1 *. b) == s0 * s1 * (a <.> b)"
    , Quad22 (\a b s0 s1 -> (s0 *. a) <.> (s1 *. b) == s0 * s1 * (a <.> b)))
  ]

tensorProductLaws ::
     ( Eq (r (r a))
     , Addition (r (r a))
     , TensorProduct (r a)
     , Epsilon (r a)
     , Addition (r a)
     )
  => [Law2 (r a) a]
tensorProductLaws =
  [ ( "left distribution over addition a><b + c><b == (a+c) >< b"
    , Ternary30 (\a b c -> a >< b + c >< b == (a + c) >< b))
  , ( "right distribution over addition a><b + a><c == a >< (b+c)"
    , Ternary30 (\a b c -> a >< b + a >< c == a >< (b + c)))
  -- , ( "left module tensor correspondance a *. (b><c) == (a><b) .* c"
  --   , Ternary30 (\a b c -> a *. (b><c) == (a><b) .* c))
  -- , ( "right module tensor correspondance (a><b) .* c == a *. (b><c)"
  --   , Ternary30 (\a b c -> (a><b) .* c == a *. (b><c)))
  ]

-- basis
additionBasisLaws :: (Epsilon (r a), AdditiveBasis r a) => [Law (r a)]
additionBasisLaws =
  [ ( "associative: (a .+. b) .+. c ~= a .+. (b .+. c)"
    , Ternary (\a b c -> (a .+. b) .+. c ~= a .+. (b .+. c)))
  , ("left id: zero .+. a = a", Unary (\a -> zero .+. a == a))
  , ("right id: a .+. zero = a", Unary (\a -> a .+. zero == a))
  , ("commutative: a .+. b == b .+. a", Binary (\a b -> a .+. b == b .+. a))
  ]

additionGroupBasisLaws :: (Eq (r a), Unital (Sum a), AdditiveGroupBasis r a, Applicative r) => [Law (r a)]
additionGroupBasisLaws =
  [ ( "minus: a .-. a = pure zero"
    , Unary (\a -> (a .-. a) == pure zero))
  ]

multiplicationBasisLaws :: (Eq (r a), HadamardMultiplication r a, Applicative r) => [Law (r a)]
multiplicationBasisLaws =
  [ ( "associative: (a .*. b) .*. c == a .*. (b .*. c)"
    , Ternary (\a b c -> (a .*. b) .*. c == a .*. (b .*. c)))
  , ("left id: pure one .*. a = a", Unary (\a -> pure one .*. a == a))
  , ( "right id: a .*. pure one = a"
    , Unary (\a -> a .*. pure one == a))
  , ("commutative: a .*. b == b .*. a", Binary (\a b -> a .*. b == b .*. a))
  ]

multiplicationGroupBasisLaws ::
     ( Epsilon a
     , Epsilon (r a)
     , HadamardDivision r a
     , Applicative r
     )
  => [Law (r a)]
multiplicationGroupBasisLaws =
  [ ( "basis divide: a ./. a ~= pure one"
    , Unary (\a -> a == pure zero || (a ./. a) ~= pure one))
  ]

-- | semiring
semiringLaws :: (Epsilon a, Semiring a) => [Law a]
semiringLaws = additionLaws <> distributionLaws <>
    [ ( "associative: (a * b) * c = a * (b * c)"
    , Ternary (\a b c -> (a `times` b) `times` c == a `times` (b `times` c)))
    , ("left id: one * a = a", Unary (\a -> one `times` a == a))
    , ("right id: a * one = a", Unary (\a -> a `times` one == a))
    ]

-- | ring
ringLaws :: (Epsilon a, Ring a) => [Law a]
ringLaws = semiringLaws <> additionGroupLaws

-- | starsemiring
starSemiringLaws :: (Epsilon a, StarSemiring a) => [Law a]
starSemiringLaws = semiringLaws <>
    [ ( "star law: star a == one + a `times` star a"
    , Unary (\a -> star a == one + a `times` star a))
    ]

-- | involutive ring
involutiveRingLaws :: forall a. (Eq a, Unital (Product a),InvolutiveRing a) => [Law a]
involutiveRingLaws =
    [ ( "adjoint plus law: adj (a + b) ==> adj a + adj b"
    , Binary (\a b -> adj (a `plus` b) == adj a `plus` adj b))
    , ( "adjoint times law: adj (a * b) ==> adj b * adj a"
    , Binary (\a b -> adj (a `times` b) == adj b `times` adj a))
    , ( "adjoint multiplication unit law: adj one ==> one"
    , Nonary (adj (one :: a) == one))
    , ( "adjoint own inverse law: adj (adj a) ==> a"
    , Unary (\a -> adj (adj a) == a))
    ]


-- integrals are the law groups that apply to Integral-like numbers
integralsLaws :: (Eq a, AbelianGroup a, Integral a, Signed a, ToInteger a, FromInteger a, Multiplication a, AbelianGroup (Sum a), AbelianGroup (Product a)) => [Law a]
integralsLaws =
  additionLaws <>
  additionGroupLaws <>
  multiplicationLaws <>
  distributionLaws <>
  integralLaws <>
  signedLaws


