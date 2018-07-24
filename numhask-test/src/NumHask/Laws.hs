{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module NumHask.Laws
  ( LawArity(..)
  , LawArity2(..)
  , Law
  , Law2
  , Laws(..)
  , testLawOf
  , testLawOf1
  , testLawOf2
  , idempotentLaws
  , additiveLaws 
  , additiveGroupLaws
  , multiplicativeLaws
  , multiplicativeMonoidalLaws
  , multiplicativeGroupLaws
  , distributiveLaws
  , integralLaws
  , rationalLaws
  , signedLaws
  , normedLaws
  , normedBoundedLaws
  , metricLaws
  , metricIntegralLaws
  , metricIntegralBoundedLaws
  , metricRationalLaws
  , upperBoundedFieldLaws
  , lowerBoundedFieldLaws
  , quotientFieldLaws
  , expFieldLaws
  , hadamardMultiplicationLaws
  , hadamardDivisionLaws
  , moduleLaws
  , expFieldContainerLaws
  , tensorProductLaws
  , banachLaws
  , hilbertLaws
  , semiringLaws
  , ringLaws
  , starSemiringLaws
  , involutiveRingLaws
  , integralsLaws
  , integralsUnboundedLaws
  , integralsBoundedLaws
  )
where

import NumHask.Prelude
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty (TestName, TestTree)

smallRational :: (FromRatio a) => a
smallRational = 10.0

smallIntegralPower :: (FromInteger a) => a
smallIntegralPower = 6

data Laws a b = Arity1 (Law a) | Arity2 (Law2 a b)

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

testLawOf :: (Arbitrary a, Show a, Arbitrary b, Show b) => [a] -> [(a,b)] -> Laws a b -> TestTree
testLawOf as _ (Arity1 law) = testLawOf1 as law
testLawOf _ ab (Arity2 law) = testLawOf2 ab law

testLawOf1 :: (Arbitrary a, Show a) => [a] -> Law a -> TestTree
testLawOf1 _ (name, Nonary f) = testProperty name f
testLawOf1 _ (name, Unary f) = testProperty name f
testLawOf1 _ (name, Binary f) = testProperty name f
testLawOf1 _ (name, Ternary f) = testProperty name f
testLawOf1 _ (name, Ornary f) = testProperty name f
testLawOf1 _ (name, Failiary f) = testProperty name f

testLawOf2
  :: (Arbitrary a, Show a, Arbitrary b, Show b)
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
idempotentLaws :: (Eq a, Additive a, Multiplicative a) => [Law a]
idempotentLaws =
  [ ("idempotent: a + a == a", Unary (\a -> a + a == a))
  , ("idempotent: a * a == a", Unary (\a -> a * a == a))
  ]

-- | additive
additiveLaws :: (Eq a, Additive a) => [Law a]
additiveLaws =
  [ ( "associative: (a + b) + c = a + (b + c)"
    , Ternary (\a b c -> (a + b) + c == a + (b + c))
    )
  , ("left id: zero + a = a", Unary (\a -> zero + a == a))
  , ("right id: a + zero = a", Unary (\a -> a + zero == a))
  , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
  ]

additiveGroupLaws :: (Eq a, AbelianGroup (Sum a)) => [Law a]
additiveGroupLaws =
  [ ("minus: a - a = zero", Unary (\a -> (a - a) == zero))
  , ("negate minus: negate a == zero - a", Unary (\a -> negate a == zero - a))
  , ( "negate left cancel: negate a + a == zero"
    , Unary (\a -> negate a + a == zero)
    )
  , ( "negate right cancel: negate a + a == zero"
    , Unary (\a -> a + negate a == zero)
    )
  ]

-- multiplicative
multiplicativeLaws :: (Eq a, Multiplicative a) => [Law a]
multiplicativeLaws =
  [ ( "associative: (a * b) * c = a * (b * c)"
    , Ternary (\a b c -> (a * b) * c == a * (b * c))
    )
  , ("left id: one * a = a", Unary (\a -> one * a == a))
  , ("right id: a * one = a", Unary (\a -> a * one == a))
  , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
  ]

multiplicativeMonoidalLaws :: (Eq a, Unital (Product a)) => [Law a]
multiplicativeMonoidalLaws =
  [ ( "associative: (a * b) * c = a * (b * c)"
    , Ternary (\a b c -> (a * b) * c == a * (b * c))
    )
  , ("left id: one * a = a", Unary (\a -> one * a == a))
  , ("right id: a * one = a", Unary (\a -> a * one == a))
  ]

multiplicativeGroupLaws
  :: (Eq a, Unital (Sum a), Absorbing (Product a), AbelianGroup (Product a))
  => [Law a]
multiplicativeGroupLaws =
  [ ( "divide: a == zero || a / a == one"
    , Unary (\a -> a == zero || (a / a) == one)
    )
  , ( "recip divide: recip a == one / a"
    , Unary (\a -> a == zero || recip a == one / a)
    )
  , ( "recip left: a == zero || recip a * a == one"
    , Unary (\a -> a == zero || recip a * a == one)
    )
  , ( "recip right: a == zero || a * recip a == one"
    , Unary (\a -> a == zero || a * recip a == one)
    )
  ]

-- distributive
distributiveLaws :: (Eq a, Distributive a) => [Law a]
distributiveLaws =
  [ ("left annihilation: a * zero == zero", Unary (\a -> a * zero == zero))
  , ("right annihilation: zero * a == zero", Unary (\a -> zero * a == zero))
  , ( "left distributivity: a * (b + c) == a * b + a * c"
    , Ternary (\a b c -> a * (b + c) == a * b + a * c)
    )
  , ( "right distributivity: (a + b) * c == a * c + b * c"
    , Ternary (\a b c -> (a + b) * c == a * c + b * c)
    )
  ]

-- integral
integralLaws :: (Eq a, Integral a, FromInteger a, ToInteger a) => [Law a]
integralLaws =
  [ ( "integral divmod: b == zero || b * (a `div` b) + (a `mod` b) == a"
    , Binary (\a b -> b == zero || b * (a `div` b) + (a `mod` b) == a)
    )
  , ("fromIntegral a = a", Unary (\a -> fromIntegral a == a))
  ]

-- rational
rationalLaws :: (Eq a, FromRatio a, ToRatio a) => [Law a]
rationalLaws = [("fromRational a = a", Unary (\a -> fromRational a == a))]

-- metric
signedLaws :: (Eq a, Signed a) => [Law a]
signedLaws = [("sign a * abs a == a", Unary (\a -> sign a * abs a == a))]

normedLaws
  :: forall a b
   . (Ord b, Unital (Sum a), Unital (Sum b), Unital (Product b), Normed a b)
  => [Law2 a b]
normedLaws =
  [ ( "positive"
    , Binary11 (\a p -> p < (one :: b) || (normLp p a :: b) >= (zero :: b))
    )
  , ( "preserves zero"
    , Binary11
      (\_ p -> p < (one :: b) || (normLp p (zero :: a) :: b) == (zero :: b))
    )
  ]

normedBoundedLaws
  :: forall a b
   . ( Eq a
     , Bounded a
     , Ord b
     , Unital (Sum a)
     , Unital (Sum b)
     , Unital (Product b)
     , Normed a b
     )
  => [Law2 a b]
normedBoundedLaws =
  [ ( "positive or non-minBound"
    , Binary11
      (\a p ->
        a == minBound || p < (one :: b) || (normLp p a :: b) >= (zero :: b)
      )
    )
  , ( "preserves zero"
    , Binary11
      (\_ p -> p < (one :: b) || (normLp p (zero :: a) :: b) == (zero :: b))
    )
  ]

metricIntegralLaws
  :: forall a b
   . ( FromInteger b
     , Ord b
     , Signed b
     , Epsilon b
     , Metric a b
     , AbelianGroup (Sum b)
     )
  => [Law2 a b]
metricIntegralLaws =
  [ ("Lp: positive", Ternary21 (\a b p -> p < one || distanceLp p a b >= zero))
  , ( "Lp: zero if equal"
    , Binary11 (\a p -> p < one || distanceLp p a a == zero)
    )
  , ( "Lp: associative"
    , Ternary21
      (\a b p ->
        p
          < one
          || p
          > (smallIntegralPower :: b)
          || distanceLp p a b
          == distanceLp p b a
      )
    )
  , ( "Lp: triangle rule - sum of distances > distance"
    , Quad31
      (\a b c p ->
        (p < one)
          || not
              (zero >
                (distanceLp p a c + distanceLp p b c - distanceLp p a b)
              )
          && not
              (zero >
                (distanceLp p a b + distanceLp p b c - distanceLp p a c)
              )
          && not
              (zero >
                (distanceLp p a b + distanceLp p a c - distanceLp p b c)
              )
      )
    )
  ]

metricLaws
  :: forall a b
   . ( Ord b
     , Signed b
     , Epsilon b
     , Metric a b
     , AbelianGroup (Sum b)
     )
  => [Law2 a b]
metricLaws =
  [ ("Lp: positive", Ternary21 (\a b p -> p < one || distanceLp p a b >= zero))
  , ( "Lp: zero if equal"
    , Binary11 (\a p -> p < one || distanceLp p a a == zero)
    )
  , ( "Lp: associative"
    , Ternary21
      (\a b p ->
        (p < one) || distanceLp p a b == distanceLp p b a
      )
    )
  , ( "Lp: triangle rule - sum of distances > distance"
    , Quad31
      (\a b c p ->
        (p < one) ||
        not (zero >
            (distanceLp p a c + distanceLp p b c - distanceLp p a b)
          ) &&
        not (zero >
            (distanceLp p a b + distanceLp p b c - distanceLp p a c)
          ) &&
        not (zero >
            (distanceLp p a b + distanceLp p a c - distanceLp p b c)
          )
      )
    )
  ]


-- triangle rule doesn't apply to bounded Integrals
metricIntegralBoundedLaws
  :: forall a b
   . (FromInteger b, Bounded b, Ord b, Signed b, Epsilon b, Metric a b)
  => [Law2 a b]
metricIntegralBoundedLaws =
  [ ( "Lp: positive"
    , Ternary21
      (\a b p ->
        p < one || distanceLp p a b >= zero || distanceLp p a b == minBound
      )
    )
  , ( "Lp: zero if equal"
    , Binary11 (\a p -> p < one || distanceLp p a a == zero)
    )
  , ( "Lp: associative"
    , Ternary21
      (\a b p ->
        (p < one)
          || (p > (smallIntegralPower :: b))
          || (distanceLp p a b == distanceLp p b a)
      )
    )
  ]

metricRationalLaws
  :: forall a b
   . ( FromRatio b
     , Ord b
     , Signed b
     , Epsilon b
     , Metric a b
     , Normed a b
     , AbelianGroup (Sum b)
     )
  => [Law2 a b]
metricRationalLaws =
  [ ("Lp: positive", Ternary21 (\a b p -> p < one || distanceLp p a b >= zero))
  , ( "Lp: zero if equal"
    , Binary11 (\a p -> p < one || distanceLp p a a == zero)
    )
  , ( "Lp: associative"
    , Ternary21
      (\a b p ->
        p
          < one
          || p
          > (smallRationalPower :: b)
          || distanceLp p a b
          == distanceLp p b a
      )
    )
  , ( "Lp: triangle rule - sum of distances > distance"
    , Quad31
      (\a b c p ->
        (p < one)
          || (normL1 a > (smallRational :: b))
          || (normL1 b > (smallRational :: b))
          || (normL1 c > (smallRational :: b))
          || not
              (zero >
                (distanceLp p a c + distanceLp p b c - distanceLp p a b)
              )
          && not
              (zero >
                (distanceLp p a b + distanceLp p b c - distanceLp p a c)
              )
          && not
              (zero >
                (distanceLp p a b + distanceLp p a c - distanceLp p b c)
              )
      )
    )
  ] where
  smallRationalPower = 6.0

-- bounded fields
upperBoundedFieldLaws :: forall a . (Eq a, UpperBoundedField a) => [Law a]
upperBoundedFieldLaws =
  [ ( "upper bounded field (infinity) laws"
    , Unary
      (\a ->
        ((one :: a) / zero + infinity == infinity)
          && (infinity + a == infinity)
          && ((zero :: a) / zero /= nan)
      )
    )
  ]

lowerBoundedFieldLaws :: forall a . (Eq a, LowerBoundedField a) => [Law a]
lowerBoundedFieldLaws =
  [ ( "lower bounded field (negative infinity) laws"
    , Unary
      (\a ->
        (negate (one :: a) / zero == negInfinity)
          && ((negInfinity :: a) + negInfinity == negInfinity)
          && (negInfinity + a == negInfinity)
      )
    )
  ]

quotientFieldLaws
  :: (Field a, QuotientField a Integer, FromInteger a, Ord a)
  => [Law2 a Integer]
quotientFieldLaws =
  [ ( "a - one < floor a <= a <= ceiling a < a + one"
    , Unary10
      (\a ->
        ((a - one) < fromInteger (floor a))
          && (fromInteger (floor a) <= a)
          && (a <= fromInteger (ceiling a))
          && (fromInteger (ceiling a) < a + one)
      )
    )
  , ( "round a == floor (a + one/(one+one))"
    , Unary10
      (\a -> case even ((floor $ a + one / (one + one)) :: Integer) of
        True -> (round a :: Integer) == floor (a + (one / (one + one)))
        False -> (round a :: Integer) == ceiling (a - (one / (one + one)))
      )
    )
  ]

expFieldLaws
  :: forall a b
   . ( FromInteger b
     , Unital (Sum b)
     , ExpField a
     , Normed a b
     , Epsilon a
     , Ord a
     , Ord b
     )
  => [Law2 a b]
expFieldLaws =
  [ ( "sqrt . (**(one+one)) == id"
    , Unary10
      (\a ->
        not (a > (zero :: a))
          || (normL1 a > (10 :: b))
          || ((sqrt . (** (one + one)) $ a) == a)
          && (((** (one + one)) . sqrt $ a) == a)
      )
    )
  , ( "log . exp ~= id"
    , Unary10
      (\a ->
        not (a > (zero :: a))
          || (normL1 a > (10 :: b))
          || ((log . exp $ a) == a)
          && ((exp . log $ a) == a)
      )
    )
  , ( "for +ive b, a != 0,1: a ** logBase a b == b"
    , Binary20
      (\a b ->
        (not (normL1 b > (zero :: b))
        || not (nearZero (a - zero))
        || (a == one)
        || (a == zero && nearZero (logBase a b))
        || (a ** logBase a b == b)
        )
      )
    )
  ]

expFieldContainerLaws
  :: ( ExpField (r a)
     , Foldable r
     , ExpField a
     , Epsilon a
     , FromRatio a
     , Epsilon (r a)
     , Ord a
     )
  => [Law (r a)]
expFieldContainerLaws =
  [ ( "sqrt . (**2) == id"
    , Unary
      (\a ->
        not (all (> zero) a)
          || any (> smallRational) a
          || ((sqrt . (** (one + one)) $ a) == a)
          && (((** (one + one)) . sqrt $ a) == a)
      )
    )
  , ( "log . exp == id"
    , Unary
      (\a ->
        not (all (> zero) a)
          || any (> smallRational) a
          || ((log . exp $ a) == a)
          && ((exp . log $ a) == a)
      )
    )
  , ( "for +ive b, a != 0,1: a ** logBase a b ~= b"
    , Binary
      (\a b ->
        (not (all (> zero) b)
        || not (all nearZero a)
        || all (== one) a
        || (all (== zero) a && all nearZero (logBase a b))
        || (a ** logBase a b == b)
        )
      )
    )
  ]

moduleLaws
  :: (Epsilon a, Epsilon (r a), MultiplicativeAction r a, Module r a) => [Law2 (r a) a]
moduleLaws =
  [ ( "multiplicative module unital: a .* one == a"
    , Unary10 (\a -> a .* one == a)
    )
  , ( "module right distributive: (a + b) .* c == (a .* c) + (b .* c)"
    , Ternary21 (\a b c -> (a + b) .* c == (a .* c) + (b .* c))
    )
  , ( "module left distributive: c *. (a + b) == (c *. a) + (c *. b)"
    , Ternary21 (\a b c -> c *. (a + b) == (c *. a) + (c *. b))
    )
  , ("annihilation: a .* zero == zero", Unary10 (\a -> a .* zero == zero))
  , ( "module multiplicative equivalence: a .* b == b *. a"
    , Binary11 (\a b -> a .* b == b *. a)
    )
  ]

banachLaws
  :: ( Foldable r
     , Epsilon (r a)
     , Banach r a
     , Module r a
     , MultiplicativeAction r a
     , Signed a
     , FromRatio a
     , Ord a
     , Applicative r
     )
  => [Law2 (r a) a]
banachLaws =
  [ ( "L1: normalize a .* norm a == one"
    , Unary10
      (\a ->
        (a == pure zero)
          || (any ((> smallRational) . abs) a
             || ((normalizeL1 a .* normL1 a) == a)
             )
      )
    )
  , ( "L2: normalize a .* norm a == one"
    , Unary10
      (\a ->
        (a == pure zero)
          || (any ((> smallRational) . abs) a
             || ((normalizeL2 a .* normL2 a) == a)
             )
      )
    )
{-
    , ( "Lp: normalizeLp a p .* normLp a p == one"
    , Binary11
        (\a p ->
           a == pure zero ||
           (any ((> smallRational) . normL1) a || (normalizeLp p a .* normLp p a) == a)))
-}
  ]

hilbertLaws
  ::
    ( Module r a
    , MultiplicativeAction r a
    , Epsilon a
    , Hilbert r a
    )
  => [Law2 (r a) a]
hilbertLaws =
  [ ("commutative a <.> b == b <.> a", Ternary21 (\a b _ -> a <.> b == b <.> a))
  , ( "distributive over additive a <.> (b + c) == a <.> b + a <.> c"
    , Ternary30 (\a b c -> a <.> (b + c) == a <.> b + a <.> c)
    )
  , ( "bilinear a <.> (s *. b + c) == s * (a <.> b) + a <.> c"
    , Quad31 (\a b c s -> a <.> (s *. b + c) == s * (a <.> b) + a <.> c)
    )
  , ( "scalar multiplicative (s0 *. a) <.> (s1 *. b) == s0 * s1 * (a <.> b)"
    , Quad22 (\a b s0 s1 -> (s0 *. a) <.> (s1 *. b) == s0 * s1 * (a <.> b))
    )
  ]

tensorProductLaws
  :: (Eq (r (r a)), Additive (r (r a)), TensorProduct (r a), Epsilon (r a))
  => [Law2 (r a) a]
tensorProductLaws =
  [ ( "left distributive over additive a><b + c><b == (a+c) >< b"
    , Ternary30 (\a b c -> a >< b + c >< b == (a + c) >< b)
    )
  , ( "right distributive over additive a><b + a><c == a >< (b+c)"
    , Ternary30 (\a b c -> a >< b + a >< c == a >< (b + c))
    )
  -- , ( "left module tensor correspondance a *. (b><c) == (a><b) .* c"
  --   , Ternary30 (\a b c -> a *. (b><c) == (a><b) .* c))
  -- , ( "right module tensor correspondance (a><b) .* c == a *. (b><c)"
  --   , Ternary30 (\a b c -> (a><b) .* c == a *. (b><c)))
  ]

hadamardMultiplicationLaws
  :: (Eq (r a), HadamardMultiplication r a, Applicative r) => [Law (r a)]
hadamardMultiplicationLaws =
  [ ( "associative: (a .*. b) .*. c == a .*. (b .*. c)"
    , Ternary (\a b c -> (a .*. b) .*. c == a .*. (b .*. c))
    )
  , ("left id: pure one .*. a = a", Unary (\a -> pure one .*. a == a))
  , ("right id: a .*. pure one = a", Unary (\a -> a .*. pure one == a))
  , ("commutative: a .*. b == b .*. a", Binary (\a b -> a .*. b == b .*. a))
  ]

hadamardDivisionLaws
  :: (Epsilon a, Epsilon (r a), HadamardDivision r a, Applicative r)
  => [Law (r a)]
hadamardDivisionLaws = 
  [ ( "basis divide: a ./. a == pure one"
    , Unary (\a -> a == pure zero || (a ./. a) == pure one)
    )
  ]

-- | semiring
semiringLaws :: (Epsilon a, Semiring a) => [Law a]
semiringLaws =
  additiveLaws
    <> distributiveLaws
    <> [ ( "associative: (a * b) * c = a * (b * c)"
         , Ternary (\a b c -> (a * b) * c == a * (b * c))
         )
       , ("left id: one * a = a", Unary (\a -> one * a == a))
       , ("right id: a * one = a", Unary (\a -> a * one == a))
       ]

-- | ring
ringLaws :: (Epsilon a, Ring a) => [Law a]
ringLaws = semiringLaws <> additiveGroupLaws

-- | starsemiring
starSemiringLaws :: (Epsilon a, StarSemiring a) => [Law a]
starSemiringLaws =
  semiringLaws
    <> [ ( "star law: star a == one + a * star a"
         , Unary (\a -> star a == one + a * star a)
         )
       ]

-- | involutive ring
involutiveRingLaws
  :: forall a . (Eq a, Unital (Product a), InvolutiveRing a) => [Law a]
involutiveRingLaws =
  [ ( "adjoint plus law: adj (a + b) ==> adj a + adj b"
    , Binary (\a b -> adj (a + b) == adj a + adj b)
    )
  , ( "adjoint times law: adj (a * b) ==> adj b * adj a"
    , Binary (\a b -> adj (a * b) == adj b * adj a)
    )
  , ( "adjoint multiplicative unit law: adj one ==> one"
    , Nonary (adj (one :: a) == one)
    )
  , ( "adjoint own inverse law: adj (adj a) ==> a"
    , Unary (\a -> adj (adj a) == a)
    )
  ]

-- integralsLaws are the common laws that apply to Integral-like numbers
integralsLaws
  :: ( Eq a
     , Integral a
     , Signed a
     , ToInteger a
     , FromInteger a
     , AbelianGroup (Sum a)
     )
  => [Law a]
integralsLaws =
  additiveLaws
  <> additiveGroupLaws
  <> multiplicativeLaws
  <> distributiveLaws
  <> integralLaws
  <> signedLaws

integralsUnboundedLaws
  :: ( Ord a
    , Epsilon a
    , Integral a
    , Signed a
    , ToInteger a
    , FromInteger a
    , AbelianGroup (Sum a)
    , Normed a a
    , Metric a a
    )
  => [Laws a a]
integralsUnboundedLaws =
  (Arity1 <$> integralsLaws) <>
  (Arity2 <$> metricIntegralLaws) <>
  (Arity2 <$> normedLaws)

integralsBoundedLaws
  :: ( Ord a
    , Epsilon a
    , Integral a
    , Signed a
    , ToInteger a
    , FromInteger a
    , AbelianGroup (Sum a)
    , Normed a a
    , Metric a a
    , Bounded a
    )
  => [Laws a a]
integralsBoundedLaws =
  (Arity1 <$> integralsLaws) <>
  (Arity2 <$> metricIntegralBoundedLaws) <>
  (Arity2 <$> normedBoundedLaws)
