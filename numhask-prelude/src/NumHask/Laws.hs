{-# LANGUAGE FlexibleContexts #-}

module NumHask.Laws
  ( LawArity(..)
  , LawArity2(..)
  , Law
  , Law2
  , testLawOf
  , testLawOf2
  , idempotentLaws
  , additiveLaws
  , additiveLawsFail
  , additiveGroupLaws
  , multiplicativeLaws
  , multiplicativeLawsFail
  , multiplicativeMonoidalLaws
  , multiplicativeGroupLaws
  , distributionLaws
  , distributionLawsFail
  , integralLaws
  , signedLaws
  , metricFloatLaws 
  , metricComplexFloatLaws
  , boundedFieldFloatLaws
  , quotientFieldLaws 
  , expFieldLaws
  , expFieldComplexLooseLaws  
  , additiveBasisLaws
  , additiveGroupBasisLaws
  , multiplicativeBasisLaws
  , multiplicativeGroupBasisLaws
  , additiveModuleLaws
  , additiveGroupModuleLaws
  , multiplicativeModuleLaws
  , multiplicativeGroupModuleLawsFail
  , expFieldNaperianLaws
  , metricNaperianFloatLaws
  , tensorProductLaws
  , banachLaws
  , hilbertLaws
  , semiringLaws
  , ringLaws
  , starSemiringLaws
  ) where

import NumHask.Prelude
import Test.Tasty.QuickCheck hiding ((><))
import Test.Tasty (TestName, TestTree)

data LawArity a
  = Nonary Bool
  | Unary (a -> Bool)
  | Binary (a -> a -> Bool)
  | Ternary (a -> a -> a -> Bool)
  | Ornary (a -> a -> a -> a -> Bool)
  | Failiary (a -> Property)

data LawArity2 a b
  = Unary2 (a -> Bool)
  | Binary2 (a -> b -> Bool)
  | Ternary2 (a -> a -> b -> Bool)
  | Ternary2' (a -> b -> b -> Bool)
  | Ternary2'' (a -> a -> a -> Bool)
  | Quad31 (a -> a -> a -> b -> Bool)
  | Quad22 (a -> a -> b -> b -> Bool)
  | Failiary2 (a -> Property)

type Law a = (TestName, LawArity a)

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
testLawOf2 _ (name, Unary2 f) = testProperty name f
testLawOf2 _ (name, Binary2 f) = testProperty name f
testLawOf2 _ (name, Ternary2 f) = testProperty name f
testLawOf2 _ (name, Ternary2' f) = testProperty name f
testLawOf2 _ (name, Ternary2'' f) = testProperty name f
testLawOf2 _ (name, Quad22 f) = testProperty name f
testLawOf2 _ (name, Quad31 f) = testProperty name f
testLawOf2 _ (name, Failiary2 f) = testProperty name f

-- idempotent
idempotentLaws :: (Eq a, Additive a, Multiplicative a) => [Law a]
idempotentLaws =
  [ ("idempotent: a + a == a", Unary (\a -> a + a == a))
  , ("idempotent: a * a == a", Unary (\a -> a * a == a))
  ]

-- additive
additiveLaws :: (Eq a, Additive a) => [Law a]
additiveLaws =
  [ ( "associative: (a + b) + c = a + (b + c)"
    , Ternary (\a b c -> (a + b) + c == a + (b + c)))
  , ("left id: zero + a = a", Unary (\a -> zero + a == a))
  , ("right id: a + zero = a", Unary (\a -> a + zero == a))
  , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
  ]

additiveLawsFail :: (Eq a, Additive a, Show a, Arbitrary a) => [Law a]
additiveLawsFail =
  [ ( "associative: (a + b) + c = a + (b + c)"
    , Failiary $ expectFailure . (\a b c -> (a + b) + c == a + (b + c)))
  , ("left id: zero + a = a", Unary (\a -> zero + a == a))
  , ("right id: a + zero = a", Unary (\a -> a + zero == a))
  , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
  ]

additiveGroupLaws :: (Eq a, AdditiveGroup a) => [Law a]
additiveGroupLaws =
  [ ("minus: a - a = zero", Unary (\a -> (a - a) == zero))
  , ("negate minus: negate a == zero - a", Unary (\a -> negate a == zero - a))
  , ( "negate left cancel: negate a + a == zero"
    , Unary (\a -> negate a + a == zero))
  , ( "negate right cancel: negate a + a == zero"
    , Unary (\a -> a + negate a == zero))
  ]

-- multiplicative
multiplicativeLaws :: (Eq a, Multiplicative a) => [Law a]
multiplicativeLaws =
  [ ( "associative: (a * b) * c = a * (b * c)"
    , Ternary (\a b c -> (a * b) * c == a * (b * c)))
  , ("left id: one * a = a", Unary (\a -> one * a == a))
  , ("right id: a * one = a", Unary (\a -> a * one == a))
  , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
  ]

multiplicativeMonoidalLaws ::
     (Eq a, MultiplicativeUnital a) => [Law a]
multiplicativeMonoidalLaws =
  [ ( "associative: (a * b) * c = a * (b * c)"
    , Ternary (\a b c -> (a `times` b) `times` c == a `times` (b `times` c)))
  , ("left id: one `times` a = a", Unary (\a -> one `times` a == a))
  , ("right id: a `times` one = a", Unary (\a -> a `times` one == a))
  ]

multiplicativeLawsFail ::
     (Eq a, Show a, Arbitrary a, Multiplicative a) => [Law a]
multiplicativeLawsFail =
  [ ( "associative: (a * b) * c = a * (b * c)"
    , Failiary $ expectFailure . (\a b c -> (a * b) * c == a * (b * c)))
  , ("left id: one * a = a", Unary (\a -> one * a == a))
  , ("right id: a * one = a", Unary (\a -> a * one == a))
  , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
  ]

multiplicativeGroupLaws :: (Epsilon a, Eq a, MultiplicativeGroup a) => [Law a]
multiplicativeGroupLaws =
  [ ( "divide: a == zero || a / a ≈ one"
    , Unary (\a -> a == zero || (a / a) ≈ one))
  , ( "recip divide: recip a == one / a"
    , Unary (\a -> a == zero || recip a == one / a))
  , ( "recip left: a == zero || recip a * a ≈ one"
    , Unary (\a -> a == zero || recip a * a ≈ one))
  , ( "recip right: a == zero || a * recip a ≈ one"
    , Unary (\a -> a == zero || a * recip a ≈ one))
  ]

-- distribution
distributionLaws :: (Eq a, Distribution a) => [Law a]
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
     (Show a, Arbitrary a, Epsilon a, Eq a, Distribution a) => [Law a]
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

-- metric
signedLaws :: (Eq a, Signed a) => [Law a]
signedLaws = [("sign a * abs a == a", Unary (\a -> sign a `times` abs a == a))]

metricFloatLaws :: () => [Law Float]
metricFloatLaws =
  [ ("positive", Binary (\a b -> (distance a b :: Float) >= zero))
  , ("zero if equal", Unary (\a -> (distance a a :: Float) == zero))
  , ( "associative"
    , Binary (\a b -> (distance a b :: Float) ≈ (distance b a :: Float)))
  , ( "triangle rule - sum of distances > distance"
    , Ternary
        (\a b c ->
           (abs a > 10.0) ||
           (abs b > 10.0) ||
           (abs c > 10.0) ||
           not
             (veryNegative
                (distance a c + distance b c - (distance a b :: Float))) &&
           not
             (veryNegative
                (distance a b + distance b c - (distance a c :: Float))) &&
           not
             (veryNegative
                (distance a b + distance a c - (distance b c :: Float)))))
  ]

metricComplexFloatLaws :: () => [Law (Complex Float)]
metricComplexFloatLaws =
  [ ("positive", Binary (\a b -> (distance a b :: Float) >= zero))
  , ("zero if equal", Unary (\a -> (distance a a :: Float) == zero))
  , ( "associative"
    , Binary (\a b -> (distance a b :: Float) ≈ (distance b a :: Float)))
  , ( "triangle rule - sum of distances > distance"
    , Ternary
        (\a b c ->
           (size a > (10.0 :: Float)) ||
           (size b > (10.0 :: Float)) ||
           (size c > (10.0 :: Float)) ||
           not
             (veryNegative
                (distance a c + distance b c - (distance a b :: Float))) &&
           not
             (veryNegative
                (distance a b + distance b c - (distance a c :: Float))) &&
           not
             (veryNegative
                (distance a b + distance a c - (distance b c :: Float)))))
  ]

-- field
boundedFieldFloatLaws :: [Law Float]
boundedFieldFloatLaws =
  [ ( "infinity laws"
    , Unary
        (\a ->
           ((one :: Float) / zero + infinity == infinity) &&
           (infinity + a == infinity) &&
           isNaN ((infinity :: Float) - infinity) &&
           isNaN ((infinity :: Float) / infinity) &&
           isNaN (nan + a) && (zero :: Float) / zero /= nan))
  ]

quotientFieldLaws :: (Ord a, Field a, QuotientField a, FromInteger a) => [Law a]
quotientFieldLaws =
  [ ( "a - one < floor a <= a <= ceiling a < a + one"
    , Unary
        (\a ->
           ((a - one) < fromIntegral (floor a)) &&
           (fromIntegral (floor a) <= a) &&
           (a <= fromIntegral (ceiling a)) &&
           (fromIntegral (ceiling a) < a + one)))
  , ( "round a == floor (a + one/(one+one))"
    , Unary (\a -> round a == floor (a + one / (one + one))))
  ]

expFieldLaws ::
     (ExpField a, Signed a, Epsilon a, Fractional a, Ord a) => [Law a]
expFieldLaws =
  [ ( "sqrt . (**(one+one)) ≈ id"
    , Unary
        (\a ->
           not (veryPositive a) ||
           (a > 10.0) ||
           (sqrt . (** (one + one)) $ a) ≈ a &&
           ((** (one + one)) . sqrt $ a) ≈ a))
  , ( "log . exp ≈ id"
    , Unary
        (\a ->
           not (veryPositive a) ||
           (a > 10.0) || (log . exp $ a) ≈ a && (exp . log $ a) ≈ a))
  , ( "for +ive b, a != 0,1: a ** logBase a b ≈ b"
    , Binary
        (\a b ->
           (not (veryPositive b) ||
            not (nearZero (a - zero)) ||
            (a == one) ||
            (a == zero && nearZero (logBase a b)) || (a ** logBase a b ≈ b))))
  ]

expFieldComplexLooseLaws :: Float -> [Law (Complex Float)]
expFieldComplexLooseLaws _ =
  [ ( "sqrt . (**(one+one)) ≈ id test contains a stack overflow"
    , Unary (const True))
  , ("log . exp test contains a stack overflow", Unary (const True))
  , ( "for +ive b, a != 0,1: a ** logBase a b ≈ b"
    , Binary
        (\a b@(rb :+ ib) ->
           (not (rb > zero && ib > zero) ||
            not (nearZero (a - zero)) ||
            (a == one) ||
            (a == zero && nearZero (logBase a b)) || (a ** logBase a b ≈ b))))
  ]

metricNaperianFloatLaws :: (Metric (r Float) Float) => [Law (r Float)]
metricNaperianFloatLaws =
  [ ("positive", Binary (\a b -> distance a b >= (zero :: Float)))
  , ("zero if equal", Unary (\a -> distance a a == (zero :: Float)))
  , ("associative", Binary (\a b -> distance a b ≈ (distance b a :: Float)))
  , ( "triangle rule - sum of distances > distance"
    , Ternary
        (\a b c ->
           not
             (veryNegative
                (distance a c + distance b c - (distance a b :: Float))) &&
           not
             (veryNegative
                (distance a b + distance b c - (distance a c :: Float))) &&
           not
             (veryNegative
                (distance a b + distance a c - (distance b c :: Float)))))
  ]

expFieldNaperianLaws ::
     ( ExpField (r a)
     , Foldable r
     , ExpField a
     , Epsilon a
     , Signed a
     , Epsilon (r a)
     , Fractional a
     , Ord a
     )
  => [Law (r a)]
expFieldNaperianLaws =
  [ ( "sqrt . (**2) ≈ id"
    , Unary
        (\a ->
           not (all veryPositive a) ||
           any (> 10.0) a ||
           (sqrt . (** (one + one)) $ a) ≈ a &&
           ((** (one + one)) . sqrt $ a) ≈ a))
  , ( "log . exp ≈ id"
    , Unary
        (\a ->
           not (all veryPositive a) ||
           any (> 10.0) a || (log . exp $ a) ≈ a && (exp . log $ a) ≈ a))
  , ( "for +ive b, a != 0,1: a ** logBase a b ≈ b"
    , Binary
        (\a b ->
           (not (all veryPositive b) ||
            not (all nearZero a) ||
            all (== one) a ||
            (all (== zero) a && all nearZero (logBase a b)) ||
            (a ** logBase a b ≈ b))))
  ]

-- module
additiveModuleLaws ::
     (Eq (r a), Epsilon a, Epsilon (r a), AdditiveModule r a) => [Law2 (r a) a]
additiveModuleLaws =
  [ ( "additive module associative: (a + b) .+ c ≈ a + (b .+ c)"
    , Ternary2 (\a b c -> (a + b) .+ c ≈ a + (b .+ c)))
  , ( "additive module commutative: (a + b) .+ c ≈ (a .+ c) + b"
    , Ternary2 (\a b c -> (a + b) .+ c ≈ (a .+ c) + b))
  , ("additive module unital: a .+ zero == a", Unary2 (\a -> a .+ zero == a))
  , ( "module additive equivalence: a .+ b ≈ b +. a"
    , Binary2 (\a b -> a .+ b ≈ b +. a))
  ]

additiveGroupModuleLaws ::
     (Eq (r a), Epsilon a, Epsilon (r a), AdditiveGroupModule r a)
  => [Law2 (r a) a]
additiveGroupModuleLaws =
  [ ( "additive group module associative: (a + b) .- c ≈ a + (b .- c)"
    , Ternary2 (\a b c -> (a + b) .- c ≈ a + (b .- c)))
  , ( "additive group module commutative: (a + b) .- c ≈ (a .- c) + b"
    , Ternary2 (\a b c -> (a + b) .- c ≈ (a .- c) + b))
  , ( "additive group module unital: a .- zero == a"
    , Unary2 (\a -> a .- zero == a))
  , ( "module additive group equivalence: a .- b ≈ negate b +. a"
    , Binary2 (\a b -> a .- b ≈ negate b +. a))
  ]

multiplicativeModuleLaws ::
     (Eq (r a), Epsilon a, Epsilon (r a), MultiplicativeModule r a)
  => [Law2 (r a) a]
multiplicativeModuleLaws =
  [ ( "multiplicative module unital: a .* one == a"
    , Unary2 (\a -> a .* one == a))
  , ( "module right distribution: (a + b) .* c ≈ (a .* c) + (b .* c)"
    , Ternary2 (\a b c -> (a + b) .* c ≈ (a .* c) + (b .* c)))
  , ( "module left distribution: c *. (a + b) ≈ (c *. a) + (c *. b)"
    , Ternary2 (\a b c -> c *. (a + b) ≈ (c *. a) + (c *. b)))
  , ("annihilation: a .* zero == zero", Unary2 (\a -> a .* zero == zero))
  , ( "module multiplicative equivalence: a .* b ≈ b *. a"
    , Binary2 (\a b -> a .* b ≈ b *. a))
  ]

multiplicativeGroupModuleLawsFail ::
     ( Eq a
     , Eq (r a)
     , Epsilon a
     , Epsilon (r a)
     , MultiplicativeGroupModule r a
     )
  => [Law2 (r a) a]
multiplicativeGroupModuleLawsFail =
  [ ( "multiplicative group module unital: a ./ one == a"
    , Unary2 (\a -> nearZero a || a ./ one == a))
  , ( "module multiplicative group equivalence: a ./ b ≈ recip b *. a"
    , Binary2 (\a b -> b == zero || a ./ b ≈ recip b *. a))
  ]

banachLaws ::
     ( Ord a
     , Fractional a
     , Signed a
     , Foldable r
     , Eq (r a)
     , Epsilon (r a)
     , Banach r a
     , Singleton r
     )
  => [Law2 (r a) b]
banachLaws =
  [ ( "normalize a .* size a ≈ one"
    , Unary2
        (\a ->
           a == singleton zero ||
           (any ((> 10.0) . abs) a || (normalize a .* size a) ≈ a)))
  ]

hilbertLaws ::
    ( Eq a
    , MultiplicativeModule r a
    , Epsilon a
    , Epsilon (r a)
    , Hilbert r a)
  => [Law2 (r a) a]
hilbertLaws =
  [ ("commutative a <.> b ≈ b <.> a", Ternary2 (\a b _ -> a <.> b ≈ b <.> a))
  , ( "distributive over addition a <.> (b + c) == a <.> b + a <.> c"
    , Ternary2'' (\a b c -> a <.> (b + c) ≈ a <.> b + a <.> c))
  , ( "bilinear a <.> (s *. b + c) == s * (a <.> b) + a <.> c"
    , Quad31 (\a b c s -> a <.> (s *. b + c) == s * (a <.> b) + a <.> c))
  , ( "scalar multiplication (s0 *. a) <.> (s1 *. b) == s0 * s1 * (a <.> b)"
    , Quad22 (\a b s0 s1 -> (s0 *. a) <.> (s1 *. b) == s0 * s1 * (a <.> b)))
  ]

tensorProductLaws ::
     ( Eq (r (r a))
     , Additive (r (r a))
     , TensorProduct (r a)
     , Epsilon (r a)
     )
  => [Law2 (r a) a]
tensorProductLaws =
  [ ( "left distribution over addition a><b + c><b == (a+c) >< b"
    , Ternary2'' (\a b c -> a >< b + c >< b == (a + c) >< b))
  , ( "right distribution over addition a><b + a><c == a >< (b+c)"
    , Ternary2'' (\a b c -> a >< b + a >< c == a >< (b + c)))
  -- , ( "left module tensor correspondance a *. (b><c) == (a><b) .* c"
  --   , Ternary2'' (\a b c -> a *. (b><c) == (a><b) .* c))
  -- , ( "right module tensor correspondance (a><b) .* c == a *. (b><c)"
  --   , Ternary2'' (\a b c -> (a><b) .* c == a *. (b><c)))
  ]

-- basis
additiveBasisLaws :: (Eq (r a), Epsilon (r a), AdditiveBasis r a) => [Law (r a)]
additiveBasisLaws =
  [ ( "associative: (a .+. b) .+. c ≈ a .+. (b .+. c)"
    , Ternary (\a b c -> (a .+. b) .+. c ≈ a .+. (b .+. c)))
  , ("left id: zero .+. a = a", Unary (\a -> zero .+. a == a))
  , ("right id: a .+. zero = a", Unary (\a -> a .+. zero == a))
  , ("commutative: a .+. b == b .+. a", Binary (\a b -> a .+. b == b .+. a))
  ]

additiveGroupBasisLaws :: (Eq (r a), Singleton r, AdditiveGroupBasis r a) => [Law (r a)]
additiveGroupBasisLaws =
  [ ( "minus: a .-. a = singleton zero"
    , Unary (\a -> (a .-. a) == singleton zero))
  ]

multiplicativeBasisLaws :: (Eq (r a), Singleton r, MultiplicativeBasis r a) => [Law (r a)]
multiplicativeBasisLaws =
  [ ( "associative: (a .*. b) .*. c == a .*. (b .*. c)"
    , Ternary (\a b c -> (a .*. b) .*. c == a .*. (b .*. c)))
  , ("left id: singleton one .*. a = a", Unary (\a -> singleton one .*. a == a))
  , ( "right id: a .*. singleton one = a"
    , Unary (\a -> a .*. singleton one == a))
  , ("commutative: a .*. b == b .*. a", Binary (\a b -> a .*. b == b .*. a))
  ]

multiplicativeGroupBasisLaws ::
     ( Eq (r a)
     , Epsilon a
     , Epsilon (r a)
     , Singleton r
     , MultiplicativeGroupBasis r a
     )
  => [Law (r a)]
multiplicativeGroupBasisLaws =
  [ ( "basis divide: a ./. a ≈ singleton one"
    , Unary (\a -> a == singleton zero || (a ./. a) ≈ singleton one))
  ]

-- | semiring
semiringLaws :: (Eq a, Semiring a) => [Law a]
semiringLaws = additiveLaws <> distributionLaws <>
    [ ( "associative: (a * b) * c = a * (b * c)"
    , Ternary (\a b c -> (a `times` b) `times` c == a `times` (b `times` c)))
    , ("left id: one * a = a", Unary (\a -> one `times` a == a))
    , ("right id: a * one = a", Unary (\a -> a `times` one == a))
    ]

-- | ring
ringLaws :: (Eq a, Ring a) => [Law a]
ringLaws = semiringLaws <> additiveGroupLaws

-- | starsemiring
starSemiringLaws :: (Eq a, StarSemiring a) => [Law a]
starSemiringLaws = semiringLaws <>
    [ ( "star law: star a == one + a `times` star a"
    , Unary (\a -> star a == one + a `times` star a))
    ]

