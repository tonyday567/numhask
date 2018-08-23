{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RebindableSyntax #-}

module NumHask.Hedgehog.Prop where

import Hedgehog as H
import NumHask.Prelude

-- | Combinator for a property of involving a single element
unary :: (Show a) => Gen a -> (a -> Bool) -> Property
unary src p = property $ do
  a <- forAll src
  assert (p a)

-- | Combinator for a property involving two elements
binary :: (Show a) => Gen a -> (a -> a -> Bool) -> Property
binary src p = property $ do
  a <- forAll src
  b <- forAll src
  assert (p a b)

-- | Combinator for a property involving three elements
ternary :: (Show a) => Gen a -> (a -> a -> a -> Bool) -> Property
ternary src p = property $ do
  a <- forAll src
  b <- forAll src
  c <- forAll src
  assert (p a b c)

isIdempotent :: (Eq a, Show a) =>
  (a -> a -> a) -> Gen a -> Property
isIdempotent (#) src = unary src $ \a ->
  (a # a) == a

isCommutative :: (Eq a, Show a) =>
  (a -> a -> a) -> Gen a -> Property
isCommutative (#) src = binary src $ \a b ->
  (a # b) == (b # a)

isUnital :: (Eq a, Show a) => a -> (a -> a -> a) -> Gen a -> Property
isUnital z (#) src = unary src $ \a ->
  (z # a) == a && (a # z) == a

isAssociative :: (Eq a, Show a) => (a -> a -> a) -> Gen a -> Property
isAssociative (#) src = ternary src $ \a b c ->
  (a # b) # c == a # (b # c)

isAdditive :: (Eq a, Show a, Additive a) => Gen a -> [(PropertyName, Property)]
isAdditive src =
  [ ("zero", isUnital zero (+) src)
  , ("associative +", isAssociative (+) src)
  , ("commutative +", isCommutative (+) src)
  ]

isGroup :: (Eq a, Show a) => a -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) ->
  Gen a -> Property
isGroup u (#) (~#) i src = unary src $ \a ->
  (a ~# a) == u &&
  (i a == u ~# a) &&
  (i a # a) == u &&
  (a # i a) == u

isSubtractive :: (Eq a, Show a, Subtractive a) => Gen a -> [(PropertyName, Property)]
isSubtractive src =
  [ ("subtractive -", isGroup zero (+) (-) negate src)
  ]

isMultiplicative :: (Eq a, Show a, Multiplicative a) => Gen a -> [(PropertyName, Property)]
isMultiplicative src =
  [ ("one", isUnital one (*) src)
  , ("associative *", isAssociative (*) src)
  , ("commutative *", isCommutative (*) src)
  ]

isDivisive :: (Eq a, Show a, Divisive a) => Gen a -> [(PropertyName, Property)]
isDivisive src =
  [ ("divisive /", isGroup one (*) (/) recip src)
  ]

isDistributive :: (Eq a, Show a) => a -> (a -> a -> a) -> (a -> a -> a) ->
  Gen a -> Property
isDistributive u (#) (#*) src = ternary src $ \a b c ->
  a #* u == u &&
  u #* a == u &&
  a #* (b # c) == (a #* b) # (a #* c) &&
  (a # b) #* c == (a #* c) # (b #* c)

isIntegral :: (Eq a, Show a, Integral a, FromInteger a, ToInteger a) => Gen a -> Property
isIntegral src = binary src $ \a b ->
  b == zero || b * (a `div` b) + (a `mod` b) == a &&
  fromIntegral a == a

isRational :: (Eq a, Show a, FromRatio a, ToRatio a) => Gen a -> Property
isRational src = unary src $ \a ->
  fromRational a == a

isSigned :: (Eq a, Show a, Multiplicative a, Signed a) => Gen a -> Property
isSigned src = unary src $ \a ->
  sign a * abs a == a

isNormed :: forall a b. (Ord b, Additive a, Additive b, Show a, Normed a b) => [b] -> Gen a -> Property
isNormed _ src = unary src $ \a ->
  normL1 a >= (zero :: b) &&
  normL1 (zero :: a) == (zero :: b)

isNormedBounded :: forall a. (Ord a, Bounded a, Additive a, Show a, Normed a a) => Gen a -> Property
isNormedBounded src = unary src $ \a ->
  a == minBound ||
  normL1 a >= (zero :: a) &&
  normL1 (zero :: a) == (zero :: a)

isNormedUnbounded :: forall a. (Ord a, Additive a, Show a, Normed a a) => Gen a -> Property
isNormedUnbounded src = unary src $ \a ->
  normL1 a >= (zero :: a) &&
  normL1 (zero :: a) == (zero :: a)

isMetricBounded :: forall a. (Ord a, Bounded a, Additive a, Show a, Metric a a) => Gen a -> Property
isMetricBounded src = binary src $ \a b -> 
  distanceL1 a b >= (zero :: a) &&
  distanceL1 a a == (zero :: a) ||
  distanceL1 a b == (minBound :: a)

isMetricUnbounded :: forall a. (Ord a, Additive a, Show a, Metric a a) => Gen a -> Property
isMetricUnbounded src = ternary src $ \a b c ->
  distanceL1 a b >= (zero :: a) &&
  distanceL1 a a == (zero :: a) &&
  (distanceL1 a c + distanceL1 b c >= (distanceL1 a b :: a)) &&
  (distanceL1 a b + distanceL1 b c >= (distanceL1 a c :: a)) &&
  (distanceL1 a b + distanceL1 a c >= (distanceL1 b c :: a))

isUpperBoundedField :: forall a. (Ord a, UpperBoundedField a, Show a) => Gen a -> Property
isUpperBoundedField src = unary src $ \a ->
  ((one :: a) / zero + infinity == infinity) &&
  (infinity + a == infinity) &&
  ((zero :: a) / zero /= nan)

isLowerBoundedField :: forall a. (Ord a, LowerBoundedField a, Show a) => Gen a -> Property
isLowerBoundedField src = unary src $ \a ->
  (negate (one :: a) / zero == negInfinity) &&
  ((negInfinity :: a) + negInfinity == negInfinity) &&
  (negInfinity + a == negInfinity)

-- > a - one < floor a <= a <= ceiling a < a + one
-- > round a == floor (a + one/(one+one))
--
isQuotientField :: forall a. (Ord a, FromInteger a, QuotientField a Integer, Show a) => Gen a -> Property
isQuotientField src = unary src $ \a ->
  ((a - one) < fromInteger (floor a))
  && (fromInteger (floor a) <= a)
  && (a <= fromInteger (ceiling a))
  && (fromInteger (ceiling a) < a + one) &&
  (case even ((floor $ a + one / (one + one)) :: Integer) of
      True -> (round a :: Integer) == floor (a + (one / (one + one)))
      False -> (round a :: Integer) == ceiling (a - (one / (one + one))))

-- > sqrt . (**(one+one)) == id
-- > log . exp == id
-- > for +ive b, a != 0,1: a ** logBase a b == b
isExpField :: forall a. (Ord a, Epsilon a, Subtractive a, ExpField a, Show a, Normed a a) => Gen a -> Property
isExpField src = binary src $ \a b ->
  (not (a > (zero :: a))
    || ((sqrt . (** (one + one)) $ a) == a)
    && (((** (one + one)) . sqrt $ a) == a)) &&
  (not (a > (zero :: a))
    || ((log . exp $ a) == a)
    && ((exp . log $ a) == a)) &&
  (not (normL1 b > (zero :: a))
    || not (nearZero (a - zero))
    || (a == one)
    || (a == zero && nearZero (logBase a b))
    || (a ** logBase a b == b))

isSemiring :: (Eq a, Show a, Semiring a) => Gen a -> [(PropertyName, Property)]
isSemiring src =
  [ ("zero", isUnital zero (+) src)
  , ("associative +", isAssociative (+) src)
  , ("commutative +", isCommutative (+) src)
  , ("distributive", isDistributive zero (+) (*) src)
  , ("one", isUnital one (*) src)
  , ("associative *", isAssociative (*) src)  ]

isRing :: (Eq a, Show a, Ring a) => Gen a -> [(PropertyName, Property)]
isRing src =
  isSemiring src <> isSubtractive src

isStarSemiring :: (Eq a, Show a, StarSemiring a) => Gen a -> Property
isStarSemiring src = unary src $ \a ->
  star a == one + a * star a

isInvolutive :: forall a. (Eq a, Show a, InvolutiveRing a) => Gen a -> Property
isInvolutive src = binary src $ \a b ->
  adj (a + b) == adj a + adj b &&
  adj (a * b) == adj b * adj a &&
  adj (one :: a) == (one :: a) &&
  adj (adj a) == a

assertProps
  :: H.GroupName
  -> H.TestLimit
  -> H.Gen a
  -> (H.Gen a -> [(H.PropertyName, H.Property)])
  -> IO Bool
assertProps t n g ps =
  H.checkParallel $
  H.Group t $ (\(pn,pp) -> (pn, H.withTests n pp)) <$> ps g
