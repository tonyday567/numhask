{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module NumHask.Hedgehog.Prop where

import Hedgehog as H
import NumHask.Prelude hiding ((%))

-- | running tests in parallel
assertProps
  :: H.GroupName
  -> H.TestLimit
  -> H.Gen a
  -> (H.Gen a -> [(H.PropertyName, H.Property)])
  -> IO Bool
assertProps t n g ps =
  H.checkParallel $
  H.Group t $ (\(pn,pp) -> (pn, H.withTests n pp)) <$> ps g

-- | run tests sequentially
assertPropsSeq
  :: H.GroupName
  -> H.TestLimit
  -> H.Gen a
  -> (H.Gen a -> [(H.PropertyName, H.Property)])
  -> IO Bool
assertPropsSeq t n g ps =
  H.checkSequential $
  H.Group t $ (\(pn,pp) -> (pn, H.withTests n pp)) <$> ps g

-- * Combinators
-- These combinators seem neat, but hedgehog UI requires check fails to be closer to the source.
-- better to thus ignore the redundant code warnings.
--
-- with usage:
--       ┏━━ numhask-hedgehog/src/NumHask/Hedgehog/Prop.hs ━━━
--    12 ┃ unary :: (Show a) => Gen a -> (a -> Bool) -> Property
--    13 ┃ unary src p = property $ do
--    14 ┃   a <- forAll src
--       ┃   │ EmptyInterval
--    15 ┃   assert (p a)
--       ┃   ^^^^^^^^^^^^
--
-- with redundant code snippets:
--       ┏━━ numhask-hedgehog/src/NumHask/Hedgehog/Prop.hs ━━━
--    60 ┃ isUnital :: (Eq a, Show a) => a -> (a -> a -> a) -> Gen a -> Property
--    61 ┃ isUnital z (#) src = property $ do
--    62 ┃   rv <- forAll src
--       ┃   │ EmptyInterval
--    63 ┃   let p a = (z # a) == a && (a # z) == a
--    64 ┃   assert (p rv)
--       ┃   ^^^^^^^^^^^^^
-- 

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
isIdempotent (#) src = property $ do
  rv <- forAll src
  let p = \a -> (a # a) == a
  assert (p rv)

isCommutative :: (Eq a, Show a) =>
  (a -> a -> a) -> Gen a -> Property
isCommutative (#) src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b -> (a # b) == (b # a)
  assert (p rv rv')

isUnital :: (Eq a, Show a) => a -> (a -> a -> a) -> Gen a -> Property
isUnital z (#) src = property $ do
  rv <- forAll src
  let p = \a -> (z # a) == a && (a # z) == a
  assert (p rv)

isAssociative :: (Eq a, Show a) => (a -> a -> a) -> Gen a -> Property
isAssociative (#) src = property $ do
  rv <- forAll src
  rv' <- forAll src
  rv'' <- forAll src
  let p = \a b c -> (a # b) # c == a # (b # c)
  assert (p rv rv' rv'')

isAdditive :: (Eq a, Show a, Additive a) => Gen a -> [(PropertyName, Property)]
isAdditive src =
  [ ("zero", isUnital zero (+) src)
  , ("associative +", isAssociative (+) src)
  , ("commutative +", isCommutative (+) src)
  ]

isGroup :: (Eq a, Show a) => a -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) ->
  Gen a -> Property
isGroup u (#) (%) i src = property $ do
  rv <- forAll src
  let p = \a ->
        (a % a) == u &&
        (i a == u % a) &&
        (i a # a) == u &&
        (a # i a) == u
  assert (p rv)

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
isDistributive u (#) (%) src = property $ do
  rv <- forAll src
  rv' <- forAll src
  rv'' <- forAll src
  let p = \a b c ->
        a % u == u &&
        u % a == u &&
        a % (b # c) == (a % b) # (a % c) &&
        (a # b) % c == (a % c) # (b % c)
  assert (p rv rv' rv'')

isAbsorbativeUnit :: (Eq a, Show a) => a -> (a -> a -> a) -> Gen a -> Property
isAbsorbativeUnit u (#) src = property $ do
  rv <- forAll src
  let p = \a ->
        (a # u) == u &&
        (u # a) == u
  assert (p rv)

isAbsorbative :: (Eq a, Show a) => (a -> a -> a) -> (a -> a -> a) -> Gen a -> Property
isAbsorbative (#) (%) src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        (a # (a % b)) == (a % (a # b)) &&
        a == (a % (a # b))
  assert (p rv rv')

isIntegral :: (Eq a, Show a, Integral a) => Gen a -> Property
isIntegral src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        b == zero ||
        b * (a `div` b) + (a `mod` b) == a
  assert (p rv rv')

isFromIntegral :: (Eq a, Show a, FromInteger a, ToInteger a) => Gen a -> Property
isFromIntegral src = property $ do
  rv <- forAll src
  let p = \a -> fromIntegral a == a
  assert (p rv)

isRational :: (Eq a, Show a, FromRatio a, ToRatio a) => Gen a -> Property
isRational src = property $ do
  rv <- forAll src
  let p = \a ->
        fromRational a == a
  assert (p rv)

isSigned :: (Eq a, Show a, Signed a) => Gen a -> Property
isSigned src = property $ do
  rv <- forAll src
  let p = \a ->
        sign a * abs a == a
  assert (p rv)

isNormed :: forall a b. (JoinSemiLattice b, Show a, Normed a b)
  => [b] -> Gen a -> Property
isNormed _ src = property $ do
  rv <- forAll src
  let p = \a ->
        (normL1 a `joinLeq` (zero :: b)) &&
        normL1 (zero :: a) == (zero :: b)
  assert (p rv)

isNormedBounded :: forall a. (JoinSemiLattice a, Bounded a, Show a, Normed a a)
  => Gen a -> Property
isNormedBounded src = property $ do
  rv <- forAll src
  let p = \a ->
        a == minBound ||
        normL1 a `joinLeq` (zero :: a) &&
        normL1 (zero :: a) == (zero :: a)
  assert (p rv)

isNormedUnbounded :: forall a. (JoinSemiLattice a, Show a, Normed a a) => Gen a -> Property
isNormedUnbounded src = property $ do
  rv <- forAll src
  let p = \a ->
        (normL1 a `joinLeq` (zero :: a)) &&
        normL1 (zero :: a) == (zero :: a)
  assert (p rv)

isMetricBounded :: forall a. (JoinSemiLattice a, Bounded a, Additive a, Show a, Metric a a) => Gen a -> Property
isMetricBounded src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        distanceL1 a b `joinLeq` (zero :: a) &&
        distanceL1 a a == (zero :: a) ||
        distanceL1 a b == (minBound :: a)
  assert (p rv rv')

isMetricUnbounded :: forall a. (JoinSemiLattice a, Additive a, Show a, Metric a a) => Gen a -> Property
isMetricUnbounded src = property $ do
  rv <- forAll src
  rv' <- forAll src
  rv'' <- forAll src
  let p = \a b c ->
        distanceL1 a b `joinLeq` (zero :: a) &&
        distanceL1 a a == (zero :: a) &&
        ((distanceL1 a c + distanceL1 b c) `joinLeq` (distanceL1 a b :: a)) &&
        ((distanceL1 a b + distanceL1 b c) `joinLeq` (distanceL1 a c :: a)) &&
        ((distanceL1 a b + distanceL1 a c) `joinLeq` (distanceL1 b c :: a))
  assert (p rv rv' rv'')

isUpperBoundedField :: forall a. (Eq a, UpperBoundedField a, Show a) => Gen a -> Property
isUpperBoundedField src = property $ do
  rv <- forAll src
  let p = \a ->
        ((one :: a) / zero + infinity == infinity) &&
        (infinity + a == infinity) &&
        ((zero :: a) / zero /= nan)
  assert (p rv)

isLowerBoundedField :: forall a. (Eq a, LowerBoundedField a, Show a) => Gen a -> Property
isLowerBoundedField src = property $ do
  rv <- forAll src
  let p = \a ->
        (negate (one :: a) / zero == negInfinity) &&
        ((negInfinity :: a) + negInfinity == negInfinity) &&
        (negInfinity + a == negInfinity)
  assert (p rv)

-- > a - one < floor a <= a <= ceiling a < a + one
-- > round a == floor (a + one/(one+one))
--
isQuotientIntegerField :: forall a. (JoinSemiLattice a, FromInteger a, QuotientField a Integer, Show a) => Gen a -> Property
isQuotientIntegerField src = property $ do
  rv <- forAll src
  let p = \a ->
        ((a - one) ~< fromInteger (floor a)) &&
        (fromInteger (floor a) ~<= a) &&
        (a ~<= fromInteger (ceiling a)) &&
        (fromInteger (ceiling a) ~< a + one) &&
        (case even ((floor $ a + one / (one + one)) :: Integer) of
           True -> (round a :: Integer) == floor (a + (one / (one + one)))
           False -> (round a :: Integer) == ceiling (a - (one / (one + one))))
  assert (p rv)
  where
    (~<) a b = joinLeq b a && not (a == b)
    (~<=) = flip joinLeq

-- > sqrt . (**(one+one)) == id
-- > log . exp == id
-- > for +ive b, a != 0,1: a ** logBase a b == b
isExpField :: forall a. (Ord a, Epsilon a, ExpField a, Show a, Normed a a) => Gen a -> Property
isExpField src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
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
  assert (p rv rv')

isSemiring :: (Eq a, Show a, Distributive a) => Gen a -> [(PropertyName, Property)]
isSemiring src =
  [ ("zero", isUnital zero (+) src)
  , ("associative +", isAssociative (+) src)
  , ("commutative +", isCommutative (+) src)
  , ("distributive", isDistributive zero (+) (*) src)
  , ("one", isUnital one (*) src)
  , ("associative *", isAssociative (*) src)  ]

isRing :: (Eq a, Show a, Distributive a, Subtractive a) => Gen a -> [(PropertyName, Property)]
isRing src =
  isSemiring src <> isSubtractive src

isStarSemiring :: (Eq a, Show a, StarSemiring a) => Gen a -> Property
isStarSemiring src = property $ do
  rv <- forAll src
  let p = \a ->
        star a == one + a * star a
  assert (p rv)

isInvolutive :: forall a. (Eq a, Show a, InvolutiveRing a) => Gen a -> Property
isInvolutive src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        adj (a + b) == adj a + adj b &&
        adj (a * b) == adj b * adj a &&
        adj (one :: a) == (one :: a) &&
        adj (adj a) == a
  assert (p rv rv')

