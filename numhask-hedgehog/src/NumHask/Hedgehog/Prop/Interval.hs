{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module NumHask.Hedgehog.Prop.Interval where

import NumHask.Prelude hiding ((%), (.*.))
import Hedgehog as H
import NumHask.Hedgehog.Prop (unary, binary, ternary)

-- * individual tests
isIdempotent :: forall a. (Show a, Epsilon a, Lattice a, Subtractive a, Multiplicative a) => (Interval a -> Interval a -> Interval a) -> a -> Gen a -> Property
isIdempotent (##) acc src = unary src $ \a ->
  a |.| (eps acc a ## eps acc a :: Interval a)

isCommutative :: forall a. (Show a, Epsilon a, Lattice a, Subtractive a, Multiplicative a) => (a -> a -> a) -> (Interval a -> Interval a -> Interval a) -> a -> Gen a -> Property
isCommutative (#) (##) acc src = binary src $ \a b ->
  (a # b) |.| eps acc b ## eps acc a

isUnital :: forall a. (Show a, Epsilon a, Lattice a, Subtractive a, Multiplicative a) => a -> (a -> a -> a) -> a -> Gen a -> Property
isUnital u (#) acc src = unary src $ \a ->
  (u # a) |.| (eps acc a :: Interval a) &&
  (a # u) |.| (eps acc a :: Interval a)

isAssociative :: forall a. (Show a, Epsilon a, Lattice a, Subtractive a, Multiplicative a) => (a -> a -> a) -> (Interval a -> Interval a -> Interval a) -> a -> Gen a -> Property
isAssociative (#) (##) acc src = ternary src $ \a b c ->
  ((a # b) # c) |.| (eps acc a ## (eps acc b ## eps acc c))

isAdditive :: forall a.(Show a, Epsilon a, Space (Interval a), Multiplicative a) => a -> Gen a -> [(PropertyName, Property)]
isAdditive acc src =
  [ ("zero", isUnital zero (+) acc src)
  , ("associative +", isAssociative (+) (+) acc src)
  , ("commutative +", isCommutative (+) (+) acc src)
  ]

isSubtractive :: forall a.(Show a, Epsilon a, Space (Interval a), Divisive a) => a -> Gen a -> Property
isSubtractive acc src = unary src $ \a -> 
  (a - a) |.| (eps acc zero :: Interval a) &&
  (negate a |.| (eps acc zero - (eps acc a :: Interval a))) &&
  (negate a + a) |.| (eps acc zero :: Interval a) &&
  (a + negate a) |.| (eps acc zero :: Interval a)

isMultiplicative :: forall a.(Show a, Epsilon a, Space (Interval a), Multiplicative a) => a -> Gen a -> [(PropertyName, Property)]
isMultiplicative acc src =
  [ ("one", isUnital one (*) acc src)
  , ("associative *", isAssociative (*) (*) acc src)
  , ("commutative *", isCommutative (*) (*) acc src)
  ]

isDivisive :: forall a.(Show a, Epsilon a, Space (Interval a), UpperBoundedField a, LowerBoundedField a) => a -> Gen a -> Property
isDivisive acc src = unary src $ \a ->
  (a / a) |.| (eps acc one :: Interval a) &&
  (recip a |.| (eps acc one / (eps acc a :: Interval a))) &&
  (recip a * a) |.| (eps acc one :: Interval a) &&
  (a * recip a) |.| (eps acc one :: Interval a)

isDistributiveTimesPlus :: forall a. (Show a, Epsilon a, Space (Interval a), Multiplicative a) => a -> Gen a -> Property
isDistributiveTimesPlus acc src = ternary src $ \a b c ->
  (a * (b + c)) |.| ((a .*. b) + (a .*. c)) &&
  ((a + b) * c) |.| ((a .*. c) + (b .*. c))
  where
    (.*.) x y = eps acc x * eps acc y :: Interval a

isDistributiveJoinMeet :: forall a. (Show a, Epsilon a, Space (Interval a), Multiplicative a) => a -> Gen a -> Property
isDistributiveJoinMeet acc src = ternary src $ \a b c ->
  (a \/ (b /\ c)) |.| ((a .\/. b) /\ (a .\/. c)) &&
  ((a /\ b) \/ c) |.| ((a .\/. c) /\ (b .\/. c))
  where
    (.\/.) x y = eps acc x \/ eps acc y :: Interval a

isAbsorbative :: forall a.(Show a, Epsilon a, Space (Interval a), Multiplicative a) => (a -> a -> a) -> a -> Gen a -> Property
isAbsorbative (#) acc src = unary src $ \a ->
  (a # zero) |.| (eps acc zero :: Interval a) &&
  (zero # a) |.| (eps acc zero :: Interval a)

isAbsorbative' :: forall a. (Show a, Epsilon a, Lattice a, Subtractive a, Multiplicative a) => (a -> a -> a) -> (a -> a -> a) -> (Interval a -> Interval a -> Interval a) -> (Interval a -> Interval a -> Interval a) -> a -> Gen a -> Property
isAbsorbative' (#) (%) (##) (%%) acc src = binary src $ \a b ->
  (a # (a % b)) |.| (eps acc a %% (eps acc a ## eps acc b)) &&
  a |.| (eps acc a %% (eps acc a ## eps acc b :: Interval a))

isSigned :: forall a.(Show a, Epsilon a, Space (Interval a), Signed a) => a -> Gen a -> Property
isSigned acc src = unary src $ \a ->
  (sign a * abs a) |.| (eps acc a :: Interval a)

isNormedUnbounded :: forall a. (Ord a, Show a, Epsilon a, Space (Interval a), Multiplicative a, Normed a a) => a -> Gen a -> Property
isNormedUnbounded acc src = unary src $ \a ->
  (normL1 a >= (zero :: a)) &&
  (normL1 (zero :: a) :: a) |.| (eps acc zero :: Interval a)

isMetricUnbounded :: forall a. (Show a, Epsilon a, Space (Interval a), Multiplicative a, Metric a a) => a -> Gen a -> Property
isMetricUnbounded acc src = ternary src $ \a b c ->
  singleton (distanceL1 a b) |>| (eps acc zero :: Interval a) ||
  distanceL1 a b |.| (eps acc zero  :: Interval a) &&
  distanceL1 a a |.| (eps acc zero :: Interval a) &&
  ((eps acc zero :: Interval a)
    |<| singleton (distanceL1 a c + distanceL1 b c - distanceL1 a b)) &&
  (eps acc zero :: Interval a)
  |<| singleton (distanceL1 a b + distanceL1 b c - distanceL1 a c) &&
  (eps acc zero :: Interval a)
  |<| singleton (distanceL1 a b + distanceL1 a c - distanceL1 b c)

isExpField :: forall a. (Show a, Epsilon a, Space (Interval a), ExpField a, Normed a a) => a -> Gen a -> Property
isExpField acc src = binary src $ \a b ->
  (not ((eps acc zero :: Interval a) |<| singleton a)
    || ((sqrt . (** (one + one)) $ a) |.| (eps acc a :: Interval a))
    && (((** (one + one)) . sqrt $ a) |.| (eps acc a :: Interval a))) &&
  (not ((eps acc zero :: Interval a) |<| singleton a)
    || ((log . exp $ a) |.| (eps acc a :: Interval a))
    && ((exp . log $ a) |.| (eps acc a :: Interval a))) &&
  (not ((eps acc zero :: Interval a) |<| singleton (normL1 b))
    || not (nearZero (a - zero))
    || (a |.| (eps acc one :: Interval a))
    || (a |.| (eps acc zero :: Interval a) &&
        nearZero (logBase a b))
    || (a ** logBase a b |.| (eps acc b :: Interval a)))

isCommutativeSpace :: forall s. (Epsilon (Element s), Multiplicative (Element s), Show s, Space s) => (s -> s -> s) -> Element s -> Gen s -> Property
isCommutativeSpace (#) acc src = binary src $ \a b ->
  (widenEps acc b # widenEps acc a) `contains` (a # b)

isAssociativeSpace :: forall s. (Epsilon (Element s), Multiplicative (Element s), Show s, Space s) => (s -> s -> s) -> Element s -> Gen s -> Property
isAssociativeSpace (#) acc src = ternary src $ \a b c ->
  ((widenEps acc a # widenEps acc b) # widenEps acc c) `contains`
  (a # (b # c))

isUnitalSpace :: forall s. (Epsilon (Element s), Multiplicative (Element s), Show s, Space s) => s -> (s -> s -> s) -> Element s -> Gen s -> Property
isUnitalSpace u (#) acc src = unary src $ \a ->
  (widenEps acc u # widenEps acc a) `contains` a &&
  (widenEps acc a # widenEps acc u) `contains` a

isDistributiveUI :: forall s. (Space s, Show s, Epsilon (Element s), Multiplicative (Element s)) => Element s -> Gen s -> Property
isDistributiveUI acc src = ternary src $ \a b c ->
  (widenEps acc a `intersection` (widenEps acc b `union` widenEps acc c)) `contains`
  ((a `intersection` b) `union` (a `intersection` c)) &&

  ((widenEps acc a `union` widenEps acc b) `intersection` widenEps acc c) `contains`
  ((a `intersection` c) `union` (b `intersection` c))

isContainedUnion :: forall s. (Epsilon (Element s), Multiplicative (Element s), Show s, Space s) => Element s -> Gen s -> Property
isContainedUnion acc src = binary src $ \a b ->
  (widenEps acc a `union` widenEps acc b) `contains` a &&
  (widenEps acc a `union` widenEps acc b) `contains` b
