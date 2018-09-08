{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module NumHask.Hedgehog.Prop.Interval where

import NumHask.Prelude hiding ((%), (.*.))
import Hedgehog as H

type CanMeasure a = (HasRange a, Multiplicative a, Show a, Epsilon a)

-- * individual tests
isIdempotent :: forall a. (CanMeasure a) =>
  (Interval a -> Interval a -> Interval a) -> a -> Gen a -> Property
isIdempotent (##) acc src = property $ do
  rv <- forAll src
  let p = \a ->
        a |.| (eps acc a ## eps acc a :: Interval a)
  assert (p rv)

isCommutative :: forall a. (CanMeasure a) =>
  (a -> a -> a) -> (Interval a -> Interval a -> Interval a) -> a -> Gen a -> Property
isCommutative (#) (##) acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        (a # b) |.| eps acc b ## eps acc a
  assert (p rv rv')

isUnital :: forall a. (CanMeasure a) =>
  a -> (a -> a -> a) -> a -> Gen a -> Property
isUnital u (#) acc src = property $ do
  rv <- forAll src
  let p = \a ->
        (u # a) |.| (eps acc a :: Interval a) &&
        (a # u) |.| (eps acc a :: Interval a)
  assert (p rv)

isAssociative :: forall a. (CanMeasure a) =>
  (a -> a -> a) -> (Interval a -> Interval a -> Interval a) -> a -> Gen a -> Property
isAssociative (#) (##) acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  rv'' <- forAll src
  let p = \a b c ->
        ((a # b) # c) |.| (eps acc a ## (eps acc b ## eps acc c))
  assert (p rv rv' rv'')

isAdditive :: forall a.(CanMeasure a) =>
  a -> Gen a -> [(PropertyName, Property)]
isAdditive acc src =
  [ ("zero", isUnital zero (+) acc src)
  , ("associative +", isAssociative (+) (+) acc src)
  , ("commutative +", isCommutative (+) (+) acc src)
  ]

isSubtractive :: forall a. (CanMeasure a) =>
  a -> Gen a -> Property
isSubtractive acc src = property $ do
  rv <- forAll src
  let p = \a -> 
        (a - a) |.| (eps acc zero :: Interval a) &&
        (negate a |.| (eps acc zero - (eps acc a :: Interval a))) &&
        (negate a + a) |.| (eps acc zero :: Interval a) &&
        (a + negate a) |.| (eps acc zero :: Interval a)
  assert (p rv)

isMultiplicative :: forall a. (CanMeasure a) =>
  a -> Gen a -> [(PropertyName, Property)]
isMultiplicative acc src =
  [ ("one", isUnital one (*) acc src)
  , ("associative *", isAssociative (*) (*) acc src)
  , ("commutative *", isCommutative (*) (*) acc src)
  ]

isDivisive :: forall a. (CanMeasure a, LowerBoundedField a, UpperBoundedField a) =>
  a -> Gen a -> Property
isDivisive acc src = property $ do
  rv <- forAll src
  let p = \a ->
        (a / a) |.| (eps acc one :: Interval a) &&
        (recip a |.| (eps acc one / (eps acc a :: Interval a))) &&
        (recip a * a) |.| (eps acc one :: Interval a) &&
        (a * recip a) |.| (eps acc one :: Interval a)
  assert (p rv)

isDistributiveTimesPlus :: forall a. (CanMeasure a) =>
  a -> Gen a -> Property
isDistributiveTimesPlus acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  rv'' <- forAll src
  let p = \a b c ->
        (a * (b + c)) |.| ((a .*. b) + (a .*. c)) &&
        ((a + b) * c) |.| ((a .*. c) + (b .*. c))
  assert (p rv rv' rv'')
    where
      (.*.) x y = eps acc x * eps acc y :: Interval a

isDistributiveJoinMeet :: forall a. (CanMeasure a) =>
  a -> Gen a -> Property
isDistributiveJoinMeet acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  rv'' <- forAll src
  let p = \a b c ->
        (a \/ (b /\ c)) |.| ((a .\/. b) /\ (a .\/. c)) &&
        ((a /\ b) \/ c) |.| ((a .\/. c) /\ (b .\/. c))
  assert (p rv rv' rv'')
    where
      (.\/.) x y = eps acc x \/ eps acc y :: Interval a

isZeroAbsorbative :: forall a. (CanMeasure a) =>
  (a -> a -> a) -> a -> Gen a -> Property
isZeroAbsorbative (#) acc src = property $ do
  rv <- forAll src
  let p = \a ->
        (a # zero) |.| (eps acc zero :: Interval a) &&
        (zero # a) |.| (eps acc zero :: Interval a)
  assert (p rv)

isAbsorbative :: forall a. (CanMeasure a) =>
  (a -> a -> a) -> (a -> a -> a) ->
  (Interval a -> Interval a -> Interval a) -> (Interval a -> Interval a -> Interval a) ->
  a -> Gen a -> Property
isAbsorbative (#) (%) (##) (%%) acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        (a # (a % b)) |.| (eps acc a %% (eps acc a ## eps acc b)) &&
        a |.| (eps acc a %% (eps acc a ## eps acc b :: Interval a))
  assert (p rv rv')

isSigned :: forall a. (CanMeasure a, Signed a) => a -> Gen a -> Property
isSigned acc src = property $ do
  rv <- forAll src
  let p = \a ->
        (sign a * abs a) |.| (eps acc a :: Interval a)
  assert (p rv)

isNormedUnbounded :: forall a. (CanMeasure a, Normed a a) =>
  a -> Gen a -> Property
isNormedUnbounded acc src = property $ do
  rv <- forAll src
  let p = \a ->
        (normL1 a `joinLeq` (zero :: a)) &&
        (normL1 (zero :: a) :: a) |.| (eps acc zero :: Interval a)
  assert (p rv)

isMetricUnbounded :: forall a. (CanMeasure a, Metric a a) =>
  a -> Gen a -> Property
isMetricUnbounded acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  rv'' <- forAll src
  let p = \a b c ->
        singleton (distanceL1 a b) |>| (eps acc zero :: Interval a) ||
        distanceL1 a b |.| (eps acc zero  :: Interval a) &&
        distanceL1 a a |.| (eps acc zero :: Interval a) &&
        ((eps acc zero :: Interval a)
         |<| singleton (distanceL1 a c + distanceL1 b c - distanceL1 a b)) &&
        (eps acc zero :: Interval a)
        |<| singleton (distanceL1 a b + distanceL1 b c - distanceL1 a c) &&
        (eps acc zero :: Interval a)
        |<| singleton (distanceL1 a b + distanceL1 a c - distanceL1 b c)
  assert (p rv rv' rv'')

isExpField :: forall a. (CanMeasure a, ExpField a, Signed a) =>
  a -> Gen a -> Property
isExpField acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        (not ((eps acc zero :: Interval a) |<| singleton a)
         || ((sqrt . (** (one + one)) $ a) |.| (eps acc a :: Interval a))
         && (((** (one + one)) . sqrt $ a) |.| (eps acc a :: Interval a))) &&
        (not ((eps acc zero :: Interval a) |<| singleton a)
         || ((log . exp $ a) |.| (eps acc a :: Interval a))
         && ((exp . log $ a) |.| (eps acc a :: Interval a))) &&
        (not ((eps acc zero :: Interval a) |<| singleton (abs b))
         || not (nearZero (a - zero))
         || (a |.| (eps acc one :: Interval a))
         || (a |.| (eps acc zero :: Interval a) &&
            nearZero (logBase a b))
         || (a ** logBase a b |.| (eps acc b :: Interval a)))
  assert (p rv rv')

isCommutativeSpace :: forall s. (Epsilon (Element s), Multiplicative (Element s), Show s, Space s) =>
  (s -> s -> s) -> Element s -> Gen s -> Property
isCommutativeSpace (#) acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        (widenEps acc b # widenEps acc a) `contains` (a # b)
  assert (p rv rv')

isCommutativeSpaceMaybe :: forall s. (Epsilon (Element s), Multiplicative (Element s), Show s, Space s) =>
  (s -> s -> Maybe s) -> Element s -> Gen s -> Property
isCommutativeSpaceMaybe (#) acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        contains <$> (widenEps acc b # widenEps acc a) <*> (a # b)
  assert (p rv rv' == Just True)

isAssociativeSpace :: forall s. (Epsilon (Element s), Multiplicative (Element s), Show s, Space s) =>
  (s -> s -> s) -> Element s -> Gen s -> Property
isAssociativeSpace (#) acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  rv'' <- forAll src
  let p = \a b c ->
        ((widenEps acc a # widenEps acc b) # widenEps acc c) `contains`
        (a # (b # c))
  assert (p rv rv' rv'')

isAssociativeSpaceMaybe :: forall s. (Epsilon (Element s), Multiplicative (Element s), Show s, Space s) =>
  (s -> s -> Maybe s) -> Element s -> Gen s -> Property
isAssociativeSpaceMaybe (#) acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  rv'' <- forAll src
  let p = \a b c ->
        contains <$>
        join ((#) <$> (widenEps acc a # widenEps acc b) <*> pure (widenEps acc c)) <*>
        join ((#) <$> pure a <*> (b # c))
  assert (p rv rv' rv'' == Just True)

isUnitalSpace :: forall s. (Epsilon (Element s), Multiplicative (Element s), Show s, Space s) =>
  s -> (s -> s -> s) -> Element s -> Gen s -> Property
isUnitalSpace u (#) acc src = property $ do
  rv <- forAll src
  let p = \a ->
        (widenEps acc u # widenEps acc a) `contains` a &&
        (widenEps acc a # widenEps acc u) `contains` a
  assert (p rv)

isContainedUnion :: forall s. (Epsilon (Element s), Multiplicative (Element s), Show s, Space s) =>
  Element s -> Gen s -> Property
isContainedUnion acc src = property $ do
  rv <- norm <$> forAll src
  rv' <- norm <$> forAll src
  let p = \a b ->
        (widenEps acc a `union` widenEps acc b) `contains` a &&
        (widenEps acc a `union` widenEps acc b) `contains` b
  assert (p rv rv')

isLatticeSpace :: forall s. (Show s, Space s) =>
  Gen s -> Property
isLatticeSpace src = property $ do
  rv <- norm <$> forAll src
  let p = \a ->
        lower a \/ upper a == lower a &&
        lower a /\ upper a == upper a
  assert (p rv)

isProjectiveLower :: forall s. (FieldSpace s, Epsilon (Element s), Show s) =>
  Element s -> Gen s -> Property
isProjectiveLower acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        lower b |.| (eps acc (project a b (lower a)) :: Hull (Element s))
  assert (p rv rv')

isProjectiveUpper :: forall s. (FieldSpace s, Epsilon (Element s), Show s) =>
  Gen s -> Property
isProjectiveUpper src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        upper b |.| ((project a b (upper a) +/- epsilon) :: Hull (Element s))
  assert (p rv rv')
