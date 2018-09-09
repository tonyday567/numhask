{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module NumHask.Hedgehog.Prop.Space where

import NumHask.Prelude hiding ((%), (.*.))
import Hedgehog as H hiding (Range)

type CanMeasure a = (Lattice a, Multiplicative a, Show a, Epsilon a)
 
-- * individual tests
isIdempotent :: forall a. (CanMeasure a) =>
  (Range a -> Range a -> Range a) -> a -> Gen a -> Property
isIdempotent (##) acc src = property $ do
  rv <- forAll src
  let p = \a ->
        a |.| (eps acc a ## eps acc a :: Range a)
  assert (p rv)

isCommutative :: forall a. (CanMeasure a) =>
  (a -> a -> a) -> (Range a -> Range a -> Range a) -> a -> Gen a -> Property
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
        (u # a) |.| (eps acc a :: Range a) &&
        (a # u) |.| (eps acc a :: Range a)
  assert (p rv)

isAssociative :: forall a. (CanMeasure a) =>
  (a -> a -> a) -> (Range a -> Range a -> Range a) -> a -> Gen a -> Property
isAssociative (#) (##) acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  rv'' <- forAll src
  let p = \a b c ->
        ((a # b) # c) |.| (eps acc a ## (eps acc b ## eps acc c))
  assert (p rv rv' rv'')

isAdditive :: forall a. (CanMeasure a) =>
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
        (a - a) |.| (eps acc zero :: Range a) &&
        (negate a |.| (eps acc zero - (eps acc a :: Range a))) &&
        (negate a + a) |.| (eps acc zero :: Range a) &&
        (a + negate a) |.| (eps acc zero :: Range a)
  assert (p rv)

isMultiplicative :: forall a. (CanMeasure a) =>
  a -> Gen a -> [(PropertyName, Property)]
isMultiplicative acc src =
  [ ("one", isUnital one (*) acc src)
  , ("associative *", isAssociative (*) (*) acc src)
  , ("commutative *", isCommutative (*) (*) acc src)
  ]

isDivisive :: forall a. (CanMeasure a, BoundedLattice a, Divisive a) =>
  a -> Gen a -> Property
isDivisive acc src = property $ do
  rv <- forAll src
  let p = \a ->
        (a / a) |.| (eps acc one :: Range a) &&
        (recip a |.| (eps acc one / (eps acc a :: Range a))) &&
        (recip a * a) |.| (eps acc one :: Range a) &&
        (a * recip a) |.| (eps acc one :: Range a)
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
      (.*.) x y = eps acc x * eps acc y :: Range a

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
      (.\/.) x y = eps acc x \/ eps acc y :: Range a

isZeroAbsorbative :: forall a. (CanMeasure a) =>
  (a -> a -> a) -> a -> Gen a -> Property
isZeroAbsorbative (#) acc src = property $ do
  rv <- forAll src
  let p = \a ->
        (a # zero) |.| (eps acc zero :: Range a) &&
        (zero # a) |.| (eps acc zero :: Range a)
  assert (p rv)

isAbsorbative :: forall a. (CanMeasure a) =>
  (a -> a -> a) -> (a -> a -> a) ->
  (Range a -> Range a -> Range a) -> (Range a -> Range a -> Range a) ->
  a -> Gen a -> Property
isAbsorbative (#) (%) (##) (%%) acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        (a # (a % b)) |.| (eps acc a %% (eps acc a ## eps acc b)) &&
        a |.| (eps acc a %% (eps acc a ## eps acc b :: Range a))
  assert (p rv rv')

isSigned :: forall a. (CanMeasure a, Signed a) => a -> Gen a -> Property
isSigned acc src = property $ do
  rv <- forAll src
  let p = \a ->
        (sign a * abs a) |.| (eps acc a :: Range a)
  assert (p rv)

isNormedUnbounded :: forall a. (CanMeasure a, Normed a a) =>
  a -> Gen a -> Property
isNormedUnbounded acc src = property $ do
  rv <- forAll src
  let p = \a ->
        (normL1 a `joinLeq` (zero :: a)) &&
        (normL1 (zero :: a) :: a) |.| (eps acc zero :: Range a)
  assert (p rv)

isMetricUnbounded :: forall a. (CanMeasure a, Metric a a) =>
  a -> Gen a -> Property
isMetricUnbounded acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  rv'' <- forAll src
  let p = \a b c ->
        singleton (distanceL1 a b) |>| (eps acc zero :: Range a) ||
        distanceL1 a b |.| (eps acc zero  :: Range a) &&
        distanceL1 a a |.| (eps acc zero :: Range a) &&
        ((eps acc zero :: Range a)
         |<| singleton (distanceL1 a c + distanceL1 b c - distanceL1 a b)) &&
        (eps acc zero :: Range a)
        |<| singleton (distanceL1 a b + distanceL1 b c - distanceL1 a c) &&
        (eps acc zero :: Range a)
        |<| singleton (distanceL1 a b + distanceL1 a c - distanceL1 b c)
  assert (p rv rv' rv'')

isExpField :: forall a. (CanMeasure a, ExpField a, Signed a) =>
  a -> Gen a -> Property
isExpField acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        (not ((eps acc zero :: Range a) |<| singleton a)
         || ((sqrt . (** (one + one)) $ a) |.| (eps acc a :: Range a))
         && (((** (one + one)) . sqrt $ a) |.| (eps acc a :: Range a))) &&
        (not ((eps acc zero :: Range a) |<| singleton a)
         || ((log . exp $ a) |.| (eps acc a :: Range a))
         && ((exp . log $ a) |.| (eps acc a :: Range a))) &&
        (not ((eps acc zero :: Range a) |<| singleton (abs b))
         || not (nearZero (a - zero))
         || (a |.| (eps acc one :: Range a))
         || (a |.| (eps acc zero :: Range a) &&
            nearZero (logBase a b))
         || (a ** logBase a b |.| (eps acc b :: Range a)))
  assert (p rv rv')

isCommutativeSpace :: forall s. (Epsilon (Element s), Multiplicative (Element s), Show s, Space s) =>
  (s -> s -> s) -> Element s -> Gen s -> Property
isCommutativeSpace (#) acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        (widenEps acc b # widenEps acc a) `contains` (a # b)
  assert (p rv rv')

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

isUnitalSpace :: forall s. (Epsilon (Element s), Multiplicative (Element s), Show s, Space s) =>
  s -> (s -> s -> s) -> Element s -> Gen s -> Property
isUnitalSpace u (#) acc src = property $ do
  rv <- forAll src
  let p = \a ->
        (widenEps acc u # widenEps acc a) `contains` a &&
        (widenEps acc a # widenEps acc u) `contains` a
  assert (p rv)

isLatticeSpace :: forall s. (Show s, Space s) =>
  Gen s -> Property
isLatticeSpace src = property $ do
  rv <- norm <$> forAll src
  let p = \a ->
        lower a \/ upper a == lower a &&
        lower a /\ upper a == upper a
  assert (p rv)

-- 'zero |.| a - a' not 'zero = a - a'
isSubtractiveSpace :: forall s. (Space s, Subtractive s, Eq s, CanMeasure (Element s), Show s) =>
  Gen s -> Property
isSubtractiveSpace src = property $ do
  rv <- forAll src
  let p = \a ->
        (zero |.| (a - a)) &&
        (negate a == zero - a) &&
        (zero |.| (negate a + a))
  assert (p rv) 

-- 'one |.| a / a' not 'one = a / a'
isDivisiveSpace :: forall s. (Space s, Divisive s, Eq s, CanMeasure (Element s)
                       , Show s) =>
  Gen s -> Property
isDivisiveSpace src = property $ do
  rv <- forAll src
  let p = \a ->
        (one |.| (a / a)) &&
        (recip a == one / a) &&
        (one |.| (recip a * a))
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

isProjectiveLower :: forall s. (FieldSpace s, Epsilon (Element s), Show s) =>
  Element s -> Gen s -> Property
isProjectiveLower acc src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        lower b |.| (eps acc (project a b (lower a)) :: NumHask.Prelude.Range (Element s))
  assert (p rv rv')

isProjectiveUpper :: forall s. (FieldSpace s, Epsilon (Element s), Show s) =>
  Gen s -> Property
isProjectiveUpper src = property $ do
  rv <- forAll src
  rv' <- forAll src
  let p = \a b ->
        upper b |.| ((project a b (upper a) +/- epsilon) :: NumHask.Prelude.Range (Element s))
  assert (p rv rv')

