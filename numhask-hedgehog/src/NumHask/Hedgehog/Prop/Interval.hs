{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

module NumHask.Hedgehog.Prop.Interval where

import NumHask.Prelude
import Hedgehog as H
import NumHask.Data.Interval
import NumHask.Hedgehog.Prop

-- * individual tests
isIdempotentPlus :: (Epsilon a, CanInterval a, Multiplicative a, Subtractive a, Show a) => a -> Gen a -> Property
isIdempotentPlus acc src = unary src $ \a ->
  a =.= (eps acc a + eps acc a)

isIdempotentTimes :: (Epsilon a, CanInterval a, Multiplicative a, Subtractive a, Show a) => a -> Gen a -> Property
isIdempotentTimes acc src = unary src $ \a ->
  a =.= (eps acc a * eps acc a)

isCommutativePlus :: (Epsilon a, CanInterval a, Multiplicative a, Subtractive a, Show a) => a -> Gen a -> Property
isCommutativePlus acc src = binary src $ \a b ->
  (a + b) =.= (eps acc b + eps acc a)

isCommutativeTimes :: (Epsilon a, CanInterval a, Multiplicative a, Subtractive a, Show a) => a -> Gen a -> Property
isCommutativeTimes acc src = binary src $ \a b ->
  (a * b) =.= (eps acc b * eps acc a)

isUnitalPlus :: (Epsilon a, CanInterval a, Multiplicative a, Subtractive a, Show a) => a -> Gen a -> Property
isUnitalPlus acc src = unary src $ \a ->
  (zero + a) =.= eps acc a && (a + zero) =.= eps acc a

isUnitalTimes :: (Epsilon a, CanInterval a, Multiplicative a, Subtractive a, Show a) => a -> Gen a -> Property
isUnitalTimes acc src = unary src $ \a ->
  (one * a) =.= eps acc a && (a * one) =.= eps acc a

isAssociativePlus :: (Epsilon a, CanInterval a, Multiplicative a, Subtractive a, Show a) => a -> Gen a -> Property
isAssociativePlus acc src = ternary src $ \a b c ->
  ((a + b) + c) =.= (eps acc a + (eps acc b + eps acc c))

isAssociativeTimes :: (Epsilon a, CanInterval a, Multiplicative a, Subtractive a, Show a) => a -> Gen a -> Property
isAssociativeTimes acc src = ternary src $ \a b c ->
  ((a * b) * c) =.= (eps acc a * (eps acc b * eps acc c))

isAdditive :: (Show a, Epsilon a, CanInterval a, Subtractive a, Multiplicative a) => a -> Gen a -> [(PropertyName, Property)]
isAdditive acc src =
  [ ("zero", isUnitalPlus acc src)
  , ("associative +", isAssociativePlus acc src)
  , ("commutative +", isCommutativePlus acc src)
  ]

isSubtractive :: (Show a, Epsilon a, CanInterval a, Subtractive a, Divisive a) => a -> Gen a -> Property
isSubtractive acc src = unary src $ \a -> 
  (a - a) =.= eps acc zero &&
  (negate a =.= (eps acc zero - eps acc a)) &&
  (negate a + a) =.= eps acc zero &&
  (a + negate a) =.= eps acc zero

isMultiplicative :: (Show a, Epsilon a, CanInterval a, Subtractive a, Multiplicative a) => a -> Gen a -> [(PropertyName, Property)]
isMultiplicative acc src =
  [ ("zero", isUnitalTimes acc src)
  , ("associative +", isAssociativeTimes acc src)
  , ("commutative +", isCommutativeTimes acc src)
  ]

isDivisive :: (Show a, Epsilon a, CanInterval a, Divisive a, BoundedField a) => a -> Gen a -> Property
isDivisive acc src = unary src $ \a ->
  (a / a) =.= eps acc one &&
  (recip a =.= (eps acc one / eps acc a)) &&
  (recip a * a) =.= eps acc one &&
  (a * recip a) =.= eps acc one

isDistributive :: (Show a, Epsilon a, CanInterval a, Subtractive a, Multiplicative a) => a -> Gen a -> Property
isDistributive acc src = ternary src $ \a b c ->
  (a * zero) =.= zero &&
  (zero * a) =.= zero &&
  (a * (b + c)) =.= ((eps acc a * eps acc b) + (eps acc a * eps acc c)) &&
  ((a + b) * c) =.= ((eps acc a * eps acc c) + (eps acc b * eps acc c))

isSigned :: (Show a, Epsilon a, CanInterval a, Subtractive a, Multiplicative a, Signed a) => a -> Gen a -> Property
isSigned acc src = unary src $ \a ->
  (sign a * abs a) =.= eps acc a

isNormedUnbounded :: forall a. (Ord a, Show a, Epsilon a, CanInterval a, Subtractive a, Multiplicative a, Normed a a) => a -> Gen a -> Property
isNormedUnbounded acc src = unary src $ \a ->
  normL1 a >= (zero :: a) &&
  normL1 (zero :: a) =.= eps acc zero

isMetricUnbounded :: forall a. (Ord a, Show a, Epsilon a, CanInterval a, Subtractive a, Multiplicative a, Metric a a) => a -> Gen a -> Property
isMetricUnbounded acc src = ternary src $ \a b c ->
  eps acc zero `above` distanceL1 a b ||
  distanceL1 a b =.= eps acc zero &&
  distanceL1 a a =.= eps acc zero &&
  (eps acc zero
    `above` (distanceL1 a c + distanceL1 b c - distanceL1 a b)) &&
  eps acc zero
  `above` (distanceL1 a b + distanceL1 b c - distanceL1 a c) &&
  eps acc zero
  `above` (distanceL1 a b + distanceL1 a c - distanceL1 b c)

isExpField :: forall a. (Ord a, Show a, Epsilon a, Subtractive a, CanInterval a, ExpField a, Normed a a) => a -> Gen a -> Property
isExpField acc src = binary src $ \a b ->
  (not (eps acc zero `above` a)
    || ((sqrt . (** (one + one)) $ a) =.= eps acc a)
    && (((** (one + one)) . sqrt $ a) =.= eps acc a)) &&
  (not (eps acc zero `above` a)
    || ((log . exp $ a) =.= eps acc a)
    && ((exp . log $ a) =.= eps acc a)) &&
  (not (eps acc zero `above` normL1 b)
    || not (nearZero (a - zero))
    || (a =.= eps acc one)
    || (a =.= eps acc zero &&
        nearZero (logBase a b))
    || (a ** logBase a b =.= eps acc b))
