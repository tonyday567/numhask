{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module NumHask.Hedgehog.Props where

import Hedgehog as H
import NumHask.Hedgehog.Prop
import NumHask.Prelude hiding (isSigned)
import qualified NumHask.Hedgehog.Prop.Interval as I

-- * properties/law groupings
integralProps
  :: forall a.
  ( Eq a
  , Show a
  , Distributive a
  , Subtractive a
  , Integral a
  , FromInteger a
  , ToInteger a
  , Signed a
  , Bounded a
  , Normed a a
  , Metric a a
  , JoinSemiLattice a
  )
  => Gen a
  -> [(PropertyName, Property)]
integralProps g = mconcat $
  (\x -> x g) <$>
  [ isAdditive
  , isSubtractive
  , isMultiplicative
  , \x -> [("distributive", isDistributive zero (+) (*) x)]
  , \x -> [("absorbative zero", isAbsorbativeUnit zero (*) x)]
  , \x -> [("integral", isIntegral x)]
  , \x -> [("fromIntegral", isFromIntegral x)]
  , \x -> [("signed", isSigned x)]
  , \x -> [("normed", isNormedBounded x)]
  , \x -> [("metric", isMetricBounded x)]
  ]

integralUnboundedProps
  :: forall a.
  ( Eq a
  , Show a
  , Distributive a
  , Subtractive a
  , Integral a
  , FromInteger a
  , ToInteger a
  , Signed a
  , Normed a a
  , Metric a a
  , JoinSemiLattice a
  )
  => Gen a
  -> [(PropertyName, Property)]
integralUnboundedProps g = mconcat $
  (\x -> x g) <$>
  [ isAdditive
  , isSubtractive
  , isMultiplicative
  , \x -> [("distributive", isDistributive zero (+) (*) x)]
  , \x -> [("absorbative zero", isAbsorbativeUnit zero (*) x)]
  , \x -> [("integral", isIntegral x)]
  , \x -> [("fromIntegral", isFromIntegral x)]
  , \x -> [("signed", isSigned x)]
  , \x -> [("normed", isNormedUnbounded x)]
  , \x -> [("metric", isMetricUnbounded x)]
  ]

naturalProps
  :: forall a.
  ( Eq a
  , Show a
  , Distributive a
  , Integral a
  , FromInteger a
  , ToInteger a
  , Signed a
  , Normed a a
  , JoinSemiLattice a
  )
  => Gen a
  -> [(PropertyName, Property)]
naturalProps g = mconcat $
  (\x -> x g) <$>
  [ isAdditive
  , isMultiplicative
  , \x -> [("distributive", isDistributive zero (+) (*) x)]
  , \x -> [("absorbative zero", isAbsorbativeUnit zero (*) x)]
  , \x -> [("integral", isIntegral x)]
  , \x -> [("fromIntegral", isFromIntegral x)]
  , \x -> [("signed", isSigned x)]
  , \x -> [("normed", isNormedUnbounded x)]
  ]

boolProps
  :: forall a.
  ( Show a
  , Ord a
  , Distributive a
  )
  => Gen a
  -> [(PropertyName, Property)]
boolProps g = mconcat $
  (\x -> x g) <$>
  [ isAdditive
  , isMultiplicative
  , \x -> [("idempotent +", isIdempotent (+) x)]
  , \x -> [("idempotent *", isIdempotent (*) x)]
  , \x -> [("distributive", isDistributive zero (+) (*) x)]
  , \x -> [("absorbative unit", isAbsorbativeUnit zero (*) x)]
  , \x -> [("absorbative", isAbsorbative (+) (*) x)]
  ]

rationalProps
  :: forall a.
  ( Show a
  , Ord a
  , Distributive a
  , Subtractive a
  , Divisive a
  , FromRatio a
  , ToRatio a
  , Signed a
  , Normed a a
  , Metric a a
  , JoinSemiLattice a
  )
  => Gen a
  -> [(PropertyName, Property)]
rationalProps g = mconcat $
  (\x -> x g) <$>
  [ isAdditive
  , isSubtractive
  , isMultiplicative
  , \x -> [("distributive", isDistributive zero (+) (*) x)]
  , \x -> [("absorbative unit", isAbsorbativeUnit zero (*) x)]
  , isDivisive
  , \x -> [("rational", isRational x)]
  , \x -> [("signed", isSigned x)]
  , \x -> [("normed", isNormedUnbounded x)]
  , \x -> [("metric", isMetricUnbounded x)]
  ]

-- | field laws
fieldProps
  :: forall a.
  ( Show a
  , Lattice a
  , Epsilon a
  , LowerBoundedField a
  , UpperBoundedField a
  , FromRatio a
  , ExpField a
  , Signed a
  , Normed a a
  , Metric a a
  )
  => Gen a
  -> [(PropertyName, Property)]
fieldProps g = mconcat $
  (\x -> x g) <$>
  [ I.isAdditive 1.0
  , \x -> [("subtractive", I.isSubtractive 1.0 x)]
  , I.isMultiplicative 1.0
  , \x -> [("distributive", I.isDistributiveTimesPlus one x)]
  , \x -> [("absorbative", I.isZeroAbsorbative (*) one x)]
  , \x -> [("divisive", I.isDivisive 1.0 x)]
  , \x -> [("signed", I.isSigned 1.0 x)]
  , \x -> [("normed", I.isNormedUnbounded 1.0 x)]
  , \x -> [("metric", I.isMetricUnbounded 1.0 x)]
  , \x -> [("upper bounded field", isUpperBoundedField x)]
  , \x -> [("lower bounded field", isLowerBoundedField x)]
  , \x -> [("expField", I.isExpField 100.0 x)]
  ]

-- | quotient field laws
quotientFieldProps
  :: forall a.
  ( Show a
  , Lattice a
  , Epsilon a
  , FromInteger a
  , QuotientField a Integer
  )
  => Gen a
  -> [(PropertyName, Property)]
quotientFieldProps g = mconcat $
  (\x -> x g) <$>
  [ \x -> [("quotient field", isQuotientIntegerField x)]
  ]

complexFieldProps
  :: forall a.
  ( Show a
  , Ord a
  , Lattice (Complex a)
  , Epsilon a
  , LowerBoundedField a
  , UpperBoundedField a
  , FromRatio a
  , Signed a
  )
  => Complex a
  -> Gen (Complex a)
  -> [(PropertyName, Property)]
complexFieldProps acc g = mconcat $
  (\x -> x g) <$>
  [ I.isAdditive acc
  , \x -> [("subtractive", I.isSubtractive acc x)]
  , I.isMultiplicative acc
  , \x -> [("distributive", I.isDistributiveTimesPlus acc x)]
  , \x -> [("absorbative", I.isZeroAbsorbative (*) acc x)]
  , \x -> [("divisive", I.isDivisive (100.0 :+ 50.0) x)]
  ]

-- | field laws
logFieldProps
  :: forall a.
  ( Show a
  , Epsilon a
  , LowerBoundedField a
  , UpperBoundedField a
  , FromRatio a
  , Lattice a
  )
  => Gen a
  -> [(PropertyName, Property)]
logFieldProps g = mconcat $
  (\x -> x g) <$>
  [ I.isAdditive 1.0
  , I.isMultiplicative 1.0
  , \x -> [("distributive", I.isDistributiveTimesPlus one x)]
  , \x -> [("absorbative", I.isZeroAbsorbative (*) one x)]
  , \x -> [("divisive", I.isDivisive one x)]
  ]

-- | field laws
latticeProps
  :: forall a.
  ( Show a
  , Epsilon a
  , Multiplicative a
  , Lattice a)
  => Gen a
  -> [(PropertyName, Property)]
latticeProps g = mconcat $
  (\x -> x g) <$>
  [ \x -> [("join idem", I.isIdempotent (\/) one x)]
  , \x -> [("meet idem", I.isIdempotent (/\) one x)]
  , \x -> [("join comm", I.isCommutative (\/) (\/) one x)]
  , \x -> [("meet comm", I.isCommutative (/\) (/\) one x)]
  , \x -> [("join assoc", I.isAssociative (\/) (\/) one x)]
  , \x -> [("meet assoc", I.isAssociative (/\) (/\) one x)]
  , \x -> [("lattice distributive", I.isDistributiveJoinMeet one x)]
  , \x -> [("lattice absorb", I.isAbsorbative (\/) (/\) (\/) (/\) one x)]
  ]

-- | space laws
spaceProps
  :: forall s.
  ( Show s
  , Space s
  , Epsilon (Element s)
  , LowerBoundedField (Element s)
  , UpperBoundedField (Element s)
  )
  => Gen s
  -> [(PropertyName, Property)]
spaceProps g = mconcat $
  (\x -> x g) <$>
  [ \x -> [("commutative union", I.isCommutativeSpace union one x)]
  , \x -> [("commutative intersection", I.isCommutativeSpace intersection one x)]
  , \x -> [("associative union", I.isAssociativeSpace union one x)]
  , \x -> [("associative intersection", I.isAssociativeSpace intersection one x)]
  , \x -> [("unital union", I.isUnitalSpace nul union one x)]
  , \x -> [("unital intersection", I.isUnitalSpace whole intersection one x)]
  , \x -> [("distributive", I.isDistributiveUI one x)]
  , \x -> [("containment", I.isContainedUnion one x)]
  ]


-- * Interval algebra
intervalAlgebraProps
  :: forall a.
  ( Eq a
  , Show a
  , Additive a
  -- , Distributive a
  -- , Subtractive a
  -- , Multiplicative a
  -- , UpperBoundedField a
  -- , LowerBoundedField a
  -- , Integral a
  -- , Signed a
  -- , Bounded a
  -- , Normed a a
  -- , Metric a a
  -- , JoinSemiLattice a
  )
  => Gen a
  -> [(PropertyName, Property)]
intervalAlgebraProps g = mconcat $
  (\x -> x g) <$>
  [ isAdditive
  -- intervals satisfy 'zero |.| a - a' not 'zero = a - a'
  -- , isSubtractive
  -- , isMultiplicative
  -- , isDivisive
  -- , \x -> [("distributive", isDistributive zero (+) (*) x)]
  -- , \x -> [("absorbative zero", isAbsorbativeUnit zero (*) x)]
  -- , \x -> [("upper bounded field", isUpperBoundedField x)]
  -- , \x -> [("lower bounded field", isLowerBoundedField x)]
  -- , \x -> [("exponential field", isExpField x)]
  -- , \x -> [("trigonometric field", isTrigField x)]
  -- , \x -> [("integral", isIntegral x)]
  -- , \x -> [("signed", isSigned x)]
  -- , \x -> [("normed", isNormedBounded x)]
  -- , \x -> [("metric", isMetricBounded x)]
  ]

