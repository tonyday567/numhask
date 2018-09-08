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
  , HasRange a
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
  [ I.isAdditive one
  , \x -> [("subtractive", I.isSubtractive one x)]
  , I.isMultiplicative one
  , \x -> [("distributive", I.isDistributiveTimesPlus one x)]
  , \x -> [("absorbative", I.isZeroAbsorbative (*) one x)]
  , \x -> [("divisive", I.isDivisive one x)]
  , \x -> [("signed", I.isSigned one x)]
  , \x -> [("normed", I.isNormedUnbounded one x)]
  , \x -> [("metric", I.isMetricUnbounded one x)]
  , \x -> [("upper bounded field", isUpperBoundedField x)]
  , \x -> [("lower bounded field", isLowerBoundedField x)]
  , \x -> [("expField", I.isExpField 100.0 x)]
  ]

-- | quotient field laws
quotientFieldProps
  :: forall a.
  ( Show a
  , HasRange a
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
  ( I.CanMeasure (Complex a)
  , Epsilon a
  , LowerBoundedField a
  , UpperBoundedField a
  , FromRatio a
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
  ( I.CanMeasure a
  , LowerBoundedField a
  , UpperBoundedField a
  )
  => Gen a
  -> [(PropertyName, Property)]
logFieldProps g = mconcat $
  (\x -> x g) <$>
  [ I.isAdditive one
  , I.isMultiplicative one
  , \x -> [("distributive", I.isDistributiveTimesPlus one x)]
  , \x -> [("absorbative", I.isZeroAbsorbative (*) one x)]
  , \x -> [("divisive", I.isDivisive one x)]
  ]

-- | field laws
latticeProps
  :: forall a.
  ( I.CanMeasure a
  )
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
  , Eq s
  , Epsilon (Element s)
  , LowerBoundedField (Element s)
  , UpperBoundedField (Element s)
  )
  => Gen s
  -> [(PropertyName, Property)]
spaceProps g = mconcat $
  (\x -> x g) <$>
  [ \x -> [("commutative union", isCommutative union x)]
  , \x -> [("commutative intersection", isCommutative intersection x)]
  , \x -> [("associative union", isAssociative union x)]
  , \x -> [("associative intersection", isAssociative intersection x)]
  , \x -> [("unital union", isUnital (infinity >.< negInfinity) union x)]
  , \x -> [("unital intersection", isUnital whole intersection x)]
  , \x -> [("distributive", isDistributive (infinity >.< negInfinity) union intersection x)]
  , \x -> [("distributive", isDistributive whole intersection union x)]
  , \x -> [("containment", I.isContainedUnion one x)]
  , \x -> [("positive space", I.isLatticeSpace x)]
  ]

-- | space laws
fieldSpaceProps
  :: forall s.
  ( Show s
  , FieldSpace s
  , Epsilon (Element s)
  )
  => Gen s
  -> [(PropertyName, Property)]
fieldSpaceProps g = mconcat $
  (\x -> x g) <$>
  [ \x -> [("projective upper preserved", I.isProjectiveUpper x)]
  , \x -> [("projective lower preserved", I.isProjectiveLower two x)]
  ]

-- * Interval algebra
intervalIntegralAlgebraProps
  :: forall a.
  ( Eq a
  , Show a
  , Subtractive a
  , Multiplicative a
  -- , Divisive a
  , JoinSemiLattice a
  , MeetSemiLattice a
  )
  => Gen (Interval a)
  -> [(PropertyName, Property)]
intervalIntegralAlgebraProps g = mconcat $
  (\x -> x g) <$>
  [ isAdditive
  , \x -> [("subtractive interval laws with zero |.| a - a", isSubtractiveI x)]
  , isMultiplicative
  -- , isDivisive
  ]

-- | Intervals are not distributive
intervalFloatAlgebraProps
  :: forall a.
  ( Show a
  , Subtractive a
  , Divisive a
  , JoinSemiLattice a
  , UpperBoundedField a
  , LowerBoundedField a
  , Epsilon a
  )
  => Gen (Interval a)
  -> [(PropertyName, Property)]
intervalFloatAlgebraProps g = mconcat $
  (\x -> x g) <$>
  [ \x -> [("commutative (+))", I.isCommutativeSpace (+) one x)]
  , \x -> [("associative (+))", I.isAssociativeSpace (+) one x)]
  , \x -> [("unital (+))", I.isUnitalSpace zero (+) one x)]
  , \x -> [("subtractive interval laws with zero |.| a - a", isSubtractiveI x)]
  , \x -> [("commutative (*))", I.isCommutativeSpace (*) one x)]
  , \x -> [("associative (*))", I.isAssociativeSpace (*) one x)]
  , \x -> [("unital (*))", I.isUnitalSpace one (*) one x)]
  , \x -> [("divisive interval laws with one |.| a / a", isDivisiveI x)]
  ]

hullFloatAlgebraProps
  :: forall a.
  ( Show a
  , Subtractive a
  , Divisive a
  , JoinSemiLattice a
  , UpperBoundedField a
  , LowerBoundedField a
  , Epsilon a
  )
  => Gen (Hull a)
  -> [(PropertyName, Property)]
hullFloatAlgebraProps g = mconcat $
  (\x -> x g) <$>
  [ isAdditive
  , \x -> [("commutative (*))", I.isCommutativeSpace (*) one x)]
  , \x -> [("associative (*))", I.isAssociativeSpace (*) two x)]
  , \x -> [("unital (*))", I.isUnitalSpace one (*) one x)]
  ]
 
