{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module NumHask.Hedgehog.Props where

import Hedgehog as H hiding (Range)
import NumHask.Hedgehog.Prop
import NumHask.Prelude hiding (isSigned)
import qualified NumHask.Hedgehog.Prop.Space as S

-- * properties/law groupings
integralProps
  :: forall a.
  ( Show a
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
  ( Show a
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
  ( Show a
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
  ( S.CanMeasure a
  , BoundedLattice a
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
  [ S.isAdditive one
  , \x -> [("subtractive", S.isSubtractive one x)]
  , S.isMultiplicative one
  , \x -> [("distributive", S.isDistributiveTimesPlus one x)]
  , \x -> [("absorbative", S.isZeroAbsorbative (*) one x)]
  , \x -> [("divisive", S.isDivisive one x)]
  , \x -> [("signed", S.isSigned one x)]
  , \x -> [("normed", S.isNormedUnbounded one x)]
  , \x -> [("metric", S.isMetricUnbounded one x)]
  , \x -> [("upper bounded field", isUpperBoundedField x)]
  , \x -> [("lower bounded field", isLowerBoundedField x)]
  , \x -> [("expField", S.isExpField 100.0 x)]
  ]

-- | quotient field laws
quotientFieldProps
  :: forall a.
  ( S.CanMeasure a
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
  ( S.CanMeasure (Complex a)
  , Epsilon a
  , BoundedLattice (Complex a)
  , Divisive a
  , FromRatio a
  )
  => Complex a
  -> Gen (Complex a)
  -> [(PropertyName, Property)]
complexFieldProps acc g = mconcat $
  (\x -> x g) <$>
  [ S.isAdditive acc
  , \x -> [("subtractive", S.isSubtractive acc x)]
  , S.isMultiplicative acc
  , \x -> [("distributive", S.isDistributiveTimesPlus acc x)]
  , \x -> [("absorbative", S.isZeroAbsorbative (*) acc x)]
  , \x -> [("divisive", S.isDivisive (100.0 :+ 50.0) x)]
  ]

-- | field laws
logFieldProps
  :: forall a.
  ( S.CanMeasure a
  , BoundedLattice a
  , Divisive a
  )
  => Gen a
  -> [(PropertyName, Property)]
logFieldProps g = mconcat $
  (\x -> x g) <$>
  [ S.isAdditive one
  , S.isMultiplicative one
  , \x -> [("distributive", S.isDistributiveTimesPlus one x)]
  , \x -> [("absorbative", S.isZeroAbsorbative (*) one x)]
  , \x -> [("divisive", S.isDivisive one x)]
  ]

-- | lattice laws
latticeProps
  :: forall a.
  ( S.CanMeasure a
  )
  => Gen a
  -> [(PropertyName, Property)]
latticeProps g = mconcat $
  (\x -> x g) <$>
  [ \x -> [("join idem", S.isIdempotent (\/) one x)]
  , \x -> [("meet idem", S.isIdempotent (/\) one x)]
  , \x -> [("join comm", S.isCommutative (\/) (\/) one x)]
  , \x -> [("meet comm", S.isCommutative (/\) (/\) one x)]
  , \x -> [("join assoc", S.isAssociative (\/) (\/) one x)]
  , \x -> [("meet assoc", S.isAssociative (/\) (/\) one x)]
  , \x -> [("lattice distributive", S.isDistributiveJoinMeet one x)]
  , \x -> [("lattice absorb", S.isAbsorbative (\/) (/\) (\/) (/\) one x)]
  ]

-- | space laws
spaceProps
  :: forall s.
  ( Show s
  , Space s
  , Monoid s
  , Eq s
  , Epsilon (Element s)
  , LowerBoundedField (Element s)
  , UpperBoundedField (Element s)
  , BoundedJoinSemiLattice (Element s)
  , BoundedMeetSemiLattice (Element s)
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
  , \x -> [("unital union", isUnital mempty mappend x)]
  , \x -> [("unital intersection", isUnital whole intersection x)]
  , \x -> [("distributive", isDistributive (infinity >.< negInfinity) union intersection x)]
  , \x -> [("distributive", isDistributive whole intersection union x)]
  , \x -> [("containment", S.isContainedUnion one x)]
  , \x -> [("positive space", S.isLatticeSpace x)]
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
  [ \x -> [("projective upper preserved", S.isProjectiveUpper x)]
  , \x -> [("projective lower preserved", S.isProjectiveLower two x)]
  ]

-- | Interval algebra is not distributive
spaceAlgebraProps
  :: forall s.
  ( Eq s
  , Show s
  , Space s
  , Subtractive s
  , Divisive s
  , S.CanMeasure (Element s)
  )
  => Gen s
  -> [(PropertyName, Property)]
spaceAlgebraProps g = mconcat $
  (\x -> x g) <$>
  [ \x -> [("commutative (+))", S.isCommutativeSpace (+) one x)]
  , \x -> [("associative (+))", S.isAssociativeSpace (+) one x)]
  , \x -> [("unital (+))", S.isUnitalSpace zero (+) one x)]
  , \x -> [("subtractive space laws with zero |.| a - a", S.isSubtractiveSpace x)]
  , \x -> [("commutative (*))", S.isCommutativeSpace (*) one x)]
  , \x -> [("associative (*))", S.isAssociativeSpace (*) one x)]
  , \x -> [("unital (*))", S.isUnitalSpace one (*) one x)]
  , \x -> [("divisive space laws with one |.| a / a", S.isDivisiveSpace x)]
  ]
