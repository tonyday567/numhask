{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

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
  , Signed a
  , Bounded a
  , Normed a a
  , Metric a a
  , JoinSemiLattice a
  , FromIntegral a Integer
  , ToIntegral a Integer
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
  , \x -> [("ToIntegral", toFromIntegral x)]
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
  , \x -> [("signed", isSigned x)]
  , \x -> [("normed", isNormedUnbounded x)]
  , \x -> [("metric", isMetricUnbounded x)]
  ]

naturalProps
  :: forall a.
  ( Show a
  , Distributive a
  , Integral a
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
  , Signed a
  , Normed a a
  , Metric a a
  , JoinSemiLattice a
  , FromRatio a Integer
  , ToRatio a Integer
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
  , \x -> [("rational", toFromRatio x)]
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
  -- FixMe: unstable test at any tolerance
  -- , \x -> [("expField", S.isExpField 100.0 x)]
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
  , LowerBoundedField (Complex a)
  , UpperBoundedField (Complex a)
  , BoundedLattice (Complex a)
  , Divisive a
  , FromRational a
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
  , LowerBoundedField a
  , UpperBoundedField a
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

