{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module NumHask.Data.Positive where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Analysis.Metric
import qualified Prelude as P

newtype Positive a = Positive { unPositive :: a } deriving (P.Show, P.Eq)

instance (Additive a) => Magma (Sum (Positive a)) where
  (Sum (Positive a)) `magma` (Sum (Positive b)) = Sum (Positive (a + b))

instance (Additive a) => Unital (Sum (Positive a)) where
  unit = Sum (Positive zero)

instance (Additive a) => Associative (Sum (Positive a))

instance (Additive a) => Commutative (Sum (Positive a))

instance (Subtractive a) => Invertible (Sum (Positive a)) where
  inv (Sum (Positive a)) = Sum (Positive (negate a))

instance (Multiplicative a) => Absorbing (Product (Positive a)) where
  absorb = Product (Positive zero')

instance (Distributive  a) => Distributive  (Positive a)

instance (Multiplicative a) => Magma (Product (Positive a)) where
  (Product (Positive a)) `magma` (Product (Positive b)) =
    Product (Positive (a * b))

instance (Multiplicative a) =>
  Associative (Product (Positive a))

instance (Multiplicative a) => Unital (Product (Positive a)) where
  unit = Product one

instance (Multiplicative a) => Commutative (Product (Positive a))

instance (Divisive a) => Invertible (Product (Positive a)) where
  inv (Product (Positive a)) = Product (Positive (recip a))

instance (IntegralDomain a) => IntegralDomain (Positive a)

instance (UpperBoundedField a) =>
  UpperBoundedField (Positive a) where
  infinity = Positive infinity
  isNaN (Positive a) = isNaN a

instance (UpperBoundedField a) => P.Bounded (Positive a) where
  minBound = zero
  maxBound = infinity

instance (Normed a a) =>
  Normed a (Positive a) where
  normL1 a = Positive (normL1 a)
  normL2 a = Positive (normL2 a)
  normLp (Positive p) a = Positive (normLp p a)

instance (Normed a a) =>
  Normed (Positive a) (Positive a) where
  normL1 (Positive a) = Positive (normL1 a)
  normL2 (Positive a) = Positive (normL2 a)
  normLp (Positive p) (Positive a) = Positive (normLp p a)

instance (Subtractive a, Normed a a) => Metric a (Positive a) where
  distanceL1 a b = Positive P.$ normL1 (a - b)
  distanceL2 a b = Positive P.$ normL2 (a - b)
  distanceLp (Positive p) a b = Positive P.$ normLp p (a - b)

instance (Subtractive a, Normed a a) => Metric (Positive a) (Positive a) where
  distanceL1 (Positive a) (Positive b) = Positive P.$ normL1 (a - b)
  distanceL2 (Positive a) (Positive b) = Positive P.$ normL2 (a - b)
  distanceLp (Positive p) (Positive a) (Positive b) = Positive P.$ normLp p (a - b)
