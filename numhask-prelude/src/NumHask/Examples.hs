{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | NumHask usage examples
module NumHask.Examples
  (
    -- ** Imports and Pragmas
    -- $imports

    -- $setup
    -- ** Basic Arithmetic
    -- $basic

    -- ** Complex numbers
    -- $complex

    -- ** Vectors
    -- $vector

    -- ** Matrices
    -- $matrices
  )
where

import NumHask.Prelude

-- $imports
-- NumHask.Prelude is a replacement for the standard prelude with the 'NoImplicitPrelude' extension explicitly required.
--
-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XFlexibleContexts
-- >>> import NumHask.Prelude
--
-- $basic
-- 'Int', 'Integer', 'Double' and 'Float' are from base.  NumHask takes these classes and redefines the basic arithmetic operators.
--
-- >>> 1 + 1
-- 2
-- >>> 1 - 1
-- 0
-- >>> 1 * 1
-- 1
-- >>> 1.0 / 1.0
-- 1.0
--
-- Note that the literal numbers in the divide above defaulted to Float rather than Int.
--
-- >>> 1 / (1::Int)
-- ...
-- ... No instance for (Invertible (Product Int))
-- ...
--
-- >>> 1.0 / fromIntegral (1::Int)
-- 1.0
--
-- RebindableSyntax removes the Haskell98 link between literal numbers and base classes.  Literal numbers are pre-processed by ghc as `fromInteger 1` and `fromRational 1.0`.
--
-- >>> :t 1
-- 1 :: Num p => p
--
-- >>> :t 1.0
-- 1.0 :: Fractional p => p
--
-- >>> :set -XRebindableSyntax
-- >>> :t 1
-- 1 :: FromInteger a => a
--
-- >>> :t 1.0
-- 1.0 :: FromRatio b => b
--
-- 'Float' and 'Double' are 'NumHask.Algebra.Fields.Field' instances.
--
-- >>> zero == 0.0
-- True
-- >>> one == 1.0
-- True
-- >>> 1.0 + 1.0
-- 2.0
-- >>> 1.0 - 1.0
-- 0.0
-- >>> 1.0 * 1.0
-- 1.0
-- >>> 1.0 / 1.0
-- 1.0
--
-- 'QuotientField'
--
-- >>> 1 `div` 2
-- 0
-- >>> 3 `mod` 2
-- 1
--
-- 'BoundedField'
--
-- >>> 1.0/0.0
-- Infinity
-- >>> -1.0/0.0
-- -Infinity
-- >>> 0.0/0.0 + 1.0
-- NaN
--
-- should be Infinity
-- >>> one/zero
-- ...
-- ... No instance for (Invertible (Product ()))
-- ...
--
-- should be -Infinity
-- >>> -one/zero
-- ...
-- ... No instance for (Invertible (Sum ())) ...
-- ...
--
-- should be NaN
-- >>> zero/zero+one
-- ...
-- ... No instance for (Invertible (Product ()))
-- ...
--
-- 'ExpField'
--
-- >>> logBase 2 4
-- 2.0
-- >>> 2 ** 2
-- 4.0
-- >>> sqrt 4
-- 2.0
-- >>> exp 2
-- 7.38905609893065
-- >>> log 2
-- 0.6931471805599453
--
-- $complex
--
-- >>> let a = 1 :+ 2
-- >>> a
-- 1 :+ 2
-- >>> zero - a
-- (-1) :+ (-2)
-- >>> (1 :+ (-2)) * ((-2) :+ 4)
-- 6 :+ 8
-- >>> (1.0 :+ (-1.0)) / (2.0 :+ 2.0)
-- 0.0 :+ (-0.5)

newtype Positive a = Positive { unPositive :: a } deriving (Show, Eq)

instance (Magma (Sum a)) => Magma (Sum (Positive a)) where
  (Sum (Positive a)) `magma` (Sum (Positive b)) = Sum (Positive (a + b))

instance (Unital (Sum a)) => Unital (Sum (Positive a)) where
  unit = Sum (Positive zero)

instance (Associative (Sum a)) => Associative (Sum (Positive a))

instance (Commutative (Sum a)) => Commutative (Sum (Positive a))

instance (Invertible (Sum a)) => Invertible (Sum (Positive a)) where
  inv (Sum (Positive a)) = Sum (Positive (negate a))

instance (Multiplicative a) => Absorbing (Product (Positive a)) where
  absorb = Product (Positive zero')

instance (Distributive  a) => Distributive  (Positive a)

instance (Magma (Product a)) => Magma (Product (Positive a)) where
  (Product (Positive a)) `magma` (Product (Positive b)) =
    Product (Positive (a * b))

instance (Associative (Product a)) =>
  Associative (Product (Positive a))

instance (Unital (Product a)) => Unital (Product (Positive a)) where
  unit = Product one

instance (Commutative (Product a)) => Commutative (Product (Positive a))

instance (Invertible (Product a)) => Invertible (Product (Positive a)) where
  inv (Product (Positive a)) = Product (Positive (recip a))

-- FIXME: needs Invertible (Sum (Positive a)), or UndecidableInstances
instance (IntegralDomain a) => IntegralDomain (Positive a)

instance (UpperBoundedField a) =>
  UpperBoundedField (Positive a) where
  infinity = Positive infinity
  isNaN (Positive a) = isNaN a

instance (UpperBoundedField a) => Bounded (Positive a) where
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

instance (Invertible (Sum a), Normed a a) => Metric a (Positive a) where
  distanceL1 a b = Positive $ normL1 (a - b)
  distanceL2 a b = Positive $ normL2 (a - b)
  distanceLp (Positive p) a b = Positive $ normLp p (a - b)

instance (Invertible (Sum a), Normed a a) => Metric (Positive a) (Positive a) where
  distanceL1 (Positive a) (Positive b) = Positive $ normL1 (a - b)
  distanceL2 (Positive a) (Positive b) = Positive $ normL2 (a - b)
  distanceLp (Positive p) (Positive a) (Positive b) = Positive $ normLp p (a - b)
