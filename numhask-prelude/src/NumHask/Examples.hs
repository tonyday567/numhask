{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
  ) where

import NumHask.Prelude

-- $imports
-- NumHask.Prelude is a replacement for the standard prelude with the 'NoImplicitPrelude' extension explicitly required.
--
-- $setup
-- >>> :set -XNoImplicitPrelude
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
-- >>> 1 / 1
-- 1.0
--
-- Note that the literal numbers in the divide above defaulted to Float rather than Int.
--
-- >>> 1 / (1::Int)
-- ...
-- ... No instance for (MultiplicativeGroup Int)
-- ...
--
-- >>> 1 / fromIntegral (1::Int)
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
-- >>> one/zero
-- Infinity
-- >>> -one/zero
-- -Infinity
-- >>> zero/zero+one
-- NaN
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
-- >>> (1 :+ (-1)) / (2 :+ 2)
-- 0.0 :+ (-0.5)

newtype PositiveFloat = PositiveFloat { unPositive :: Float } deriving (Show, Eq, AdditiveMagma, AdditiveAssociative, AdditiveUnital, AdditiveCommutative, Additive, MultiplicativeMagma, MultiplicativeUnital, MultiplicativeAssociative, MultiplicativeCommutative, Multiplicative, MultiplicativeInvertible, MultiplicativeGroup, Distribution, Semiring, Ring, CRing, Semifield, UpperBoundedField)

instance AdditiveInvertible PositiveFloat where
  negate _ = nan

instance AdditiveGroup PositiveFloat

instance Bounded PositiveFloat where
  minBound = zero
  maxBound = infinity

