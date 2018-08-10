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
-- ... No instance for (Absorbing (Product ()))
-- ...
--
-- should be -Infinity
-- >>> -one/zero
-- ...
-- ... No instance for (Absorbing (Product ()))
-- ...
--
-- should be NaN
-- >>> zero/zero+one
-- ...
-- ... No instance for (Absorbing (Product ()))
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
