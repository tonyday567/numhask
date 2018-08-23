{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wall #-}

module NumHask.Data.Positive where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Analysis.Metric
import NumHask.Data.Integral
import qualified Prelude as P
import GHC.Exts (fromString)
import Control.Exception
import Data.Typeable (Typeable)

newtype Positive a = Positive { unPositive :: a }
  deriving
    ( P.Show
    , P.Eq
    , P.Ord
    , Additive
    , Multiplicative
    , Divisive
    , Distributive
    , IntegralDomain
    , Field
    , ExpField
    , TrigField
    , Integral
    , Signed
    , Epsilon
    )

-- not sure if this is correct or needed
type role Positive representational

positive :: (P.Ord a, Additive a) => a -> P.Maybe (Positive a)
positive a
  | a P.< zero = P.Nothing
  | P.otherwise = P.Just (Positive a)

positive_ :: (P.Ord a, Additive a) => a -> Positive a
positive_ a
  | a P.< zero = throw (NumHaskError "positive number less than zero")
  | P.otherwise = Positive a

instance (P.Ord a, Subtractive a) => Subtractive (Positive a) where
  negate (Positive a)
    | a P.== zero = Positive zero
    | P.otherwise = throw (NumHaskError "negating a positive number")

  (Positive a) - (Positive b)
    | a P.>= b = Positive (a - b)
    | P.otherwise = throw (NumHaskError "subtracting a larger positive")

instance (P.Ord a, QuotientField a P.Integer) =>
  QuotientField (Positive a) (Positive P.Integer) where
  properFraction (Positive a) = let (i,r) = properFraction a in (Positive i, Positive r)

instance (UpperBoundedField a) =>
  UpperBoundedField (Positive a) where
  infinity = Positive infinity
  isNaN (Positive a) = isNaN a

instance (UpperBoundedField a) => P.Bounded (Positive a) where
  minBound = zero
  maxBound = infinity

-- Metric
instance (Normed a a) =>
  Normed a (Positive a) where
  normL1 a = Positive (normL1 a)
  normL2 a = Positive (normL2 a)
  normLp (Positive p) a = Positive (normLp p a)

instance (Subtractive a, Normed a a) => Metric a (Positive a) where
  distanceL1 a b = Positive P.$ normL1 (a - b)
  distanceL2 a b = Positive P.$ normL2 (a - b)
  distanceLp (Positive p) a b = Positive P.$ normLp p (a - b)




-- transfer to a new module
newtype NumHaskError = NumHaskError { errorMessage :: P.String }
  deriving (P.Show, Typeable)

instance Exception NumHaskError

