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

module NumHask.Data.Wrapped where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Analysis.Metric
import NumHask.Data.Integral
import NumHask.Data.Rational
import qualified Prelude as P

newtype Wrapped a = Wrapped { unWrapped :: a }
  deriving
    ( P.Show
    , P.Eq
    , P.Ord
    , Additive
    , Subtractive
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
    , UpperBoundedField
    , LowerBoundedField
    , ToInteger
    , FromInteger
    , FromRatio
    , ToRatio
    )

-- not sure if this is correct or needed
type role Wrapped representational

instance (P.Ord a, QuotientField a P.Integer) =>
  QuotientField (Wrapped a) (Wrapped P.Integer) where
  properFraction (Wrapped a) = let (i,r) = properFraction a in (Wrapped i, Wrapped r)

