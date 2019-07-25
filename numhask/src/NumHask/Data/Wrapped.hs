{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wall #-}

module NumHask.Data.Wrapped where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Algebra.Abstract.Lattice
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
    , JoinSemiLattice
    , MeetSemiLattice
    , Epsilon
    , UpperBoundedField
    , LowerBoundedField
    )

-- not sure if this is correct or needed
type role Wrapped representational

instance (P.Ord a, QuotientField a P.Integer) =>
  QuotientField (Wrapped a) (Wrapped P.Integer) where
  properFraction (Wrapped a) = let (i,r) = properFraction a in (Wrapped i, Wrapped r)

instance (FromIntegral a b) => FromIntegral (Wrapped a) b where
  fromIntegral_ a = Wrapped (fromIntegral_ a)

instance (ToIntegral a b) => ToIntegral (Wrapped a) b where
  toIntegral_ (Wrapped a) = toIntegral_ a

instance (FromRatio a b) => FromRatio (Wrapped a) b where
  fromRatio a = Wrapped (fromRatio a)

instance (ToRatio a b) => ToRatio (Wrapped a) b where
  toRatio (Wrapped a) = toRatio a

