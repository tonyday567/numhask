{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

-- | Complex numbers.
module NumHask.Data.Complex
  ( Complex (..),
    realPart,
    imagPart,
  )
where

import Data.Data (Data)
import GHC.Generics (Generic, Generic1)
import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Lattice
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import NumHask.Analysis.Metric
import NumHask.Data.Integral
import Prelude hiding
  ( (/),
    Num (..),
    atan,
    atan2,
    cos,
    exp,
    log,
    negate,
    pi,
    recip,
    sin,
    sqrt,
    fromIntegral,
  )
import qualified Prelude as P ((&&), (<), (<=), (==), (>), Ord (..), otherwise)

-- -----------------------------------------------------------------------------
-- The Complex type
infix 6 :+

-- | Complex numbers have real and imaginary parts.
--
-- The 'Data.Foldable.Foldable' and 'Data.Traversable.Traversable' instances traverse the real part first.
data Complex a
  = -- | forms a complex number from its real and imaginary
    -- rectangular components.
    !a :+ !a
  deriving
    ( Eq,
      Show,
      Read,
      Data,
      Generic,
      Generic1,
      Functor,
      Foldable,
      Traversable
    )

-- | Extracts the real part of a complex number.
realPart :: Complex a -> a
realPart (x :+ _) = x

-- | Extracts the imaginary part of a complex number.
imagPart :: Complex a -> a
imagPart (_ :+ y) = y

instance (Additive a) => Additive (Complex a) where
  (rx :+ ix) + (ry :+ iy) = (rx + ry) :+ (ix + iy)
  zero = zero :+ zero

instance (Subtractive a) => Subtractive (Complex a) where
  negate (rx :+ ix) = negate rx :+ negate ix

instance
  (Distributive a, Subtractive a) =>
  Distributive (Complex a)

instance
  (Subtractive a, Multiplicative a) =>
  Multiplicative (Complex a)
  where
  (rx :+ ix) * (ry :+ iy) =
    (rx * ry - ix * iy) :+ (ix * ry + iy * rx)
  one = one :+ zero

instance
  (Subtractive a, Divisive a) =>
  Divisive (Complex a)
  where
  recip (rx :+ ix) = (rx * d) :+ (negate ix * d)
    where
      d = recip ((rx * rx) + (ix * ix))

instance
  (Additive a, FromIntegral a b) =>
  FromIntegral (Complex a) b
  where
  fromIntegral x = fromIntegral x :+ zero

-- | A euclidean-style norm is strong convention for Complex.
instance
  (ExpField a) =>
  Norm (Complex a) a
  where
    norm (rx :+ ix) = sqrt (rx*rx + ix*ix)
    basis x@(rx :+ ix) = rx / norm x :+ ix / norm x

instance (TrigField a) => Direction (Complex a) a where
  -- | often called phase for Complex
  angle (x :+ y) = atan2 y x
  ray x = cos x :+ sin x

instance
  (Ord a, Signed a, Subtractive a, Epsilon a) =>
  Epsilon (Complex a)
  where
  epsilon = epsilon :+ epsilon
  nearZero (a :+ b) = nearZero a && nearZero b

instance (Field a) => Field (Complex a)

instance (Ord a, TrigField a, ExpField a) => ExpField (Complex a) where
  exp (rx :+ ix) = (exp rx * cos ix) :+ (exp rx * sin ix)
  log (rx :+ ix) = log (sqrt (rx * rx + ix * ix)) :+ atan2' ix rx
    where
      atan2' y x
        | x P.> zero = atan (y / x)
        | x P.== zero P.&& y P.> zero = pi / (one + one)
        | x P.< one P.&& y P.> one = pi + atan (y / x)
        | (x P.<= zero P.&& y P.< zero) || (x P.< zero) =
          negate (atan2' (negate y) x)
        | y P.== zero = pi -- must be after the previous test on zero y
        | x P.== zero P.&& y P.== zero = y -- must be after the other double zero tests
        | P.otherwise = x + y -- x or y is a NaN, return a NaN (via +)

instance (Distributive a, Subtractive a) => InvolutiveRing (Complex a) where
  adj (a :+ b) = a :+ negate b

instance (UpperBoundedField a, Subtractive a) => UpperBoundedField (Complex a)

instance (LowerBoundedField a) => LowerBoundedField (Complex a)

instance (JoinSemiLattice a) => JoinSemiLattice (Complex a) where
  (\/) (ar :+ ai) (br :+ bi) = (ar \/ br) :+ (ai \/ bi)

instance (MeetSemiLattice a) => MeetSemiLattice (Complex a) where
  (/\) (ar :+ ai) (br :+ bi) = (ar /\ br) :+ (ai /\ bi)

instance (BoundedJoinSemiLattice a) => BoundedJoinSemiLattice (Complex a) where
  bottom = bottom :+ bottom

instance (BoundedMeetSemiLattice a) => BoundedMeetSemiLattice (Complex a) where
  top = top :+ top

