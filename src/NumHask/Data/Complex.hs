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
    mkPolar,
    cis,
    polar,
    magnitude,
    phase,
  )
where

import Data.Data (Data)
import GHC.Generics (Generic, Generic1)
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Lattice
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
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

-- | Complex numbers are an algebraic type.
--
-- For a complex number @z@, @'abs' z@ is a number with the magnitude of @z@,
-- but oriented in the positive real direction, whereas @'sign' z@
-- has the phase of @z@, but unit magnitude.
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

instance
  (ExpField a, Normed a a) =>
  Normed (Complex a) a
  where
  norm (rx :+ ix) = norm rx + norm ix

instance
  (Subtractive a, ExpField a, Normed a a) =>
  Metric (Complex a) a
  where
  distance a b = norm (a - b)

instance
  (Ord a, Signed a, Subtractive a, Epsilon a) =>
  Epsilon (Complex a)
  where
  epsilon = epsilon :+ epsilon
  nearZero (a :+ b) = nearZero a && nearZero b

instance (IntegralDomain a, Subtractive a) => IntegralDomain (Complex a)

instance (Field a, Subtractive a) => Field (Complex a)

instance (Ord a, TrigField a, ExpField a, Subtractive a) => ExpField (Complex a) where
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

instance (UpperBoundedField a, IntegralDomain a, Subtractive a) => UpperBoundedField (Complex a)

instance (LowerBoundedField a) => LowerBoundedField (Complex a)

instance (JoinSemiLattice a) => JoinSemiLattice (Complex a) where
  (\/) (ar :+ ai) (br :+ bi) = (ar \/ br) :+ (ai \/ bi)

instance (MeetSemiLattice a) => MeetSemiLattice (Complex a) where
  (/\) (ar :+ ai) (br :+ bi) = (ar /\ br) :+ (ai /\ bi)

instance (BoundedJoinSemiLattice a) => BoundedJoinSemiLattice (Complex a) where
  bottom = bottom :+ bottom

instance (BoundedMeetSemiLattice a) => BoundedMeetSemiLattice (Complex a) where
  top = top :+ top

-- | Convert from a magnitude and angle.
mkPolar :: TrigField a => a -> a -> Complex a
mkPolar r theta = (r * cos theta) :+ (r * sin theta)

-- | @'cis' t@ is a complex value with magnitude @1@
-- and phase @t@ (modulo @2*'pi'@).
{-# SPECIALIZE cis :: Double -> Complex Double #-}
cis :: TrigField a => a -> Complex a
cis theta = cos theta :+ sin theta

-- | The function 'polar' takes a complex number and
-- returns a (magnitude, phase) pair in canonical form:
-- the magnitude is nonnegative, and the phase in the range @(-'pi', 'pi']@;
-- if the magnitude is zero, then so is the phase.
{-# SPECIALIZE polar :: Complex Double -> (Double, Double) #-}
polar :: (TrigField a, ExpField a) => Complex a -> (a, a)
polar z = (magnitude z, phase z)

-- | The nonnegative magnitude of a complex number.
{-# SPECIALIZE magnitude :: Complex Double -> Double #-}
magnitude :: (ExpField a) => Complex a -> a
magnitude (x :+ y) = sqrt (x*x + y*y)

-- | The phase of a complex number, in the range @(-'pi', 'pi']@.
-- If the magnitude is zero, then so is the phase.
{-# SPECIALIZE phase :: Complex Double -> Double #-}
phase :: (TrigField a) => Complex a -> a
phase (x :+ y) = atan2 y x
