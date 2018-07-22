{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module NumHask.Data.Complex where

import Data.Data (Data)
import GHC.Generics (Generic, Generic1)
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Analysis.Metric
import Prelude
  hiding (Num(..), (**), (/), atan, cos, exp, log, negate, pi, recip, sin, sqrt, isNaN)
import qualified Prelude as P (Ord(..), (&&), (<), (<=), (==), (>), otherwise)

-- -----------------------------------------------------------------------------
-- The Complex type
infix 6 :+

-- | Complex numbers are an algebraic type.
--
-- For a complex number @z@, @'abs' z@ is a number with the magnitude of @z@,
-- but oriented in the positive real direction, whereas @'sign' z@
-- has the phase of @z@, but unit magnitude.
--
-- The 'Foldable' and 'Traversable' instances traverse the real part first.
data Complex a =
  !a :+ !a -- ^ forms a complex number from its real and imaginary
                -- rectangular components.
  deriving ( Eq
           , Show
           , Read
           , Data
           , Generic
           , Generic1
           , Functor
           , Foldable
           , Traversable
           )

-- | Extracts the real part of a complex number.
realPart :: Complex a -> a
realPart (x :+ _) = x

-- | Extracts the imaginary part of a complex number.
imagPart :: Complex a -> a
imagPart (_ :+ y) = y

instance (Magma (Sum a)) => Magma (Sum (Complex a)) where
  (Sum (rx :+ ix)) `magma` (Sum (ry :+ iy)) =
    Sum $ (rx + ry) :+ (ix + iy)

instance (Unital (Sum a)) => Unital (Sum (Complex a)) where
  unit = Sum (zero :+ zero)

instance (Associative (Sum a)) => Associative (Sum (Complex a))

instance (Commutative (Sum a)) => Commutative (Sum (Complex a))

instance (Invertible (Sum a)) => Invertible (Sum (Complex a)) where
  inv (Sum (rx :+ ix)) = Sum $ negate rx :+ negate ix

instance (Invertible (Sum a), Multiplicative a) =>
  Absorbing (Product (Complex a)) where
  absorb = Product $ zero' :+ zero'

instance (Invertible (Sum a), Distributive a) =>
  Distributive (Complex a)

instance (Invertible (Sum a), Unital (Sum a), Unital (Product a)) =>
  Unital (Product (Complex a)) where
  unit = Product $ one :+ zero

instance
  ( Invertible (Sum a)
  , Magma (Product a)
  ) =>
  Magma (Product (Complex a)) where
  (Product (rx :+ ix)) `magma` (Product (ry :+ iy)) =
    Product $ (rx * ry - ix * iy) :+ (ix * ry + iy * rx)

instance (Invertible (Sum a), Magma (Product a)) =>
  Commutative (Product (Complex a))

instance (Additive a, Invertible (Sum a), Invertible (Product a)) =>
  Invertible (Product (Complex a)) where
  inv (Product (rx :+ ix)) = Product $ (rx * d) :+ (negate ix * d)
    where
      d = recip ((rx * rx) + (ix * ix))

instance (Magma (Product a), Invertible (Sum a)) =>
  Associative (Product (Complex a))

instance (Multiplicative a, ExpField a, Normed a a) =>
  Normed (Complex a) a where
  normL1 (rx :+ ix) = normL1 rx + normL1 ix
  normL2 (rx :+ ix) = sqrt (rx * rx + ix * ix)
  normLp p (rx :+ ix) = (normL1 rx ** p + normL1 ix ** p) ** (one / p)

instance (Multiplicative a, ExpField a, Normed a a) =>
  Metric (Complex a) a where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)
  distanceLp p a b = normLp p (a - b)

instance (Ord a, Signed a, AbelianGroup (Sum a), Epsilon a)
  => Epsilon (Complex a) where
  epsilon = epsilon :+ epsilon
  nearZero (a :+ b) = nearZero a && nearZero b

instance (IntegralDomain a) => IntegralDomain (Complex a)

instance (Field a) => Field (Complex a)

-- | FIXME: needs testing
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

instance (Distributive a, Invertible (Sum a)) => InvolutiveRing (Complex a) where
  adj (a :+ b) = a :+ negate b

instance (UpperBoundedField a, IntegralDomain a) => UpperBoundedField (Complex a) where
  isNaN (a :+ b) = isNaN a || isNaN b

instance (LowerBoundedField a) => LowerBoundedField (Complex a)

-- * Helpers from Data.Complex
mkPolar :: TrigField a => a -> a -> Complex a
mkPolar r theta = (r * cos theta) :+ (r * sin theta)

-- | @'cis' t@ is a complex value with magnitude @1@
-- and phase @t@ (modulo @2*'pi'@).
{-# SPECIALISE cis :: Double -> Complex Double #-}

cis :: TrigField a => a -> Complex a
cis theta = cos theta :+ sin theta

-- | The function 'polar' takes a complex number and
-- returns a (magnitude, phase) pair in canonical form:
-- the magnitude is nonnegative, and the phase in the range @(-'pi', 'pi']@;
-- if the magnitude is zero, then so is the phase.
{-# SPECIALISE polar :: Complex Double -> (Double, Double) #-}

polar :: (RealFloat a, ExpField a) => Complex a -> (a, a)
polar z = (magnitude z, phase z)

-- | The nonnegative magnitude of a complex number.
{-# SPECIALISE magnitude :: Complex Double -> Double #-}

magnitude :: (ExpField a, RealFloat a) => Complex a -> a
magnitude (x :+ y) = scaleFloat
  k
  (sqrt (sqr (scaleFloat mk x) + sqr (scaleFloat mk y)))
 where
  k = max (exponent x) (exponent y)
  mk = -k
  sqr z = z * z

-- | The phase of a complex number, in the range @(-'pi', 'pi']@.
-- If the magnitude is zero, then so is the phase.
{-# SPECIALISE phase :: Complex Double -> Double #-}

phase :: (RealFloat a) => Complex a -> a
phase (0 :+ 0) = 0 -- SLPJ July 97 from John Peterson
phase (x :+ y) = atan2 y x
