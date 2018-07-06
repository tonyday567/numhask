{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, GeneralizedNewtypeDeriving, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
module NumHask.Data.Complex where

import GHC.Generics (Generic, Generic1)
import Data.Data (Data)

import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Algebra.Abstract.Field
import NumHask.Analysis.Metric

import Prelude hiding (Num(..), negate, sin, cos, sqrt, (/), atan, pi, exp, log, recip, (**), Semigroup)
import qualified Prelude as P ( (&&), (>), (<=), (<), (==), otherwise, Ord(..) )

-- -----------------------------------------------------------------------------
-- The Complex type

infix  6  :+

-- | Complex numbers are an algebraic type.
--
-- For a complex number @z@, @'abs' z@ is a number with the magnitude of @z@,
-- but oriented in the positive real direction, whereas @'sign' z@
-- has the phase of @z@, but unit magnitude.
--
-- The 'Foldable' and 'Traversable' instances traverse the real part first.
data Complex a
  = !a :+ !a    -- ^ forms a complex number from its real and imaginary
                -- rectangular components.
        deriving (Eq, Show, Read, Data, Generic, Generic1
                , Functor, Foldable, Traversable)

-- | Extracts the real part of a complex number.
realPart :: Complex a -> a
realPart (x :+ _) =  x

-- | Extracts the imaginary part of a complex number.
imagPart :: Complex a -> a
imagPart (_ :+ y) =  y


instance (Magma (Sum a)) => Magma (Sum (Complex a)) where
  (Sum (rx :+ ix)) `comb` (Sum (ry :+ iy)) = Sum $ (rx `plus` ry) :+ (ix `plus` iy)

instance (Unital (Sum a)) => Unital (Sum (Complex a)) where
  unit = Sum (zero :+ zero)

instance (Semigroup (Sum a)) => Semigroup (Sum (Complex a))

instance (Commutative (Sum a)) => Commutative (Sum (Complex a))

instance (Invertible (Sum a)) => Invertible (Sum (Complex a)) where
  inv (Sum (rx :+ ix)) = Sum $ neg rx :+ neg ix

instance (Multiplication a, AbelianGroup (Sum a)) => Absorbing (Product (Complex a)) where
  absorb = Product $ zero' :+ zero'

instance (Distributive  a, AbelianGroup (Sum a)) => Distributive  (Complex a)

instance (AbelianGroup (Sum a), Unital (Product a)) => Unital (Product (Complex a)) where
  unit = Product $ one :+ zero

instance (Magma (Product a), AbelianGroup (Sum a)) => Magma (Product (Complex a)) where
  (Product (rx :+ ix)) `comb` (Product (ry :+ iy)) = Product $
    (rx `times` ry - ix `times` iy) :+ (ix `times` ry + iy `times` rx)

instance (Commutative (Product a), AbelianGroup (Sum a)) => Commutative (Product (Complex a))

instance (AbelianGroup (Sum a), Invertible (Product a)) => Invertible (Product (Complex a)) where
  inv (Product (rx :+ ix)) = Product $ (rx `times` d) :+ (neg ix `times` d)
    where
      d = recip ((rx `times` rx) `plus` (ix `times` ix))

instance (AbelianGroup (Sum a), Semigroup (Product a)) =>
         Semigroup (Product (Complex a))

instance (Multiplication a, ExpField a, Normed a a) =>
         Normed (Complex a) a where
  normL1 (rx :+ ix) = normL1 rx + normL1 ix
  normL2 (rx :+ ix) = sqrt (rx * rx + ix * ix)
  normLp p (rx :+ ix) = (normL1 rx ** p + normL1 ix ** p) ** (one / p)

instance (Multiplication a, ExpField a, Normed a a) => Metric (Complex a) a where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)
  distanceLp p a b = normLp p (a - b)

instance (IntegralDomain a) => IntegralDomain (Complex a)

instance (Field a) => Field (Complex a)

-- | todo: bottom is here somewhere???
-- | is it possible to get rid of P.Ord?
instance (P.Ord a, TrigField a, ExpField a) => ExpField (Complex a) where
  exp (rx :+ ix) = (exp rx * cos ix) :+ (exp rx * sin ix)
  log (rx :+ ix) = log (sqrt (rx * rx + ix * ix)) :+ atan2' ix rx
    where
      atan2' y x
        | x P.> zero = atan (y / x)
        | x P.== zero P.&& y P.> zero = pi / (one + one)
        | x P.< one P.&& y P.> one = pi + atan (y / x)
        | (x P.<= zero P.&& y P.< zero) || (x P.< zero) =
          neg (atan2' (neg y) x)
        | y P.== zero = pi -- must be after the previous test on zero y
        | x P.== zero P.&& y P.== zero = y -- must be after the other double zero tests
        | P.otherwise = x + y -- x or y is a NaN, return a NaN (via +)


-- * Helpers from Data.Complex 

mkPolar :: TrigField a => a -> a -> Complex a
mkPolar r theta  =  (r * cos theta) :+ (r * sin theta)


-- | @'cis' t@ is a complex value with magnitude @1@
-- and phase @t@ (modulo @2*'pi'@).
{-# SPECIALISE cis :: Double -> Complex Double #-}
cis              :: TrigField a => a -> Complex a
cis theta        =  cos theta :+ sin theta

-- | The function 'polar' takes a complex number and
-- returns a (magnitude, phase) pair in canonical form:
-- the magnitude is nonnegative, and the phase in the range @(-'pi', 'pi']@;
-- if the magnitude is zero, then so is the phase.
{-# SPECIALISE polar :: Complex Double -> (Double,Double) #-}
polar            :: (RealFloat a, ExpField a) => Complex a -> (a,a)
polar z          =  (magnitude z, phase z)

-- | The nonnegative magnitude of a complex number.
{-# SPECIALISE magnitude :: Complex Double -> Double #-}
magnitude :: (ExpField a, RealFloat a) => Complex a -> a
magnitude (x :+ y) =  scaleFloat k (sqrt (sqr (scaleFloat mk x) + sqr (scaleFloat mk y)))
                    where k  = max (exponent x) (exponent y)
                          mk = - k
                          sqr z = z * z

-- | The phase of a complex number, in the range @(-'pi', 'pi']@.
-- If the magnitude is zero, then so is the phase.
{-# SPECIALISE phase :: Complex Double -> Double #-}
phase :: (RealFloat a) => Complex a -> a
phase (0 :+ 0)   = 0            -- SLPJ July 97 from John Peterson
phase (x :+ y)   = atan2 y x
