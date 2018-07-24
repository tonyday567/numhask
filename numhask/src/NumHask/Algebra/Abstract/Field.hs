{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Field classes
module NumHask.Algebra.Abstract.Field
  ( Field
  , ExpField(..)
  , QuotientField(..)
  , UpperBoundedField(..)
  , LowerBoundedField(..)
  , BoundedField
  , TrigField(..)
  )
where

import Data.Bool (bool)
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Data.Integral
import qualified Prelude as P

import Prelude ((.), ($), fst, snd)

-- | A Field is a Integral domain in which every non-zero element has a multiplicative inverse.
--
-- A summary of the rules inherited from super-classes of Field
--
-- > zero + a == a
-- > a + zero == a
-- > (a + b) + c == a + (b + c)
-- > a + b == b + a
-- > a - a = zero
-- > negate a = zero - a
-- > negate a + a = zero
-- > a + negate a = zero
-- > one * a == a
-- > a * one == a
-- > (a * b) * c == a * (b * c)
-- > a * (b + c) == a * b + a * c
-- > (a + b) * c == a * c + b * c
-- > a * zero == zero
-- > zero * a == zero
-- > a * b == b * a
-- > a / a = one
-- > recip a = one / a
-- > recip a * a = one
-- > a * recip a = one
class (IntegralDomain a) =>
      Field a

instance Field P.Double

instance Field P.Float

instance Field b => Field (a -> b)

-- | A hyperbolic field class
--
-- > sqrt . (**2) == identity
-- > log . exp == identity
-- > for +ive b, a != 0,1: a ** logBase a b ≈ b
class (Field a) =>
      ExpField a where
  exp :: a -> a
  log :: a -> a
  logBase :: a -> a -> a
  logBase a b = log b / log a
  (**) :: a -> a -> a
  (**) a b = exp (log a * b)
  sqrt :: a -> a
  sqrt a = a ** (one / (one + one))

instance ExpField P.Double where
  exp = P.exp
  log = P.log
  (**) = (P.**)

instance ExpField P.Float where
  exp = P.exp
  log = P.log
  (**) = (P.**)

instance ExpField b => ExpField (a -> b) where
  exp f = exp . f
  log f = log . f
  logBase f f' = \a -> logBase (f a) (f' a)
  f ** f' = \a -> f a ** f' a
  sqrt f = sqrt . f

-- | quotient fields explode constraints if they allow for polymorphic integral types
--
-- > a - one < floor a <= a <= ceiling a < a + one
-- > round a == floor (a + one/(one+one))
--
-- fixme: had to redefine Signed operators here because of the Field import in Metric, itself due to Complex being defined there
class (Field a, Integral b) => QuotientField a b where
  properFraction :: a -> (b, a)

  round :: a -> b
  default round ::(P.Ord a, P.Eq b, Invertible (Sum b)) => a -> b
  round x = case properFraction x of
    (n,r) -> let
      m         = bool (n+one) (n-one) (r P.< zero)
      half_down = abs' r - (one/(one+one))
      abs' a
        | a P.< zero = negate a
        | P.otherwise = a
      in
        case P.compare half_down zero of
          P.LT -> n
          P.EQ -> bool m n (even n)
          P.GT -> m

  ceiling :: a -> b
  default ceiling ::(P.Ord a) => a -> b
  ceiling x = bool n (n+one) (r P.> zero)
    where (n,r) = properFraction x

  floor :: a -> b
  default floor ::(P.Ord a, Invertible (Sum b)) => a -> b
  floor x = bool n (n-one) (r P.< zero)
    where (n,r) = properFraction x

  truncate :: a -> b
  default truncate :: (P.Ord a) => a -> b
  truncate x = bool (ceiling x) (floor x) (x P.>= zero)

instance QuotientField P.Float P.Integer where
  properFraction = P.properFraction

instance QuotientField P.Double P.Integer where
  properFraction = P.properFraction

instance QuotientField b c => QuotientField (a -> b) (a -> c) where
  properFraction f = (fst . frac, snd . frac)
    where
      frac a = properFraction @b @c (f a)

  round f = round . f

  ceiling f = ceiling . f

  floor f = floor . f

-- | A bounded field includes the concepts of infinity and NaN, thus moving away from error throwing.
--
-- > one / zero + infinity == infinity
-- > infinity + a == infinity
-- > zero / zero != nan
--
-- Note the tricky law that, although nan is assigned to zero/zero, they are never-the-less not equal. A committee decided this.
class (IntegralDomain a) =>
      UpperBoundedField a where

  infinity :: a
  infinity = one / zero

  nan :: a
  nan = zero / zero

  isNaN :: a -> P.Bool

instance UpperBoundedField P.Float where
  isNaN = P.isNaN

instance UpperBoundedField P.Double where
  isNaN = P.isNaN

instance UpperBoundedField b => UpperBoundedField (a -> b) where
  infinity _ = infinity
  nan _ = nan

class (Field a) =>
      LowerBoundedField a where

  negInfinity :: a
  negInfinity = negate (one / zero)

instance LowerBoundedField P.Float

instance LowerBoundedField P.Double

instance LowerBoundedField b => LowerBoundedField (a -> b) where
  negInfinity _ = negInfinity

-- | todo: work out boundings for complex
-- as it stands now, complex is different eg
--
-- > one / (zero :: Complex Float) == nan
-- instance (UpperBoundedField a) =>
--   UpperBoundedField (Complex a)

class (UpperBoundedField a, LowerBoundedField a) => BoundedField a

instance (UpperBoundedField a, LowerBoundedField a) => BoundedField a

-- | Trigonometric Field
class (Field a) =>
      TrigField a where
  pi :: a
  sin :: a -> a
  cos :: a -> a
  tan :: a -> a
  tan x = sin x / cos x
  asin :: a -> a
  acos :: a -> a
  atan :: a -> a
  sinh :: a -> a
  cosh :: a -> a
  tanh :: a -> a
  tanh x = sinh x / cosh x
  asinh :: a -> a
  acosh :: a -> a
  atanh :: a -> a

instance TrigField P.Double where
  pi = P.pi
  sin = P.sin
  cos = P.cos
  asin = P.asin
  acos = P.acos
  atan = P.atan
  sinh = P.sinh
  cosh = P.cosh
  asinh = P.sinh
  acosh = P.acosh
  atanh = P.atanh

instance TrigField P.Float where
  pi = P.pi
  sin = P.sin
  cos = P.cos
  asin = P.asin
  acos = P.acos
  atan = P.atan
  sinh = P.sinh
  cosh = P.cosh
  asinh = P.sinh
  acosh = P.acosh
  atanh = P.atanh

instance TrigField b => TrigField (a -> b) where
  pi _ = pi
  sin f = sin . f
  cos f = cos . f
  asin f = asin . f
  acos f = acos . f
  atan f = atan . f
  sinh f = sinh . f
  cosh f = cosh . f
  asinh f = asinh . f
  acosh f = acosh . f
  atanh f = atanh . f