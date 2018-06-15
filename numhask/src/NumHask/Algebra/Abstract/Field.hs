{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
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
  ) where

import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Ring
import NumHask.Algebra.Abstract.Multiplication
import NumHask.Algebra.Abstract.Addition
import NumHask.Algebra.Integral
import Data.Bool (bool)
import qualified Prelude                       as P

-- | A Field is a Intetral domain in which every non-zero element has a multiplicative inverse.
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

-- instance Field P.Double

-- instance Field P.Float

-- instance (Field a) => Field (Complex a)

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

-- instance ExpField P.Double where
--   exp = P.exp
--   log = P.log
--   (**) = (P.**)

-- instance ExpField P.Float where
--   exp = P.exp
--   log = P.log
--   (**) = (P.**)

-- | todo: bottom is here somewhere???
-- instance (P.Ord a, TrigField a, ExpField a) => ExpField (Complex a) where
--   exp (rx :+ ix) = exp rx * cos ix :+ exp rx * sin ix
--   log (rx :+ ix) = log (sqrt (rx * rx + ix * ix)) :+ atan2 ix rx
--     where
--       atan2 y x
--         | x P.> zero = atan (y / x)
--         | x P.== zero P.&& y P.> zero = pi / (one + one)
--         | x P.< one P.&& y P.> one = pi + atan (y / x)
--         | (x P.<= zero P.&& y P.< zero) || (x P.< zero) =
--           negate (atan2 (negate y) x)
--         | y P.== zero = pi -- must be after the previous test on zero y
--         | x P.== zero P.&& y P.== zero = y -- must be after the other double zero tests
--         | P.otherwise = x + y -- x or y is a NaN, return a NaN (via +)

-- | quotient fields explode constraints if they allow for polymorphic integral types
--
-- > a - one < floor a <= a <= ceiling a < a + one
-- > round a == floor (a + one/(one+one))
--
-- fixme: had to redefine Signed operators here because of the Field import in Metric, itself due to Complex being defined there
class (Field a, Integral b) => QuotientField a b where
  properFraction :: a -> (b, a)

  round :: a -> b
  default round ::(P.Ord a, P.Eq b, Invertible (Add b)) => a -> b
  round x = case properFraction x of
    (n,r) -> let
      m         = bool (n+one) (n-one) (r P.< zero)
      half_down = abs' r - (one/(one+one))
      abs' a
        | a P.< zero = neg a
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
  default floor ::(P.Ord a, Invertible (Add b)) => a -> b
  floor x = bool n (n-one) (r P.< zero)
    where (n,r) = properFraction x

-- instance QuotientField P.Float P.Integer where
--   properFraction = P.properFraction

-- instance QuotientField P.Double P.Integer where
--   properFraction = P.properFraction

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

  isNan :: a -> P.Bool

-- instance UpperBoundedField P.Float

-- instance UpperBoundedField P.Double

class (Field a) =>
      LowerBoundedField a where

  negInfinity :: a
  negInfinity = neg (one / zero)

-- instance LowerBoundedField P.Float

-- instance LowerBoundedField P.Double

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

-- instance TrigField P.Double where
--   pi = P.pi
--   sin = P.sin
--   cos = P.cos
--   asin = P.asin
--   acos = P.acos
--   atan = P.atan
--   sinh = P.sinh
--   cosh = P.cosh
--   asinh = P.sinh
--   acosh = P.acosh
--   atanh = P.atanh

-- instance TrigField P.Float where
--   pi = P.pi
--   sin = P.sin
--   cos = P.cos
--   asin = P.asin
--   acos = P.acos
--   atan = P.atan
--   sinh = P.sinh
--   cosh = P.cosh
--   asinh = P.sinh
--   acosh = P.acosh
--   atanh = P.atanh
