{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

-- | Field classes
module NumHask.Algebra.Field
  ( Semifield
  , Field
  , ExpField(..)
  , QuotientField(..)
  , UpperBoundedField(..)
  , LowerBoundedField(..)
  , TrigField(..)
  ) where

import Data.Complex (Complex(..))
import NumHask.Algebra.Additive
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import Data.Bool (bool)
import Prelude (Bool, Double, Float, Integer, (||))
import qualified Prelude as P

-- | A Semifield is chosen here to be a Field without an Additive Inverse
class (MultiplicativeInvertible a, MultiplicativeGroup a, Semiring a) =>
      Semifield a

instance Semifield Double

instance Semifield Float

instance (Semifield a, AdditiveGroup a) => Semifield (Complex a)

-- | A Field is a Ring plus additive invertible and multiplicative invertible operations.
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
class (AdditiveGroup a, MultiplicativeGroup a, Ring a) =>
      Field a

instance Field Double

instance Field Float

instance (Field a) => Field (Complex a)

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

instance ExpField Double where
  exp = P.exp
  log = P.log
  (**) = (P.**)

instance ExpField Float where
  exp = P.exp
  log = P.log
  (**) = (P.**)

-- | todo: bottom is here somewhere???
instance (TrigField a, ExpField a) => ExpField (Complex a) where
  exp (rx :+ ix) = exp rx * cos ix :+ exp rx * sin ix
  log (rx :+ ix) = log (sqrt (rx * rx + ix * ix)) :+ atan2 ix rx

-- | quotient fields explode constraints if they allow for polymorphic integral types
--
-- > a - one < floor a <= a <= ceiling a < a + one
-- > round a == floor (a + one/(one+one))
--
-- fixme: had to redefine Signed operators here because of the Field import in Metric, itself due to Complex being defined there
class (P.Ord a, Field a) =>
      QuotientField a where
  properFraction :: a -> (Integer, a)

  round :: a -> Integer
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
          P.EQ -> bool m n (P.even n)
          P.GT -> m

  ceiling :: a -> Integer
  ceiling x = bool n (n+one) (r P.> zero)
    where (n,r) = properFraction x

  floor :: a -> Integer
  floor x = bool n (n-one) (r P.< zero)
    where (n,r) = properFraction x

instance QuotientField Float where
  properFraction = P.properFraction

instance QuotientField Double where
  properFraction = P.properFraction

-- | A bounded field includes the concepts of infinity and NaN, thus moving away from error throwing.
--
-- > one / zero + infinity == infinity
-- > infinity + a == infinity
-- > isNaN (infinity - infinity)
-- > isNaN (infinity / infinity)
-- > isNaN (nan + a)
-- > zero / zero != nan
--
-- Note the tricky law that, although nan is assigned to zero/zero, they are never-the-less not equal. A committee decided this.
class (Semifield a) =>
      UpperBoundedField a where

  infinity :: a
  infinity = one / zero
  nan :: a
  nan = zero / zero

  isNaN :: a -> Bool

instance UpperBoundedField Float where
  isNaN = P.isNaN

instance UpperBoundedField Double where
  isNaN = P.isNaN

class (Field a) =>
      LowerBoundedField a where

  negInfinity :: a
  negInfinity = negate (one / zero)

instance LowerBoundedField Float

instance LowerBoundedField Double

-- | todo: work out boundings for complex
-- as it stands now, complex is different eg
--
-- > one / (zero :: Complex Float) == nan
instance (AdditiveGroup a, UpperBoundedField a) =>
  UpperBoundedField (Complex a) where
  isNaN (rx :+ ix) = isNaN rx || isNaN ix

-- | Trigonometric Field
class (P.Ord a, Field a) =>
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
  atan2 :: a -> a -> a
  atan2 y x
    | x P.> zero = atan (y / x)
    | x P.== zero P.&& y P.> zero = pi / (one + one)
    | x P.< one P.&& y P.> one = pi + atan (y / x)
    | (x P.<= zero P.&& y P.< zero) || (x P.< zero) =
      negate (atan2 (negate y) x)
    | y P.== zero = pi -- must be after the previous test on zero y
    | x P.== zero P.&& y P.== zero = y -- must be after the other double zero tests
    | P.otherwise = x + y -- x or y is a NaN, return a NaN (via +)

instance TrigField Double where
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

instance TrigField Float where
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
