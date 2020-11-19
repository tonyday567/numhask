{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

-- | Field classes
module NumHask.Algebra.Field
  ( Field,
    ExpField (..),
    QuotientField (..),
    UpperBoundedField (..),
    LowerBoundedField (..),
    TrigField (..),
    half,
  )
where

import Data.Bool (bool)
import NumHask.Algebra.Additive
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import NumHask.Data.Integral
import Prelude ((.))
import qualified Prelude as P

-- $setup
--
-- >>> :set -XRebindableSyntax
-- >>> :set -XNegativeLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XScopedTypeVariables
-- >>> import NumHask.Prelude
-- >>> import Test.QuickCheck

-- | A <https://en.wikipedia.org/wiki/Field_(mathematics) Field> is a set
--   on which addition, subtraction, multiplication, and division are defined. It is also assumed that multiplication is distributive over addition.
--
-- A summary of the rules inherited from super-classes of Field. Floating point computation is a terrible, messy business and, in practice, only rough approximation can be achieve for association and distribution.
--
-- > zero + a == a
-- > a + zero == a
-- > ((a + b) + c) (a + (b + c))
-- > a + b == b + a
-- > a - a == zero
-- > negate a == zero - a
-- > negate a + a == zero
-- > a + negate a == zero
-- > one * a == a
-- > a * one == a
-- > ((a * b) * c) == (a * (b * c))
-- > (a * (b + c)) == (a * b + a * c)
-- > ((a + b) * c) == (a * c + b * c)
-- > a * zero == zero
-- > zero * a == zero
-- > a / a == one || a == zero
-- > recip a == one / a || a == zero
-- > recip a * a == one || a == zero
-- > a * recip a == one || a == zero
class
  (Distributive a, Subtractive a, Divisive a) =>
  Field a

instance Field P.Double

instance Field P.Float

instance Field b => Field (a -> b)

-- | A hyperbolic field class
--
-- > sqrt . (**2) == id
-- > log . exp == id
-- > for +ive b, a != 0,1: a ** logBase a b ≈ b
class
  (Field a) =>
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
  logBase f f' a = logBase (f a) (f' a)
  f ** f' = \a -> f a ** f' a
  sqrt f = sqrt . f

-- | Conversion from a 'Field' to a 'Ring'
--
-- See [Field of fractions](https://en.wikipedia.org/wiki/Field_of_fractions)
--
-- > a - one < floor a <= a <= ceiling a < a + one
-- > round a == floor (a + half)
class (Field a, Multiplicative b, Additive b) => QuotientField a b where
  properFraction :: a -> (b, a)

  round :: a -> b
  default round :: (P.Ord a, P.Ord b, Subtractive b, Integral b) => a -> b
  round x = case properFraction x of
    (n, r) ->
      let m = bool (n + one) (n - one) (r P.< zero)
          half_down = abs' r - (one / (one + one))
          abs' a
            | a P.< zero = negate a
            | P.otherwise = a
       in case P.compare half_down zero of
            P.LT -> n
            P.EQ -> bool m n (even n)
            P.GT -> m

  ceiling :: a -> b
  default ceiling :: (P.Ord a) => a -> b
  ceiling x = bool n (n + one) (r P.>= zero)
    where
      (n, r) = properFraction x

  floor :: a -> b
  default floor :: (P.Ord a, Subtractive b) => a -> b
  floor x = bool n (n - one) (r P.< zero)
    where
      (n, r) = properFraction x

  truncate :: a -> b
  default truncate :: (P.Ord a) => a -> b
  truncate x = bool (ceiling x) (floor x) (x P.> zero)

instance QuotientField P.Float P.Integer where
  properFraction = P.properFraction

instance QuotientField P.Double P.Integer where
  properFraction = P.properFraction

instance QuotientField P.Float P.Int where
  properFraction = P.properFraction

instance QuotientField P.Double P.Int where
  properFraction = P.properFraction

instance QuotientField b c => QuotientField (a -> b) (a -> c) where
  properFraction f = (P.fst . frac, P.snd . frac)
    where
      frac a = properFraction @b @c (f a)

  round f = round . f

  ceiling f = ceiling . f

  floor f = floor . f

  truncate f = truncate . f

-- | A bounded field introduces the concepts of infinity and NaN.
--
-- > one / zero + infinity == infinity
-- > infinity + a == infinity
-- > zero / zero != nan
--
-- Note the tricky law that, although nan is assigned to zero/zero, they are never-the-less not equal. A committee decided this.
class
  (Field a) =>
  UpperBoundedField a where
  infinity :: a
  infinity = one / zero

  nan :: a
  nan = zero / zero

instance UpperBoundedField P.Float

instance UpperBoundedField P.Double

instance UpperBoundedField b => UpperBoundedField (a -> b) where
  infinity _ = infinity
  nan _ = nan

-- | Negative infinity.
class
  (Subtractive a, Field a) =>
  LowerBoundedField a where
  negInfinity :: a
  negInfinity = negate (one / zero)

instance LowerBoundedField P.Float

instance LowerBoundedField P.Double

instance LowerBoundedField b => LowerBoundedField (a -> b) where
  negInfinity _ = negInfinity

-- | Trigonometric Field
class
  (Field a) =>
  TrigField a where
  pi :: a
  sin :: a -> a
  cos :: a -> a
  tan :: a -> a
  tan x = sin x / cos x
  asin :: a -> a
  acos :: a -> a
  atan :: a -> a
  atan2 :: a -> a -> a
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
  atan2 = P.atan2
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
  atan2 = P.atan2
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
  atan2 f g x = atan2 (f x) (g x)
  sinh f = sinh . f
  cosh f = cosh . f
  asinh f = asinh . f
  acosh f = acosh . f
  atanh f = atanh . f

-- | A 'half' is a 'Field' because it requires addition, multiplication and division.
half :: (Field a) => a
half = one / two
