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
    logBase,
    sqrt,
    QuotientField (..),
    round,
    ceiling,
    floor,
    truncate,
    infinity,
    negInfinity,
    nan,
    TrigField (..),
    half,
  )
where

import Data.Bool (bool)
import NumHask.Algebra.Additive (Additive (..), Subtractive (..), (-))
import NumHask.Algebra.Multiplicative
  ( Divisive (..),
    Multiplicative (..),
    (/),
  )
import NumHask.Algebra.Ring (Distributive, two)
import NumHask.Data.Integral (Integral, even)
import Prelude ((.))
import qualified Prelude as P

-- $setup
--
-- >>> :set -XRebindableSyntax
-- >>> :set -XFlexibleContexts
-- >>> :set -XScopedTypeVariables
-- >>> import NumHask.Prelude

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
  ExpField a
  where
  exp :: a -> a
  log :: a -> a
  (**) :: a -> a -> a
  (**) a b = exp (log a * b)

-- | log to the base of
--
-- >>> logBase 2 8
-- 2.9999999999999996
logBase :: (ExpField a) => a -> a -> a
logBase a b = log b / log a

-- | square root
--
-- >>> sqrt 4
-- 2.0
sqrt :: (ExpField a) => a -> a
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

-- | Conversion from a 'Field' to a 'NumHask.Algebra.Ring'
--
-- See [Field of fractions](https://en.wikipedia.org/wiki/Field_of_fractions)
--
-- > a - one < floor a <= a <= ceiling a < a + one
-- > round a == floor (a + half)
class (Field a, Multiplicative b, Additive b) => QuotientField a b where
  properFraction :: a -> (b, a)

-- | round to the nearest integral
--
-- Exact ties are managed by rounding down ties if the whole component is even.
--
-- >>> round (1.5 :: Double) :: Int
-- 2
--
-- >>> round (2.5 :: Double) :: Int
-- 2
round :: (P.Ord a, P.Ord b, QuotientField a b, Subtractive b, Integral b) => a -> b
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

-- | supply the next upper whole component
--
-- >>> ceiling (1.001 :: Double) :: Int
-- 2
ceiling :: (P.Ord a, QuotientField a b) => a -> b
ceiling x = bool n (n + one) (r P.>= zero)
  where
    (n, r) = properFraction x

-- | supply the previous lower whole component
--
-- >>> floor (1.001 :: Double) :: Int
-- 1
floor :: (P.Ord a, QuotientField a b, Subtractive b) => a -> b
floor x = bool n (n - one) (r P.< zero)
  where
    (n, r) = properFraction x

-- | supply the whole component closest to zero
--
-- >>> floor (-1.001 :: Double) :: Int
-- -2
--
-- >>> truncate (-1.001 :: Double) :: Int
-- -1
truncate :: (P.Ord a, QuotientField a b, Subtractive b) => a -> b
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

-- | A field introduces the concept of infinity.
--
-- > one / zero + infinity == infinity
-- > infinity + a == infinity
-- > zero / zero != nan
--
-- Note the tricky law that, although nan is assigned to zero/zero, they are never-the-less not equal. A committee decided this.
infinity :: (Field a) => a
infinity = one / zero

-- |
--
-- Note the law:
-- -- > zero / zero != nan
nan :: (Field a) => a
nan = zero / zero

-- | negative infinity
negInfinity :: (Field a) => a
negInfinity = negate infinity

-- | Trigonometric Field
class
  (Field a) =>
  TrigField a
  where
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
