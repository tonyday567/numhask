{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wall #-}

-- | Field classes
module NumHask.Algebra.Field
  ( Field,
    ExpField (..),
    QuotientField (..),
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
-- >>> :set -XScopedTypeVariables
-- >>> import NumHask.Prelude

-- | A <https://en.wikipedia.org/wiki/Field_(mathematics) Field> is a set
--   on which addition, subtraction, multiplication, and division are defined. It is also assumed that multiplication is distributive over addition.
--
-- A summary of the rules inherited from super-classes of Field:
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

instance (Field b) => Field (a -> b)

-- | A hyperbolic field class
--
-- prop> \a -> a < zero || (sqrt . (**2)) a == a
-- prop> \a -> a < zero || (log . exp) a ~= a
-- prop> \a b -> (b < zero) || a <= zero || a == 1 || abs (a ** logBase a b - b) < 10 * epsilon
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
  logBase :: a -> a -> a
  logBase a b = log b / log a

  -- | square root
  --
  -- >>> sqrt 4
  -- 2.0
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

instance (ExpField b) => ExpField (a -> b) where
  exp f = exp . f
  log f = log . f

-- | Conversion from a 'Field' to a 'NumHask.Algebra.Ring'
--
-- See [Field of fractions](https://en.wikipedia.org/wiki/Field_of_fractions)
--
-- > \a -> a - one < floor a <= a <= ceiling a < a + one
-- prop> (\a -> a - one < fromIntegral (floor a :: Int) && fromIntegral (floor a :: Int) <= a && a <= fromIntegral (ceiling a :: Int) && fromIntegral (ceiling a :: Int) <= a + one) :: Double -> Bool
-- prop> \a -> (round a :: Int) ~= (floor (a + half) :: Int)
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

  -- | supply the next upper whole component
  --
  -- >>> ceiling (1.001 :: Double) :: Int
  -- 2
  ceiling :: a -> b
  default ceiling :: (P.Ord a) => a -> b
  ceiling x = bool n (n + one) (r P.>= zero)
    where
      (n, r) = properFraction x

  -- | supply the previous lower whole component
  --
  -- >>> floor (1.001 :: Double) :: Int
  -- 1
  floor :: a -> b
  default floor :: (P.Ord a, Subtractive b) => a -> b
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

instance (QuotientField b c) => QuotientField (a -> b) (a -> c) where
  properFraction f = (P.fst . frac, P.snd . frac)
    where
      frac a = properFraction @b @c (f a)
  round f = round . f
  ceiling f = ceiling . f
  floor f = floor . f
  truncate f = truncate . f

-- | infinity is defined for any 'Field'.
--
-- >>> one / zero + infinity
-- Infinity
--
-- >>> infinity + 1
-- Infinity
infinity :: (Field a) => a
infinity = one / zero

-- | nan is defined as zero/zero
--
-- but note the (social) law:
--
-- >>> nan == zero / zero
-- False
nan :: (Field a) => a
nan = zero / zero

-- | negative infinity
--
-- >>> negInfinity + infinity
-- NaN
negInfinity :: (Field a) => a
negInfinity = negate infinity

-- | Trigonometric Field
--
-- The list of laws is quite long: <https://en.wikipedia.org/wiki/List_of_trigonometric_identities trigonometric identities>
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

instance (TrigField b) => TrigField (a -> b) where
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
--
-- >>> half :: Double
-- 0.5
half :: (Field a) => a
half = one / two
