{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | [field](https://en.wikipedia.org/wiki/Field_(mathematics\)) classes
module NumHask.Algebra.Field
  ( SemiField,
    Field,
    ExpField (..),
    QuotientField (..),
    infinity,
    negInfinity,
    nan,
    TrigField (..),
    half,
    modF,
    divF,
    divModF,
  )
where

import Data.Bool (bool)
import Data.Kind
import NumHask.Algebra.Additive (Additive (..), Subtractive (..), (-))
import NumHask.Algebra.Multiplicative
  ( Divisive (..),
    Multiplicative (..),
    (/),
  )
import NumHask.Algebra.Ring (Distributive, Ring, two)
import NumHask.Data.Integral (FromIntegral (..), Integral, even)
import Prelude (Eq (..), (.))
import Prelude qualified as P

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> :set -XScopedTypeVariables
-- >>> import NumHask.Prelude

-- | A <https://en.wikipedia.org/wiki/Semifield Semifield> is a field with no subtraction.
--
-- @since 0.12
type SemiField a = (Distributive a, Divisive a)

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
type Field a = (Ring a, Divisive a)

-- | A hyperbolic field class
--
-- prop> \(a::Double) -> a < zero || (sqrt . (**2)) a == a
-- prop> \(a::Double) -> a < zero || (log . exp) a ~= a
-- prop> \(a::Double) (b::Double) -> (b < zero) || a <= zero || a == 1 || abs (a ** logBase a b - b) < 10 * epsilon
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

-- | Quotienting of a 'Field' into a 'NumHask.Algebra.Ring'
--
-- See [Field of fractions](https://en.wikipedia.org/wiki/Field_of_fractions)
--
-- > \a -> a - one < floor a <= a <= ceiling a < a + one
class (SemiField a) => QuotientField a whole where
  properFraction :: forall a whole. a -> (whole, a)

  -- | round to the nearest Integral
  --
  -- Exact ties are managed by rounding down ties if the whole component is even.
  --
  -- >>> round (1.5 :: Double)
  -- 2
  --
  -- >>> round (2.5 :: Double)
  -- 2
  round :: a -> w
  default round :: (Subtractive a, Integral w, P.Eq w, P.Ord a, Subtractive w) => a -> w
  round x = case properFraction x of
    (n, r) ->
      let m = bool (n + one) (n - one) (r P.< zero)
          half_up = abs' r + half
          abs' a
            | a P.< zero = negate a
            | P.otherwise = a
       in case P.compare half_up one of
            P.LT -> n
            P.EQ -> bool m n (even n)
            P.GT -> m

  -- | supply the next upper whole component
  --
  -- >>> ceiling (1.001 :: Double)
  -- 2
  ceiling :: a -> w
  default ceiling :: (P.Ord a, Distributive w) => a -> w
  ceiling x = bool n (n + one) (r P.> zero)
    where
      (n, r) = properFraction x

  -- | supply the previous lower whole component
  --
  -- >>> floor (1.001 :: Double)
  -- 1
  floor :: a -> w
  default floor :: (P.Ord a, Subtractive w, Distributive w) => a -> w
  floor x = bool n (n - one) (r P.< zero)
    where
      (n, r) = properFraction x
  -- | supply the whole component closest to zero
  --
  -- >>> floor (-1.001 :: Double)
  -- -2
  --
  -- >>> truncate (-1.001 :: Double)
  -- -1
  truncate :: a -> w
  default truncate :: (P.Ord a, Distributive w, Subtractive w) => a -> w
  truncate x = bool (ceiling x) (floor x) (x P.> zero)

instance QuotientField P.Float P.Int where
  properFraction = P.properFraction

instance QuotientField P.Double P.Int where
  properFraction = P.properFraction

-- | infinity is defined for any 'Field'.
--
-- >>> one / zero + infinity
-- Infinity
--
-- >>> infinity + 1
-- Infinity
infinity :: (SemiField a) => a
infinity = one / zero

-- | nan is defined as zero/zero
--
-- but note the (social) law:
--
-- >>> nan == zero / zero
-- False
nan :: (SemiField a) => a
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
  asinh = P.asinh
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
  asinh = P.asinh
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

-- | A half of 'one'
--
-- >>> half :: Double
-- 0.5
half :: (Additive a, Divisive a) => a
half = one / two

-- | Approximate modulo for fields
--
-- @since 0.13
--
-- >>> modF 1.5 1.2
-- 0.30000000000000004
modF :: (Eq a, Field a, FromIntegral a w, QuotientField a w) => a -> a -> a
modF n d
  | d == infinity = n
  | d == zero = nan
  | P.True = n - d * fromIntegral (floor (n / d))

-- | Approximate diviso for fields.
--
-- Compared with 'NumHask.Algebra.Field.div', divF returns the original type rather than the 'Whole' type.
--
-- @since 0.13
--
-- >>> divF 1.5 1.2
-- 1.0
divF :: (Eq a, Field a, FromIntegral a w, QuotientField a w) => a -> a -> a
divF n d
  | d == infinity = zero
  | d == zero = infinity
  | P.True = fromIntegral (floor (n / d))

-- | Approximate `NumHask.Algebra.Field.divMod` for fields.
--
-- @since 0.13
--
-- >>> divModF 1.5 1.2
-- (1.0,0.30000000000000004)
divModF :: (Eq a, Field a, FromIntegral a w, QuotientField a w) => a -> a -> (a,a)
divModF n d
  | d == infinity = (zero, n)
  | d == zero = (infinity, nan)
  | P.True = (div', n - d * div')
  where
    div' = fromIntegral (floor (n / d))
