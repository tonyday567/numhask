{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | [field](https://en.wikipedia.org/wiki/Field_(mathematics\)) classes
module NumHask.Algebra.Field
  ( SemiField,
    Field,
    ExpField (..),
    QuotientField (..),
    Quotient (..),
    toQuotient,
    fromQuotient,
    mulQuotient,
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
import NumHask.Data.Integral (DivMod (..), FromIntegral (..), Integral, even)
import Prelude (Eq (..), Show, (.))
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

-- | Whole-and-fraction pair. 'properFraction' is the normalizer (Prelude
-- toward-zero). The constructor is raw; the range is not enforced.
data Quotient w a = Quotient
  { whole :: w,
    fraction :: a
  }
  deriving (Eq, Show)

-- | Quotienting of a 'Field' into a 'NumHask.Algebra.Ring'
--
-- See [Field of fractions](https://en.wikipedia.org/wiki/Field_of_fractions)
--
-- > \a -> a - one < floor a <= a <= ceiling a < a + one
--
-- >>> properFraction (1.5 :: Double)
-- Quotient {whole = 1, fraction = 0.5}
--
-- >>> fromQuotient (properFraction (1.5 :: Double))
-- 1.5
class (SemiField a) => QuotientField a where
  type Whole a :: Type

  -- | Split into a whole part and a fractional tail in the canonical range
  -- @0 <= fraction < one@.
  --
  -- The floor decomposition, not Prelude's toward-zero split: the fraction
  -- is never negative, so @properFraction (-1.5) =
  -- Quotient {whole = -2, fraction = 0.5}@.  Deliberate departure from
  -- Prelude — the floor basis keeps the sign in 'whole', a clean
  -- change-of-basis to compute with, instead of compounding sign into the
  -- fraction.
  --
  -- >>> properFraction (-1.5 :: Double)
  -- Quotient {whole = -2, fraction = 0.5}
  properFraction :: a -> Quotient (Whole a) a

  -- | Embed the whole-number type back into the field.
  --
  -- The recombination inverse of 'properFraction'.  Scalar fields default to
  -- 'fromIntegral'; compound fields override it (e.g. @Whole (Complex a) =
  -- Complex (Whole a)@ lifts component-wise).
  fromWhole :: Whole a -> a
  default fromWhole :: (FromIntegral a (Whole a)) => Whole a -> a
  fromWhole = fromIntegral

  -- | round to the nearest Int
  --
  -- Exact ties are managed by rounding down ties if the whole component is even.
  --
  -- >>> round (1.5 :: Double)
  -- 2
  --
  -- >>> round (2.5 :: Double)
  -- 2
  round :: a -> Whole a
  default round :: (P.Ord a, P.Eq (Whole a), Integral (Whole a)) => a -> Whole a
  round x = case properFraction x of
    Quotient n f ->
      case P.compare f half of
        P.LT -> n
        P.EQ -> bool (n + one) n (even n)
        P.GT -> n + one

  -- | supply the next upper whole component
  --
  -- >>> ceiling (1.001 :: Double)
  -- 2
  ceiling :: a -> Whole a
  default ceiling :: (P.Ord a, Distributive (Whole a)) => a -> Whole a
  ceiling x = bool n (n + one) (r P.> zero)
    where
      Quotient n r = properFraction x

  -- | supply the previous lower whole component
  --
  -- >>> floor (1.001 :: Double)
  -- 1
  floor :: a -> Whole a
  floor x = whole (properFraction x)

  -- | supply the whole component closest to zero
  --
  -- >>> floor (-1.001 :: Double)
  -- -2
  --
  -- >>> truncate (-1.001 :: Double)
  -- -1
  truncate :: a -> Whole a
  default truncate :: (P.Ord a) => a -> Whole a
  truncate x = bool (ceiling x) (floor x) (x P.> zero)

-- | 'properFraction' as the named normalizer into 'Quotient'.
--
-- The canonical range is the Prelude toward-zero split: @fraction ∈ (-1, 1)@
-- with the sign of the input, and @fromQuotient (toQuotient a) == a@.
toQuotient :: (QuotientField a) => a -> Quotient (Whole a) a
toQuotient = properFraction

-- | Recombine a 'Quotient'. Law: @fromQuotient (toQuotient a) == a@.
fromQuotient :: (QuotientField a) => Quotient (Whole a) a -> a
fromQuotient (Quotient w f) = fromWhole w + f

-- | Bilinear multiplication of quotients: recombine, multiply, re-split.
--
-- @(w1 + f1) * (w2 + f2)@ expands to the four terms
-- @w1*w2 + w1*f2 + f1*w2 + f1*f2@, and 'toQuotient' pushes any overflow back
-- into 'whole'.  The same bilinear-then-normalize pattern as complex
-- multiplication.
--
-- >>> mulQuotient (toQuotient (0.5 :: Double)) (toQuotient (3.0 :: Double))
-- Quotient {whole = 1, fraction = 0.5}
--
-- >>> mulQuotient (toQuotient (1.5 :: Double)) (toQuotient (2.0 :: Double)) == toQuotient (1.5 * 2.0 :: Double)
-- True
mulQuotient ::
  (QuotientField a) =>
  Quotient (Whole a) a ->
  Quotient (Whole a) a ->
  Quotient (Whole a) a
mulQuotient q1 q2 = toQuotient (fromQuotient q1 * fromQuotient q2)

instance QuotientField P.Float where
  type Whole P.Float = P.Int
  properFraction a = Quotient n (a - P.fromIntegral n)
    where
      n = P.floor a

instance QuotientField P.Double where
  type Whole P.Double = P.Int
  properFraction a = Quotient n (a - P.fromIntegral n)
    where
      n = P.floor a

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
modF :: (Eq a, Field a, FromIntegral a (Whole a), QuotientField a) => a -> a -> a
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
divF :: (Eq a, Field a, FromIntegral a (Whole a), QuotientField a) => a -> a -> a
divF n d
  | d == infinity = zero
  | d == zero = infinity
  | P.True = fromIntegral (floor (n / d))

-- | Approximate `NumHask.Algebra.Field.divMod` for fields.
--
-- @since 0.13
--
-- >>> divModF 1.5 1.2
-- DivMod {div_ = 1.0, mod_ = 0.30000000000000004}
divModF :: (Eq a, Field a, FromIntegral a (Whole a), QuotientField a) => a -> a -> DivMod a
divModF n d
  | d == infinity = DivMod zero n
  | d == zero = DivMod infinity nan
  | P.True = DivMod div' (n - d * div')
  where
    div' = fromIntegral (floor (n / d))
