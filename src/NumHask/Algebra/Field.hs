{-# OPTIONS_GHC -Wall #-}

-- | Field
module NumHask.Algebra.Field
  ( Field
  , ExpField(..)
  , QuotientField(..)
  , BoundedField(..)
  , infinity
  , neginfinity
  , TrigField(..)
  ) where

import Data.Complex (Complex(..))
import NumHask.Algebra.Additive
import NumHask.Algebra.Distribution
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import Protolude (Bool, Double, Float, Integer, (||))
import qualified Protolude as P

-- | Field
class (AdditiveGroup a, MultiplicativeGroup a, Distribution a, Ring a) =>
      Field a

instance Field Double

instance Field Float

instance (Field a) => Field (Complex a)

-- | ExpField
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

instance (TrigField a, ExpField a) => ExpField (Complex a) where
  exp (rx :+ ix) = exp rx * cos ix :+ exp rx * sin ix
  log (rx :+ ix) = log (sqrt (rx * rx + ix * ix)) :+ atan2 ix rx

-- | quotient fields explode constraints if they are polymorphed to emit general integrals
class (Field a) =>
      QuotientField a where
  round :: a -> Integer
  ceiling :: a -> Integer
  floor :: a -> Integer
  (^^) :: a -> Integer -> a

instance QuotientField Float where
  round = P.round
  ceiling = P.ceiling
  floor = P.floor
  (^^) = (P.^^)

instance QuotientField Double where
  round = P.round
  ceiling = P.ceiling
  floor = P.floor
  (^^) = (P.^^)

-- | providing the concepts of infinity and NaN, thus moving away from error throwing
class (Field a) =>
      BoundedField a where
  maxBound :: a
  maxBound = one / zero
  minBound :: a
  minBound = negate (one / zero)
  nan :: a
  nan = zero / zero
  isNaN :: a -> Bool

-- | prints as `Infinity`
infinity :: BoundedField a => a
infinity = maxBound

-- | prints as `-Infinity`
neginfinity :: BoundedField a => a
neginfinity = minBound

instance BoundedField Float where
  isNaN = P.isNaN

instance BoundedField Double where
  isNaN = P.isNaN

instance (BoundedField a) => BoundedField (Complex a) where
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
