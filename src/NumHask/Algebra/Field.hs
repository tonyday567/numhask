{-# OPTIONS_GHC -Wall #-}

-- | Field
module NumHask.Algebra.Field (
    Field
  , ExpField(..)
  , QuotientField(..)
  , BoundedField(..)
  , infinity
  , neginfinity
  ) where

import Protolude (Double, Float, Integer, Bool, (||))
import qualified Protolude as P
import NumHask.Algebra.Additive
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Distribution
import NumHask.Algebra.Ring
import Data.Complex (Complex(..))

-- | Field
class ( AdditiveGroup a
      , MultiplicativeGroup a
      , Distribution a
      , Ring a) =>
      Field a

instance Field Double
instance Field Float
instance {-# Overlapping #-} (Field a) => Field (Complex a)

-- | ExpField
class (Field a) => ExpField a where
    exp :: a -> a
    log :: a -> a

    logBase :: a -> a -> a
    logBase a b = log b / log a

    (**) :: a -> a -> a
    (**) a b = exp (log a * b)

    sqrt :: a -> a
    sqrt a = a**(one/(one+one))

instance ExpField Double where
    exp = P.exp
    log = P.log
    (**) = (P.**)

instance ExpField Float where
    exp = P.exp
    log = P.log
    (**) = (P.**)

instance {-# Overlapping #-} (ExpField a) => ExpField (Complex a) where
    exp (rx :+ ix) = exp rx * cos ix :+ exp rx * sin ix
      where
        cos = P.undefined
        sin = P.undefined

    log (rx :+ ix) = log (sqrt (rx * rx + ix * ix)) :+ atan2 ix rx
      where
        atan2 = P.undefined

-- | quotient fields explode constraints if they are polymorphed to emit general integrals
class (Field a) => QuotientField a where
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
class (Field a) => BoundedField a where
    maxBound :: a
    maxBound = one/zero

    minBound :: a
    minBound = negate (one/zero)

    nan :: a
    nan = zero/zero

    isNaN :: a -> Bool

-- | prints as `Infinity`
infinity :: BoundedField a => a
infinity = maxBound

-- | prints as `-Infinity`
neginfinity :: BoundedField a => a
neginfinity = minBound

instance BoundedField Float where isNaN = P.isNaN
instance BoundedField Double where isNaN = P.isNaN
instance {-# Overlapping #-} (BoundedField a) => BoundedField (Complex a) where
    isNaN (rx :+ ix) = isNaN rx || isNaN ix

