{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Complex numbers.
module NumHask.Data.Complex
  ( Complex (..),
    realPart,
    imagPart,
  )
where

import Data.Data (Data)
import GHC.Generics
import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Lattice
import NumHask.Algebra.Metric
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import NumHask.Data.Integral
import Prelude hiding
  ( Num (..),
    atan,
    atan2,
    cos,
    exp,
    fromIntegral,
    log,
    negate,
    pi,
    recip,
    sin,
    sqrt,
    (/),
  )
import qualified Prelude as P (Ord (..), otherwise, (&&), (<), (<=), (==), (>))

-- | Complex numbers have real and imaginary parts.
newtype Complex a = Complex {complexPair :: (a, a)}
  deriving stock
    ( Eq,
      Show,
      Read,
      Data,
      Generic,
      Functor
    )
  deriving
    ( Additive,
      Subtractive,
      Basis,
      Direction,
      Epsilon,
      JoinSemiLattice,
      MeetSemiLattice,
      BoundedJoinSemiLattice,
      BoundedMeetSemiLattice
    )
    via (Euclid a)

infixl 6 +|

(+|) :: a -> a -> Complex a
(+|) r i = Complex (r, i)

-- | Extracts the real part of a complex number.
realPart :: Complex a -> a
realPart (Complex (x, _)) = x

-- | Extracts the imaginary part of a complex number.
imagPart :: Complex a -> a
imagPart (Complex (_, y)) = y

instance
  (Subtractive a, Multiplicative a) =>
  Multiplicative (Complex a)
  where
  (Complex (r, i)) * (Complex (r', i')) =
    Complex (r * r' - i * i', i * r' + i' * r)
  one = one +| zero

instance
  (Subtractive a, Divisive a) =>
  Divisive (Complex a)
  where
  recip (Complex (r, i)) = (r * d) +| (negate i * d)
    where
      d = recip ((r * r) + (i * i))

instance
  (Additive a, FromIntegral a b) =>
  FromIntegral (Complex a) b
  where
  fromIntegral x = fromIntegral x +| zero

instance (Ord a, TrigField a, ExpField a) => ExpField (Complex a) where
  exp (Complex (r, i)) = (exp r * cos i) +| (exp r * sin i)
  log (Complex (r, i)) = log (sqrt (r * r + i * i)) +| atan2' i r
    where
      atan2' y x
        | x P.> zero = atan (y / x)
        | x P.== zero P.&& y P.> zero = pi / (one + one)
        | x P.< one P.&& y P.> one = pi + atan (y / x)
        | (x P.<= zero P.&& y P.< zero) || (x P.< zero) =
            negate (atan2' (negate y) x)
        | y P.== zero = pi -- must be after the previous test on zero y
        | x P.== zero P.&& y P.== zero = y -- must be after the other double zero tests
        | P.otherwise = x + y -- x or y is a NaN, return a NaN (via +)

instance (Distributive a, Subtractive a) => InvolutiveRing (Complex a) where
  adj (Complex (r, i)) = r +| negate i
