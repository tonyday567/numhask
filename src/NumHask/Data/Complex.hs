{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Complex numbers.
module NumHask.Data.Complex
  ( Complex (..),
    (+:),
    realPart,
    imagPart,
    normSquared,
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
    ceiling,
    cos,
    exp,
    floor,
    fromIntegral,
    log,
    negate,
    pi,
    properFraction,
    recip,
    round,
    sin,
    sqrt,
    truncate,
    (/),
  )

-- $setup
--
-- >>> import NumHask.Prelude
-- >>> :m -Prelude

-- | The underlying representation is a newtype-wrapped tuple, compared with the base datatype. This was chosen to facilitate the use of DerivingVia.
newtype Complex a = Complex {complexPair :: (a, a)}
  deriving stock
    ( Eq,
      Show,
      Read,
      Generic,
      Data,
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
      LowerBounded,
      UpperBounded,
      ExpField
    )
    via (EuclideanPair a)

infixl 6 +:

-- | Complex number constructor.
--
-- Internally, Complex derives most instances via EuclideanPair. For instance,
--
-- >>> sqrt (1.0 +: (-1.0)) :: Complex Double
-- Complex {complexPair = (1.0986841134678098,-0.45508986056222733)}
--
-- >>> sqrt ((-1.0) +: 0.0) :: Complex Double
-- Complex {complexPair = (6.123233995736766e-17,1.0)}
(+:) :: a -> a -> Complex a
(+:) r i = Complex (r, i)

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
  one = one +: zero

instance
  (Subtractive a, Divisive a) =>
  Divisive (Complex a)
  where
  recip (Complex (r, i)) = (r * d) +: (negate i * d)
    where
      d = recip ((r * r) + (i * i))

instance
  (Additive a, FromIntegral a b) =>
  FromIntegral (Complex a) b
  where
  fromIntegral x = fromIntegral x +: zero

instance (Distributive a, Subtractive a) => InvolutiveRing (Complex a) where
  adj (Complex (r, i)) = r +: negate i

-- Can't use DerivingVia due to extra Whole constraints
instance (Subtractive a, QuotientField a) => QuotientField (Complex a) where
  type Whole (Complex a) = Complex (Whole a)

  properFraction (Complex (x, y)) =
    (Complex (xwhole, ywhole), Complex (xfrac, yfrac))
    where
      (xwhole, xfrac) = properFraction x
      (ywhole, yfrac) = properFraction y

  round (Complex (x, y)) = Complex (round x, round y)
  ceiling (Complex (x, y)) = Complex (ceiling x, ceiling y)
  floor (Complex (x, y)) = Complex (floor x, floor y)
  truncate (Complex (x, y)) = Complex (truncate x, truncate y)

-- | The squared norm: frequently useful, and doesn't require the
-- ability to take square roots.
normSquared :: Distributive a => Complex a -> a
normSquared (Complex (x, y)) = x*x + y*y
