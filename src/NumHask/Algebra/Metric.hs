{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Metric classes
module NumHask.Algebra.Metric
  ( Signed (..),
    Norm (..),
    distance,
    Direction (..),
    Polar (..),
    polar,
    coord,
    Epsilon (..),
    (~=),
  )
where

import Data.Bool (bool)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import GHC.Natural (Natural (..))
import NumHask.Algebra.Additive (Additive (zero), Subtractive (..), (-))
import NumHask.Algebra.Module (MultiplicativeAction (..))
import NumHask.Algebra.Multiplicative (Multiplicative (one))
import Prelude hiding
  ( Bounded (..),
    Integral (..),
    negate,
    (*),
    (-),
  )
import qualified Prelude as P

-- $setup
--
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude

-- | 'signum' from base is not an operator name in numhask and is replaced by 'sign'.  Compare with 'Norm' where there is a change in codomain.
--
-- prop> \a -> abs a * sign a ~= a
--
-- abs zero == zero, so any value for sign zero is ok.  We choose lawful neutral:
--
-- >>> sign zero == zero
-- True
--
-- >>> abs (-1)
-- 1
--
-- >>> sign (-1)
-- -1
class
  (Additive a, Multiplicative a) =>
  Signed a
  where
  sign :: a -> a
  abs :: a -> a

instance Signed Double where
  sign a =
    case compare a zero of
      EQ -> zero
      GT -> one
      LT -> negate one
  abs = P.abs

instance Signed Float where
  sign a =
    case compare a zero of
      EQ -> zero
      GT -> one
      LT -> negate one
  abs = P.abs

instance Signed Int where
  sign a =
    case compare a zero of
      EQ -> zero
      GT -> one
      LT -> negate one
  abs = P.abs

instance Signed Integer where
  sign a =
    case compare a zero of
      EQ -> zero
      GT -> one
      LT -> negate one
  abs = P.abs

instance Signed Natural where
  sign a =
    case compare a zero of
      EQ -> zero
      GT -> one
      LT -> negate one
  abs = id

instance Signed Int8 where
  sign a =
    case compare a zero of
      EQ -> zero
      GT -> one
      LT -> negate one
  abs = P.abs

instance Signed Int16 where
  sign a =
    case compare a zero of
      EQ -> zero
      GT -> one
      LT -> negate one
  abs = P.abs

instance Signed Int32 where
  sign a =
    case compare a zero of
      EQ -> zero
      GT -> one
      LT -> negate one
  abs = P.abs

instance Signed Int64 where
  sign a =
    case compare a zero of
      EQ -> zero
      GT -> one
      LT -> negate one
  abs = P.abs

instance Signed Word where
  sign a = bool one zero (a == zero)
  abs = P.abs

instance Signed Word8 where
  sign a = bool one zero (a == zero)
  abs = P.abs

instance Signed Word16 where
  sign a = bool one zero (a == zero)
  abs = P.abs

instance Signed Word32 where
  sign a = bool one zero (a == zero)
  abs = P.abs

instance Signed Word64 where
  sign a = bool one zero (a == zero)
  abs = P.abs

-- | Norm is a slight generalisation of Signed. The class has the same shape but allows the codomain to be different to the domain.
--
-- > \a -> norm a >= zero
-- > \a -> norm zero == zero
-- > \a -> a == norm a .* basis a
-- > \a -> norm (basis a) == one
--
-- >>> norm (-0.5 :: Double)
-- 0.5
--
-- >>> basis (-0.5 :: Double)
-- -1.0
class (Additive a, Multiplicative (Normed a), Additive (Normed a)) => Norm a where
  type Normed a :: Type

  -- | or length, or ||v||
  norm :: a -> Normed a

  -- | or direction, or v-hat
  basis :: a -> a

instance Norm Double where
  type Normed Double = Double
  norm = P.abs
  basis = P.signum

instance Norm Float where
  type Normed Float = Float
  norm = P.abs
  basis = P.signum

instance Norm Int where
  type Normed Int = Int
  norm = P.abs
  basis = P.signum

instance Norm Integer where
  type Normed Integer = Integer
  norm = P.abs
  basis = P.signum

instance Norm Natural where
  type Normed Natural = Natural
  norm = P.abs
  basis = P.signum

instance Norm Int8 where
  type Normed Int8 = Int8
  norm = P.abs
  basis = P.signum

instance Norm Int16 where
  type Normed Int16 = Int16
  norm = P.abs
  basis = P.signum

instance Norm Int32 where
  type Normed Int32 = Int32
  norm = P.abs
  basis = P.signum

instance Norm Int64 where
  type Normed Int64 = Int64
  norm = P.abs
  basis = P.signum

instance Norm Word where
  type Normed Word = Word
  norm = P.abs
  basis = P.signum

instance Norm Word8 where
  type Normed Word8 = Word8
  norm = P.abs
  basis = P.signum

instance Norm Word16 where
  type Normed Word16 = Word16
  norm = P.abs
  basis = P.signum

instance Norm Word32 where
  type Normed Word32 = Word32
  norm = P.abs
  basis = P.signum

instance Norm Word64 where
  type Normed Word64 = Word64
  norm = P.abs
  basis = P.signum

-- | Distance, which combines the Subtractive notion of difference, with Norm.
--
-- > distance a b >= zero
-- > distance a a == zero
-- > distance a b .* basis (a - b) == a - b
distance :: (Norm a, Subtractive a) => a -> a -> Normed a
distance a b = norm (a - b)

-- | Convert between a "co-ordinated" or "higher-kinded" number and representations of an angle. Typically thought of as polar co-ordinate conversion.
--
-- See [Polar coordinate system](https://en.wikipedia.org/wiki/Polar_coordinate_system)
--
-- > ray . angle == basis
-- > norm (ray x) == one
class (Additive coord, Multiplicative coord, Additive (Dir coord), Multiplicative (Dir coord)) => Direction coord where
  type Dir coord :: Type
  angle :: coord -> Dir coord
  ray :: Dir coord -> coord

-- | Something that has a magnitude and a direction.
data Polar a = Polar {magnitude :: !(Normed a), direction :: !(Dir a)}
  deriving (Generic)

-- | Convert from a number to a Polar.
polar :: (Norm coord, Direction coord) => coord -> Polar coord
polar z = Polar (norm z) (angle z)

-- | Convert from a Polar to a (coordinated aka higher-kinded) number.
coord :: (Normed coord ~ Scalar coord, MultiplicativeAction coord, Direction coord) => Polar coord -> coord
coord (Polar m d) = m .* ray d

-- | A small number, especially useful for approximate equality.
class
  (Eq a, Additive a) =>
  Epsilon a
  where
  epsilon :: a
  epsilon = zero

  -- | are we near enough?
  --
  -- >>> nearZero (epsilon :: Double)
  -- True
  nearZero :: a -> Bool
  default nearZero :: (Ord a, Subtractive a) => a -> Bool
  nearZero a = epsilon >= a && epsilon >= negate a

  -- | Approximate equality
  --
  -- >>> aboutEqual zero (epsilon :: Double)
  -- True
  aboutEqual :: a -> a -> Bool
  default aboutEqual :: (Subtractive a) => a -> a -> Bool
  aboutEqual a b = nearZero $ a - b

infixl 4 ~=

-- | About equal operator.
--
-- >>> (1.0 + epsilon) ~= (1.0 :: Double)
-- True
(~=) :: (Epsilon a) => a -> a -> Bool
(~=) = aboutEqual

-- | 1e-14
instance Epsilon Double where
  epsilon = 1e-14

-- | 1e-6
instance Epsilon Float where
  epsilon = 1e-6

-- | 0
instance Epsilon Int

instance Epsilon Integer

instance Epsilon Int8

instance Epsilon Int16

instance Epsilon Int32

instance Epsilon Int64

instance Epsilon Word

instance Epsilon Word8

instance Epsilon Word16

instance Epsilon Word32

instance Epsilon Word64
