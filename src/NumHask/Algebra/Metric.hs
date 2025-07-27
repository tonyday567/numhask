{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Metric classes
module NumHask.Algebra.Metric
  ( Basis (..),
    Absolute,
    Sign,
    EndoBased,
    abs,
    signum,
    distance,
    Direction (..),
    Polar (..),
    polar,
    coord,
    Epsilon (..),
    nearZero,
    aboutEqual,
    (~=),
    EuclideanPair (..),
  )
where

import Control.Applicative
import Data.Bool
import Data.Data
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind
import Data.Type.Equality
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import GHC.Natural (Natural (..))
import NumHask.Algebra.Action
import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Lattice
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import Prelude (Double, Eq (..), Float, Functor (..), Int, Integer, Ord (..), Show, Word, fromRational)
import Prelude qualified as P

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude

-- | 'Basis' encapsulates the notion of magnitude (intuitively the quotienting of a higher-kinded number to a scalar one) and the basis on which the magnitude quotienting was performed. An instance needs to satisfy these laws:
--
-- @since 0.11
--
-- > \a -> magnitude a >= zero
-- > \a -> magnitude zero == zero
-- > \a -> a == magnitude a *| basis a
-- > \a -> magnitude (basis a) == one
--
-- The names chosen are meant to represent the spiritual idea of a basis rather than a specific mathematics. See https://en.wikipedia.org/wiki/Basis_(linear_algebra) & https://en.wikipedia.org/wiki/Norm_(mathematics) for some mathematical motivations.
--
-- >>> magnitude (-0.5 :: Double)
-- 0.5
--
-- >>> basis (-0.5 :: Double)
-- -1.0
class (Distributive (Mag a)) => Basis a where
  type Mag a :: Type
  type Base a :: Type

  -- | or length, or ||v||
  magnitude :: a -> Mag a

  -- | or direction, or v-hat
  basis :: a -> Base a

-- | Basis where the domain and magnitude codomain are the same.
--
-- @since 0.11
type Absolute a = (Basis a, Mag a ~ a)

-- | Basis where the domain and basis codomain are the same.
--
-- @since 0.11
type Sign a = (Basis a, Base a ~ a)

-- | Basis where the domain, magnitude codomain and basis codomain are the same.
--
-- @since 0.11
type EndoBased a = (Basis a, Mag a ~ a, Base a ~ a)

-- | The absolute value of a number.
--
-- prop> \a -> abs a * signum a ~= a
--
--
-- >>> abs (-1)
-- 1
abs :: (Absolute a) => a -> a
abs = magnitude

-- | The sign of a number.
--
-- @since 0.11
--
-- >>> signum (-1)
-- -1
--
-- @abs zero == zero@, so any value for @signum zero@ is ok.  We choose lawful neutral:
--
-- >>> signum zero == zero
-- True
signum :: (Sign a) => a -> a
signum = basis

instance Basis Double where
  type Mag Double = Double
  type Base Double = Double
  magnitude = P.abs
  basis = P.signum

instance Basis Float where
  type Mag Float = Float
  type Base Float = Float
  magnitude = P.abs
  basis = P.signum

instance Basis Int where
  type Mag Int = Int
  type Base Int = Int
  magnitude = P.abs
  basis = P.signum

instance Basis Integer where
  type Mag Integer = Integer
  type Base Integer = Integer
  magnitude = P.abs
  basis = P.signum

instance Basis Natural where
  type Mag Natural = Natural
  type Base Natural = Natural
  magnitude = P.abs
  basis = P.signum

instance Basis Int8 where
  type Mag Int8 = Int8
  type Base Int8 = Int8
  magnitude = P.abs
  basis = P.signum

instance Basis Int16 where
  type Mag Int16 = Int16
  type Base Int16 = Int16
  magnitude = P.abs
  basis = P.signum

instance Basis Int32 where
  type Mag Int32 = Int32
  type Base Int32 = Int32
  magnitude = P.abs
  basis = P.signum

instance Basis Int64 where
  type Mag Int64 = Int64
  type Base Int64 = Int64
  magnitude = P.abs
  basis = P.signum

instance Basis Word where
  type Mag Word = Word
  type Base Word = Word
  magnitude = P.abs
  basis = P.signum

instance Basis Word8 where
  type Mag Word8 = Word8
  type Base Word8 = Word8
  magnitude = P.abs
  basis = P.signum

instance Basis Word16 where
  type Mag Word16 = Word16
  type Base Word16 = Word16
  magnitude = P.abs
  basis = P.signum

instance Basis Word32 where
  type Mag Word32 = Word32
  type Base Word32 = Word32
  magnitude = P.abs
  basis = P.signum

instance Basis Word64 where
  type Mag Word64 = Word64
  type Base Word64 = Word64
  magnitude = P.abs
  basis = P.signum

-- | Distance, which combines the Subtractive notion of difference, with Basis.
--
-- > distance a b >= zero
-- > distance a a == zero
-- > distance a b *| basis (a - b) == a - b
distance :: (Basis a, Subtractive a) => a -> a -> Mag a
distance a b = magnitude (a - b)

-- | Convert between a "co-ordinated" or "higher-kinded" number and a direction.
--
-- @since 0.7
--
-- > ray . angle == basis
-- > magnitude (ray x) == one
class (Distributive coord, Distributive (Dir coord)) => Direction coord where
  type Dir coord :: Type
  angle :: coord -> Dir coord
  ray :: Dir coord -> coord

-- | Something that has a magnitude and a direction, with both expressed as the same type.
--
-- @since 0.7
--
-- See [Polar coordinate system](https://en.wikipedia.org/wiki/Polar_coordinate_system)
data Polar a = Polar {radial :: a, azimuth :: a}
  deriving (Eq, Show, Generic, Data)

instance (Additive a, Multiplicative a) => Basis (Polar a) where
  type Mag (Polar a) = a
  type Base (Polar a) = a
  magnitude = radial
  basis = azimuth

-- | Convert a higher-kinded number that has direction, to a 'Polar'
--
-- @since 0.7
polar :: (Dir (Base a) ~ Mag a, Basis a, Direction (Base a)) => a -> Polar (Mag a)
polar x = Polar (magnitude x) (angle (basis x))

-- | Convert a Polar to a (higher-kinded) number that has a direction.
--
-- @since 0.07
coord :: (Scalar m ~ Dir m, MultiplicativeAction m, Direction m) => Polar (Scalar m) -> m
coord x = radial x *| ray (azimuth x)

-- | A small number, especially useful for approximate equality.
class
  (Eq a, Additive a) =>
  Epsilon a
  where
  epsilon :: a
  epsilon = zero

-- | Note that the constraint is Lattice rather than Ord allowing broader usage.
--
-- >>> nearZero (epsilon :: Double)
-- True
--
-- >>> nearZero (epsilon :: EuclideanPair Double)
-- True
nearZero :: (Epsilon a, Lattice a, Subtractive a) => a -> Bool
nearZero a = epsilon /\ a == epsilon && epsilon /\ negate a == epsilon

-- | Approximate equality
--
-- >>> aboutEqual zero (epsilon :: Double)
-- True
aboutEqual :: (Epsilon a, Lattice a, Subtractive a) => a -> a -> Bool
aboutEqual a b = nearZero (a - b)

infixl 4 ~=

-- | About equal operator.
--
-- >>> (1.0 + epsilon) ~= (1.0 :: Double)
-- True
(~=) :: (Epsilon a) => (Lattice a, Subtractive a) => a -> a -> Bool
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

-- | Two dimensional cartesian coordinates.
--
-- @since 0.11
newtype EuclideanPair a = EuclideanPair {euclidPair :: (a, a)}
  deriving stock
    (Eq, Show, Generic, Data)

instance Functor EuclideanPair where
  fmap f (EuclideanPair (x, y)) = EuclideanPair (f x, f y)

instance Applicative EuclideanPair where
  pure x = EuclideanPair (x, x)
  EuclideanPair (fx, fy) <*> EuclideanPair (x, y) = EuclideanPair (fx x, fy y)
  liftA2 f (EuclideanPair (x, y)) (EuclideanPair (x', y')) = EuclideanPair (f x x', f y y')

instance (Additive a) => Additive (EuclideanPair a) where
  (+) = liftA2 (+)
  zero = pure zero

instance (Subtractive a) => Subtractive (EuclideanPair a) where
  negate = fmap negate

instance
  (Multiplicative a) =>
  Multiplicative (EuclideanPair a)
  where
  (*) = liftA2 (*)
  one = pure one

instance
  (Subtractive a, Divisive a) =>
  Divisive (EuclideanPair a)
  where
  recip = fmap recip

instance (TrigField a) => Direction (EuclideanPair a) where
  type Dir (EuclideanPair a) = a
  angle (EuclideanPair (x, y)) = atan2 y x
  ray x = EuclideanPair (cos x, sin x)

instance
  (ExpField a, Eq a) =>
  Basis (EuclideanPair a)
  where
  type Mag (EuclideanPair a) = a
  type Base (EuclideanPair a) = EuclideanPair a

  magnitude (EuclideanPair (x, y)) = sqrt (x * x + y * y)
  basis p = let m = magnitude p in bool (p |/ m) zero (m == zero)

instance
  (Epsilon a) =>
  Epsilon (EuclideanPair a)
  where
  epsilon = pure epsilon

instance (JoinSemiLattice a) => JoinSemiLattice (EuclideanPair a) where
  (\/) (EuclideanPair (x, y)) (EuclideanPair (x', y')) = EuclideanPair (x \/ x', y \/ y')

instance (MeetSemiLattice a) => MeetSemiLattice (EuclideanPair a) where
  (/\) (EuclideanPair (x, y)) (EuclideanPair (x', y')) = EuclideanPair (x /\ x', y /\ y')

instance (LowerBounded a) => LowerBounded (EuclideanPair a) where
  bottom = pure bottom

instance (UpperBounded a) => UpperBounded (EuclideanPair a) where
  top = pure top

instance (Multiplicative a) => MultiplicativeAction (EuclideanPair a) where
  type Scalar (EuclideanPair a) = a
  (|*) (EuclideanPair (x, y)) s = EuclideanPair (s * x, s * y)

instance (Divisive a) => DivisiveAction (EuclideanPair a) where
  (|/) e s = fmap (/ s) e

instance (Ord a, TrigField a, ExpField a) => ExpField (EuclideanPair a) where
  exp (EuclideanPair (x, y)) = EuclideanPair (exp x * cos y, exp x * sin y)
  log (EuclideanPair (x, y)) = EuclideanPair (log (sqrt (x * x + y * y)), atan2 y x)

instance (QuotientField a, Subtractive a) => QuotientField (EuclideanPair a) where
  type Whole (EuclideanPair a) = EuclideanPair (Whole a)

  properFraction (EuclideanPair (x, y)) =
    (EuclideanPair (xwhole, ywhole), EuclideanPair (xfrac, yfrac))
    where
      (xwhole, xfrac) = properFraction x
      (ywhole, yfrac) = properFraction y

  round (EuclideanPair (x, y)) = EuclideanPair (round x, round y)
  ceiling (EuclideanPair (x, y)) = EuclideanPair (ceiling x, ceiling y)
  floor (EuclideanPair (x, y)) = EuclideanPair (floor x, floor y)
  truncate (EuclideanPair (x, y)) = EuclideanPair (truncate x, truncate y)
