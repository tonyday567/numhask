{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Metric classes
module NumHask.Algebra.Metric
  ( Basis (..),
    Absolute,
    Sign,
    HomoBased,
    abs,
    signum,
    distance,
    Direction (..),
    Polar (..),
    polar,
    coord,
    Epsilon (..),
    (~=),
    Euclid (..),
    EuclidPair (..),
  )
where

import Data.Bool
import Data.Type.Equality
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import GHC.Natural (Natural (..))
import NumHask.Algebra.Additive
import NumHask.Algebra.Multiplicative
import Prelude hiding
  ( Bounded (..),
    Integral (..),
    negate,
    (*),
    (-),
    (+),
    (/),
    recip,
    abs,
    signum,
    sqrt,
    sin,
    cos,
    atan,
    atan2
  )
import qualified Prelude as P
import NumHask.Algebra.Module
import NumHask.Algebra.Field
import NumHask.Algebra.Lattice
import Control.Applicative

-- $setup
--
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude

-- | 'Basis' encapsulates the intuitive notions of magnitude (the reduction of a higher-kinded number to a scalar one intuitive appeal) and the basis of which the magnitude reduction occurs. An instance needs to satisfy these laws:
--
-- > \a -> magnitude a >= zero
-- > \a -> magnitude zero == zero
-- > \a -> a == magnitude a .* basis a
-- > \a -> magnitude (basis a) == one
--
-- The names chosen are meant to represent the spiritual idea of a basis rather than a specific mathematics. See https://en.wikipedia.org/wiki/Basis_(linear_algebra) & https://en.wikipedia.org/wiki/Norm_(mathematics) for some mathematical motivations.
--
--
-- >>> magnitude (-0.5 :: Double)
-- 0.5
--
-- >>> basis (-0.5 :: Double)
-- -1.0
class (Multiplicative (Mag a), Additive (Mag a)) => Basis a where
  type Mag a :: Type
  type Base a :: Type

  -- | or length, or ||v||
  magnitude :: a -> Mag a

  -- | or direction, or v-hat
  basis :: a -> Base a

-- | Basis where the domain and magnitude codomain are the same.
type Absolute a = (Basis a, Mag a ~ a)

-- | Basis where the domain and basis codomain are the same.
type Sign a = (Basis a, Base a ~ a)

-- | Basis where the domain, magnitude codomain and basis codomain are the same.
type HomoBased a = (Basis a, Mag a ~ a, Base a ~ a)

-- | The absolute value of a number.
--
-- prop> \a -> abs a * signum a ~= a
--
-- abs zero == zero, so any value for sign zero is ok.  We choose lawful neutral:
--
-- >>> abs (-1)
-- 1
abs :: (Absolute a) => a -> a
abs = magnitude

-- | The sign of a number.
--
-- >>> signum zero == zero
-- True
--
-- >>> signum (-1)
-- -1
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
-- > distance a b .* basis (a - b) == a - b
distance :: (Basis a, Subtractive a) => a -> a -> Mag a
distance a b = magnitude (a - b)

-- | Convert between a "co-ordinated" or "higher-kinded" number and representations of a direction. Typically thought of as polar co-ordinate conversion.
--
-- See [Polar coordinate system](https://en.wikipedia.org/wiki/Polar_coordinate_system)
--
-- > ray . angle == basis
-- > magnitude (ray x) == one
class (Additive coord, Multiplicative coord, Additive (Dir coord), Multiplicative (Dir coord)) => Direction coord where
  type Dir coord :: Type
  angle :: coord -> Dir coord
  ray :: Dir coord -> coord

-- | Something that has a magnitude and a direction.
data Polar a = Polar { radial :: a, azimuth :: a}
  deriving (Generic, Show, Eq)

instance (Additive a, Multiplicative a) => Basis (Polar a) where
  type Mag (Polar a) = a
  type Base (Polar a) = a
  magnitude = radial
  basis = azimuth

-- | Convert a higher-kinded number that has direction, to a 'Polar'
polar :: (Dir (Base a) ~ Mag a, Basis a, Direction (Base a)) => a -> Polar (Mag a)
polar x = Polar (magnitude x) (angle (basis x))

-- | Convert a Polar to a (higher-kinded) number that has a direction.
coord :: (Scalar m ~ Dir m, MultiplicativeAction m, Direction m) => Polar (Scalar m) -> m
coord x = radial x .* ray (azimuth x)

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

data Euclid a = Euclid a a deriving (Generic, Generic1, Eq, Show)

instance Functor Euclid where
  fmap f (Euclid x y) = Euclid (f x) (f y)

instance Applicative Euclid where
  pure x = Euclid x x
  Euclid fx fy <*> Euclid x y = Euclid (fx x) (fy y)
  liftA2 f (Euclid x y) (Euclid x' y') = Euclid (f x x') (f y y')

instance (Additive a) => Additive (Euclid a) where
  (+) = liftA2 (+)
  zero = pure zero

instance (Subtractive a) => Subtractive (Euclid a) where
  negate = fmap negate

instance
  (Multiplicative a) =>
  Multiplicative (Euclid a)
  where
   (*) = liftA2 (*)
   one = pure one

instance
  (Subtractive a, Divisive a) =>
  Divisive (Euclid a)
  where
  recip = fmap recip

instance (TrigField a) => Direction (Euclid a) where
  type Dir (Euclid a) = a
  angle (Euclid x y) = atan2 y x
  ray x = Euclid (cos x) (sin x)

instance
  (ExpField a, Eq a) =>
  Basis (Euclid a)
  where
    type Mag (Euclid a) = a
    type Base (Euclid a) = Euclid a

    magnitude (Euclid x y) = sqrt (x * x + y * y)
    basis p = let m = magnitude p in bool (p /. m) zero (m == zero)

instance
  (Ord a, Basis a, Epsilon a, Subtractive a) =>
  Epsilon (Euclid a)
  where
    epsilon = pure epsilon
    nearZero (Euclid x y) = x <= epsilon && y <= epsilon

instance (JoinSemiLattice a) => JoinSemiLattice (Euclid a) where
  (\/) (Euclid x y) (Euclid x' y') = Euclid (x \/ x') (y \/ y')

instance (MeetSemiLattice a) => MeetSemiLattice (Euclid a) where
  (/\) (Euclid x y) (Euclid x' y') = Euclid (x /\ x') (y /\ y')

instance (BoundedJoinSemiLattice a) => BoundedJoinSemiLattice (Euclid a) where
  bottom = pure bottom

instance (BoundedMeetSemiLattice a) => BoundedMeetSemiLattice (Euclid a) where
  top = pure top

instance (Multiplicative a) => MultiplicativeAction (Euclid a) where
  type Scalar (Euclid a) = a
  (.*) s (Euclid x y) = Euclid (s*x) (s*y)

instance (Divisive a) => DivisiveAction (Euclid a) where
  (./) s = fmap (s/)

-- FIXME: performance versus Euclid
newtype EuclidPair a = EuclidPair { euclidPair :: (a,a) }
  deriving stock
  ( Generic,
    Eq,
    Show)

instance Functor EuclidPair where
  fmap f (EuclidPair (x,y)) = EuclidPair (f x, f y)

instance Applicative EuclidPair where
  pure x = EuclidPair (x,x)
  EuclidPair (fx,fy) <*> EuclidPair (x,y) = EuclidPair (fx x, fy y)
  liftA2 f (EuclidPair (x,y)) (EuclidPair (x',y')) = EuclidPair (f x x', f y y')

instance (Additive a) => Additive (EuclidPair a) where
  (+) = liftA2 (+)
  zero = pure zero

instance (Subtractive a) => Subtractive (EuclidPair a) where
  negate = fmap negate

instance
  (Multiplicative a) =>
  Multiplicative (EuclidPair a)
  where
   (*) = liftA2 (*)
   one = pure one

instance
  (Subtractive a, Divisive a) =>
  Divisive (EuclidPair a)
  where
  recip = fmap recip

instance (TrigField a) => Direction (EuclidPair a) where
  type Dir (EuclidPair a) = a
  angle (EuclidPair (x,y)) = atan2 y x
  ray x = EuclidPair (cos x, sin x)

instance
  (ExpField a, Eq a) =>
  Basis (EuclidPair a)
  where
    type Mag (EuclidPair a) = a
    type Base (EuclidPair a) = EuclidPair a

    magnitude (EuclidPair (x,y)) = sqrt (x * x + y * y)
    basis p = let m = magnitude p in bool (p /. m) zero (m == zero)

instance
  (Ord a, Basis a, Epsilon a, Subtractive a) =>
  Epsilon (EuclidPair a)
  where
    epsilon = pure epsilon
    nearZero (EuclidPair (x,y)) = x <= epsilon && y <= epsilon

instance (JoinSemiLattice a) => JoinSemiLattice (EuclidPair a) where
  (\/) (EuclidPair (x,y)) (EuclidPair (x',y')) = EuclidPair (x \/ x', y \/ y')

instance (MeetSemiLattice a) => MeetSemiLattice (EuclidPair a) where
  (/\) (EuclidPair (x,y)) (EuclidPair (x',y')) = EuclidPair (x /\ x', y /\ y')

instance (BoundedJoinSemiLattice a) => BoundedJoinSemiLattice (EuclidPair a) where
  bottom = pure bottom

instance (BoundedMeetSemiLattice a) => BoundedMeetSemiLattice (EuclidPair a) where
  top = pure top

instance (Multiplicative a) => MultiplicativeAction (EuclidPair a) where
  type Scalar (EuclidPair a) = a
  (.*) s (EuclidPair (x,y)) = EuclidPair (s*x, s*y)

instance (Divisive a) => DivisiveAction (EuclidPair a) where
  (./) s = fmap (s/)
