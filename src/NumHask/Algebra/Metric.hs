{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
#if defined(__GLASGOW_HASKELL__)
{-# LANGUAGE TypeFamilies #-}
#endif
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
#if defined(__GLASGOW_HASKELL__)
import GHC.Natural (Natural (..))
#endif
#if defined(__MHS__)
import Numeric.Natural (Natural (..))
#endif
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
-- >>> :set -Wno-deprecated-flags
-- >>> :m -Prelude
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
class Basis basis mag a | a -> mag basis where

  -- | or length, or ||v||
  magnitude :: a -> mag

  -- | or direction, or v-hat
  basis :: a -> basis

-- | Basis where the domain and magnitude codomain are the same.
--
-- @since 0.11
type Absolute basis a = Basis basis a a

-- | Basis where the domain and basis codomain are the same.
--
-- @since 0.11
type Sign mag a = Basis a mag a

-- | Basis where the domain, magnitude codomain and basis codomain are the same.
--
-- @since 0.11
type EndoBased a = Basis a a a

-- | The absolute value of a number.
--
-- >> \a -> abs a * signum a ~= a
--
--
-- >>> abs (-1)
-- 1
abs :: (EndoBased a) => a -> a
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
signum :: (EndoBased a) => a -> a
signum = basis

instance Basis Double Double Double where
  magnitude = P.abs
  basis = P.signum

instance Basis Float Float Float where
  magnitude = P.abs
  basis = P.signum

instance Basis Int Int Int where
  magnitude = P.abs
  basis = P.signum

instance Basis Integer Integer Integer where
  magnitude = P.abs
  basis = P.signum

instance Basis Natural Natural Natural where
  magnitude = P.abs
  basis = P.signum

instance Basis Int8 Int8 Int8 where
  magnitude = P.abs
  basis = P.signum

instance Basis Int16 Int16 Int16 where
  magnitude = P.abs
  basis = P.signum

instance Basis Int32 Int32 Int32 where
  magnitude = P.abs
  basis = P.signum

instance Basis Int64 Int64 Int64 where
  magnitude = P.abs
  basis = P.signum

instance Basis Word Word Word where
  magnitude = P.abs
  basis = P.signum

instance Basis Word8 Word8 Word8 where
  magnitude = P.abs
  basis = P.signum

instance Basis Word16 Word16 Word16 where
  magnitude = P.abs
  basis = P.signum

instance Basis Word32 Word32 Word32 where
  magnitude = P.abs
  basis = P.signum

instance Basis Word64 Word64 Word64 where
  magnitude = P.abs
  basis = P.signum

-- | Distance, which combines the Subtractive notion of difference, with Basis.
--
-- > distance a b >= zero
-- > distance a a == zero
-- > distance a b *| basis (a - b) == a - b
distance :: (Basis b mag a, Subtractive a) => a -> a -> mag
distance a b = magnitude (a - b)

-- | Convert between a "co-ordinated" or "higher-kinded" number and a direction.
--
-- @since 0.7
--
-- > ray . angle == basis
-- > magnitude (ray x) == one
class (Distributive coord, Distributive dir) => Direction coord dir where
  angle :: coord -> dir
  ray :: dir -> coord

-- | Something that has a magnitude and a direction, with both expressed as the same type.
--
-- @since 0.7
--
-- See [Polar coordinate system](https://en.wikipedia.org/wiki/Polar_coordinate_system)
data Polar a = Polar {radial :: a, azimuth :: a}
  deriving stock (Eq, Show)
#if defined(__GLASGOW_HASKELL__)
  deriving stock (Generic, Data)
#endif

instance (Additive a, Multiplicative a) => Basis a a (Polar a) where
  magnitude = radial
  basis = azimuth

-- | Convert a higher-kinded number that has direction, to a 'Polar'
--
-- @since 0.7
polar :: (Basis b a a, Direction b a) => a -> Polar a
polar x = Polar (magnitude x) (angle (basis x))

-- | Convert a Polar to a (higher-kinded) number that has a direction.
--
-- @since 0.07
#if defined(__GLASGOW_HASKELL__)
coord :: (MultiplicativeAction c, Scalar c ~ a, Direction c a) => Polar a -> c
#endif
#if defined(__MHS__)
coord :: (MultiplicativeAction c a, Direction c a) => Polar a -> c
#endif
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
  deriving stock (Eq, Show)
#if defined(__GLASGOW_HASKELL__)
  deriving stock (Generic, Data)
#endif

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

instance (TrigField a) => Direction (EuclideanPair a) a where
  angle (EuclideanPair (x, y)) = atan2 y x
  ray x = EuclideanPair (cos x, sin x)

instance
#if defined(__GLASGOW_HASKELL__)
  (ExpField a, Eq a, DivisiveAction (EuclideanPair a), Scalar (EuclideanPair a) ~ a) =>
#endif
#if defined(__MHS__)
  (ExpField a, Eq a, DivisiveAction (EuclideanPair a) a) =>
#endif
  Basis (EuclideanPair a) a (EuclideanPair a)
  where
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

instance (Ord a, TrigField a, ExpField a) => ExpField (EuclideanPair a) where
  exp (EuclideanPair (x, y)) = EuclideanPair (exp x * cos y, exp x * sin y)
  log (EuclideanPair (x, y)) = EuclideanPair (log (sqrt (x * x + y * y)), atan2 y x)

