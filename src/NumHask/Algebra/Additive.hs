{-# OPTIONS_GHC -Wall #-}

-- | Additive Structure

module NumHask.Algebra.Additive (
    -- ** Additive Structure
    AdditiveMagma(..)
  , AdditiveUnital(..)
  , AdditiveAssociative
  , AdditiveCommutative
  , AdditiveInvertible(..)
  , AdditiveIdempotent
  , AdditiveMonoidal
  , Additive(..)
  , AdditiveRightCancellative(..)
  , AdditiveLeftCancellative(..)
  , AdditiveGroup(..)
  ) where

import qualified Protolude as P
import Protolude (Double, Float, Int, Integer, Bool(..))
import Data.Complex (Complex(..))

-- * Additive structure
-- The Magma structures are repeated for an additive and multiplicative heirarchy, mostly so we can name the specific operators in the usual ways.
--
-- | 'plus' is used for the additive magma to distinguish from '+' which, by convention, implies commutativity
class AdditiveMagma a where plus :: a -> a -> a

instance AdditiveMagma Double where plus = (P.+)
instance AdditiveMagma Float where plus = (P.+)
instance AdditiveMagma Int where plus = (P.+)
instance AdditiveMagma Integer where plus = (P.+)
instance AdditiveMagma Bool where plus = (P.||)
instance (AdditiveMagma a) => AdditiveMagma (Complex a) where
    (rx :+ ix) `plus` (ry :+ iy) = (rx `plus` ry) :+ (ix `plus` iy)

-- | AdditiveUnital
--
-- > zero `plus` a == a
-- > a `plus` zero == a
class AdditiveMagma a => AdditiveUnital a where zero :: a

instance AdditiveUnital Double where zero = 0
instance AdditiveUnital Float where zero = 0
instance AdditiveUnital Int where zero = 0
instance AdditiveUnital Integer where zero = 0
instance AdditiveUnital Bool where zero = False
instance (AdditiveUnital a) => AdditiveUnital (Complex a) where
    zero = zero :+ zero

-- | AdditiveAssociative
--
-- > (a `plus` b) `plus` c == a `plus` (b `plus` c)
class AdditiveMagma a => AdditiveAssociative a

instance AdditiveAssociative Double
instance AdditiveAssociative Float
instance AdditiveAssociative Int
instance AdditiveAssociative Integer
instance AdditiveAssociative Bool
instance (AdditiveAssociative a) => AdditiveAssociative (Complex a)

-- | AdditiveCommutative
--
-- > a `plus` b == b `plus` a
class AdditiveMagma a => AdditiveCommutative a

instance AdditiveCommutative Double
instance AdditiveCommutative Float
instance AdditiveCommutative Int
instance AdditiveCommutative Integer
instance AdditiveCommutative Bool
instance (AdditiveCommutative a) => AdditiveCommutative (Complex a)

-- | AdditiveInvertible
--
-- > ∀ a ∈ A: negate a ∈ A
--
-- law is true by construction in Haskell
class AdditiveMagma a => AdditiveInvertible a where negate :: a -> a

instance AdditiveInvertible Double where negate = P.negate
instance AdditiveInvertible Float where negate = P.negate
instance AdditiveInvertible Int where negate = P.negate
instance AdditiveInvertible Integer where negate = P.negate
instance AdditiveInvertible Bool where negate = P.not
instance (AdditiveInvertible a) => AdditiveInvertible (Complex a) where
    negate (rx :+ ix) = negate rx :+ negate ix

-- | AdditiveIdempotent
--
-- > a `plus` a == a
class AdditiveMagma a => AdditiveIdempotent a

instance AdditiveIdempotent Bool

-- | AdditiveMonoidal
class ( AdditiveUnital a
      , AdditiveAssociative a) =>
      AdditiveMonoidal a

instance AdditiveMonoidal Double
instance AdditiveMonoidal Float
instance AdditiveMonoidal Int
instance AdditiveMonoidal Integer
instance AdditiveMonoidal Bool
instance (AdditiveMonoidal a) => AdditiveMonoidal (Complex a)

-- | Additive is commutative, unital and associative under addition
--
-- > a + b = b + a
--
-- > (a + b) + c = a + (b + c)
--
-- > zero + a = a
--
-- > a + zero = a
--
class ( AdditiveCommutative a
      , AdditiveUnital a
      , AdditiveAssociative a) =>
      Additive a where
    infixl 6 +
    (+) :: a -> a -> a
    a + b = plus a b

instance Additive Double
instance Additive Float
instance Additive Int
instance Additive Integer
instance Additive Bool
instance {-# Overlapping #-} (Additive a) => Additive (Complex a)

-- | Non-commutative left minus
class ( AdditiveUnital a
      , AdditiveAssociative a
      , AdditiveInvertible a) =>
      AdditiveLeftCancellative a where
    infixl 6 ~-
    (~-) :: a -> a -> a
    (~-) a b = negate b `plus` a

-- | Non-commutative right minus
class ( AdditiveUnital a
      , AdditiveAssociative a
      , AdditiveInvertible a) =>
      AdditiveRightCancellative a where
    infixl 6 -~
    (-~) :: a -> a -> a
    (-~) a b = a `plus` negate b

-- | AdditiveGroup
--
-- > a - a = zero
--
-- > negate a = zero - a
--
-- > negate a + a = zero
--
class ( Additive a
      , AdditiveInvertible a) =>
      AdditiveGroup a where
    infixl 6 -
    (-) :: a -> a -> a
    (-) a b = a `plus` negate b

instance AdditiveGroup Double
instance AdditiveGroup Float
instance AdditiveGroup Int
instance AdditiveGroup Integer
instance {-# Overlapping #-} (AdditiveGroup a) => AdditiveGroup (Complex a)
