{-# OPTIONS_GHC -Wall #-}

-- | A magma heirarchy for multiplication. The basic magma structure is repeated and prefixed with 'Multiplicative-'.
module NumHask.Algebra.Multiplicative
  ( MultiplicativeMagma(..)
  , MultiplicativeUnital(..)
  , MultiplicativeAssociative
  , MultiplicativeCommutative
  , MultiplicativeInvertible(..)
  , product
  , Multiplicative(..)
  , MultiplicativeRightCancellative(..)
  , MultiplicativeLeftCancellative(..)
  , MultiplicativeGroup(..)
  ) where

import Data.Complex (Complex(..))
import GHC.Natural (Natural(..))
import NumHask.Algebra.Additive
import qualified Prelude as P
import Prelude (Bool(..), Double, Float, Int, Integer)

-- | 'times' is used as the operator for the multiplicative magam to distinguish from '*' which, by convention, implies commutativity
--
-- > ∀ a,b ∈ A: a `times` b ∈ A
--
-- law is true by construction in Haskell
class MultiplicativeMagma a where
  times :: a -> a -> a

instance MultiplicativeMagma Double where
  times = (P.*)

instance MultiplicativeMagma Float where
  times = (P.*)

instance MultiplicativeMagma Int where
  times = (P.*)

instance MultiplicativeMagma Integer where
  times = (P.*)

instance MultiplicativeMagma Bool where
  times = (P.&&)

instance (MultiplicativeMagma a, AdditiveGroup a) =>
         MultiplicativeMagma (Complex a) where
  (rx :+ ix) `times` (ry :+ iy) =
    (rx `times` ry - ix `times` iy) :+ (ix `times` ry + iy `times` rx)

instance MultiplicativeMagma Natural where
  times = (P.*)

-- | Unital magma for multiplication.
--
-- > one `times` a == a
-- > a `times` one == a
class MultiplicativeMagma a =>
      MultiplicativeUnital a where
  one :: a

instance MultiplicativeUnital Double where
  one = 1

instance MultiplicativeUnital Float where
  one = 1

instance MultiplicativeUnital Int where
  one = 1

instance MultiplicativeUnital Integer where
  one = 1

instance MultiplicativeUnital Bool where
  one = True

instance (AdditiveUnital a, AdditiveGroup a, MultiplicativeUnital a) =>
         MultiplicativeUnital (Complex a) where
  one = one :+ zero

instance MultiplicativeUnital Natural where
  one = 1

-- | Associative magma for multiplication.
--
-- > (a `times` b) `times` c == a `times` (b `times` c)
class MultiplicativeMagma a =>
      MultiplicativeAssociative a

instance MultiplicativeAssociative Double

instance MultiplicativeAssociative Float

instance MultiplicativeAssociative Int

instance MultiplicativeAssociative Integer

instance MultiplicativeAssociative Bool

instance (AdditiveGroup a, MultiplicativeAssociative a) =>
         MultiplicativeAssociative (Complex a)

instance MultiplicativeAssociative Natural

-- | Commutative magma for multiplication.
--
-- > a `times` b == b `times` a
class MultiplicativeMagma a =>
      MultiplicativeCommutative a

instance MultiplicativeCommutative Double

instance MultiplicativeCommutative Float

instance MultiplicativeCommutative Int

instance MultiplicativeCommutative Integer

instance MultiplicativeCommutative Bool

instance (AdditiveGroup a, MultiplicativeCommutative a) =>
         MultiplicativeCommutative (Complex a)

instance MultiplicativeCommutative Natural

-- | Invertible magma for multiplication.
--
-- > ∀ a ∈ A: recip a ∈ A
--
-- law is true by construction in Haskell
class MultiplicativeMagma a =>
      MultiplicativeInvertible a where
  recip :: a -> a

instance MultiplicativeInvertible Double where
  recip = P.recip

instance MultiplicativeInvertible Float where
  recip = P.recip

instance (AdditiveGroup a, MultiplicativeInvertible a) =>
         MultiplicativeInvertible (Complex a) where
  recip (rx :+ ix) = (rx `times` d) :+ (negate ix `times` d)
    where
      d = recip ((rx `times` rx) `plus` (ix `times` ix))

-- | Idempotent magma for multiplication.
--
-- > a `times` a == a
class MultiplicativeMagma a =>
      MultiplicativeIdempotent a

instance MultiplicativeIdempotent Bool

-- | product definition avoiding a clash with the Product monoid in base
--
product :: (Multiplicative a, P.Foldable f) => f a -> a
product = P.foldr (*) one

-- | Multiplicative is commutative, associative and unital under multiplication
--
-- > one * a == a
-- > a * one == a
-- > (a * b) * c == a * (b * c)
-- > a * b == b * a
class ( MultiplicativeCommutative a
      , MultiplicativeUnital a
      , MultiplicativeAssociative a
      ) =>
      Multiplicative a where
  infixl 7 *
  (*) :: a -> a -> a
  a * b = times a b

instance Multiplicative Double

instance Multiplicative Float

instance Multiplicative Int

instance Multiplicative Integer

instance Multiplicative Bool

instance (AdditiveGroup a, Multiplicative a) => Multiplicative (Complex a)

instance Multiplicative Natural

-- | Non-commutative left divide
--
-- > recip a `times` a = one
class ( MultiplicativeUnital a
      , MultiplicativeAssociative a
      , MultiplicativeInvertible a
      ) =>
      MultiplicativeLeftCancellative a where
  infixl 7 ~/
  (~/) :: a -> a -> a
  a ~/ b = recip b `times` a

-- | Non-commutative right divide
--
-- > a `times` recip a = one
class ( MultiplicativeUnital a
      , MultiplicativeAssociative a
      , MultiplicativeInvertible a
      ) =>
      MultiplicativeRightCancellative a where
  infixl 7 /~
  (/~) :: a -> a -> a
  a /~ b = a `times` recip b

-- | Divide ('/') is reserved for where both the left and right cancellative laws hold.  This then implies that the MultiplicativeGroup is also Abelian.
--
-- > a / a = one
-- > recip a = one / a
-- > recip a * a = one
-- > a * recip a = one
class (Multiplicative a, MultiplicativeInvertible a) =>
      MultiplicativeGroup a where
  infixl 7 /
  (/) :: a -> a -> a
  (/) a b = a `times` recip b

instance MultiplicativeGroup Double

instance MultiplicativeGroup Float

instance (AdditiveGroup a, MultiplicativeGroup a) =>
         MultiplicativeGroup (Complex a)
