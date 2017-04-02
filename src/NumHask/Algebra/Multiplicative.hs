{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Multiplicate structure
-- Many treatments of a numeric tower treat multiplication differently to addition.  NumHask treats these two as exactly symmetrical, and thus departs from the usual mathematical terminology.

module NumHask.Algebra.Multiplicative (
   -- ** Multiplicative Structure
    MultiplicativeMagma(..)
  , MultiplicativeUnital(..)
  , MultiplicativeAssociative
  , MultiplicativeCommutative
  , MultiplicativeInvertible(..)
  , MultiplicativeHomomorphic(..)
  , MultiplicativeMonoidal
  , Multiplicative(..)
  , MultiplicativeRightCancellative(..)
  , MultiplicativeLeftCancellative(..)
  , MultiplicativeGroup(..)
  ) where

import qualified Protolude as P
import Protolude (Double, Float, Int, Integer, Bool(..))
import Data.Functor.Rep

-- * Multiplicative structure
-- | 'times' is used for the multiplicative magma to distinguish from '*' which, by convention, implies commutativity
class MultiplicativeMagma a where times :: a -> a -> a

instance MultiplicativeMagma Double where times = (P.*)
instance MultiplicativeMagma Float where times = (P.*)
instance MultiplicativeMagma Int where times = (P.*)
instance MultiplicativeMagma Integer where times = (P.*)
instance MultiplicativeMagma Bool where times = (P.&&)
instance (Representable r, MultiplicativeMagma a) => MultiplicativeMagma (r a) where
    times = liftR2 times

-- | MultiplicativeUnital
--
-- > one `times` a == a
-- > a `times` one == a
class MultiplicativeMagma a => MultiplicativeUnital a where one :: a

instance MultiplicativeUnital Double where one = 1
instance MultiplicativeUnital Float where one = 1
instance MultiplicativeUnital Int where one = 1
instance MultiplicativeUnital Integer where one = 1
instance MultiplicativeUnital Bool where one = True
instance (Representable r, MultiplicativeUnital a) =>
    MultiplicativeUnital (r a) where
    one = pureRep one

-- | MultiplicativeCommutative
--
-- > a `times` b == b `times` a
class MultiplicativeMagma a => MultiplicativeCommutative a

instance MultiplicativeCommutative Double
instance MultiplicativeCommutative Float
instance MultiplicativeCommutative Int
instance MultiplicativeCommutative Integer
instance MultiplicativeCommutative Bool
instance (Representable r, MultiplicativeCommutative a) =>
    MultiplicativeCommutative (r a)

-- | MultiplicativeAssociative
--
-- > (a `times` b) `times` c == a `times` (b `times` c)
class MultiplicativeMagma a => MultiplicativeAssociative a

instance MultiplicativeAssociative Double
instance MultiplicativeAssociative Float
instance MultiplicativeAssociative Int
instance MultiplicativeAssociative Integer
instance MultiplicativeAssociative Bool
instance (Representable r, MultiplicativeAssociative a) =>
    MultiplicativeAssociative (r a)

-- | MultiplicativeInvertible
--
-- > ∀ a ∈ A: recip a ∈ A
--
-- law is true by construction in Haskell
class MultiplicativeMagma a => MultiplicativeInvertible a where recip :: a -> a

instance MultiplicativeInvertible Double where recip = P.recip
instance MultiplicativeInvertible Float where recip = P.recip
instance (Representable r, MultiplicativeInvertible a) =>
    MultiplicativeInvertible (r a) where
    recip = fmapRep recip

-- | MultiplicativeHomomorphic
--
-- > ∀ a ∈ A: timeshom a ∈ B
--
-- law is true by construction in Haskell
class ( MultiplicativeMagma b) =>
      MultiplicativeHomomorphic a b where
    timeshom :: a -> b

instance (Representable r, MultiplicativeMagma a) =>
    MultiplicativeHomomorphic a (r a) where
    timeshom a = pureRep a

instance MultiplicativeMagma a => MultiplicativeHomomorphic a a where
    timeshom a = a

-- | MultiplicativeMonoidal
class ( MultiplicativeUnital a
      , MultiplicativeAssociative a) =>
      MultiplicativeMonoidal a

instance MultiplicativeMonoidal Double
instance MultiplicativeMonoidal Float
instance MultiplicativeMonoidal Int
instance MultiplicativeMonoidal Integer
instance MultiplicativeMonoidal Bool
instance (Representable r, MultiplicativeMonoidal a) =>
    MultiplicativeMonoidal (r a)


-- | Multiplicative is commutative, associative and unital under multiplication
--
-- > a * b = b * a
--
-- > (a * b) * c = a * (b * c)
--
-- > one * a = a
--
-- > a * one = a
--
class ( MultiplicativeCommutative a
      , MultiplicativeUnital a
      , MultiplicativeAssociative a) =>
      Multiplicative a where
    infixl 7 *
    (*) :: a -> a -> a
    a * b = times a b

instance Multiplicative Double
instance Multiplicative Float
instance Multiplicative Int
instance Multiplicative Integer
instance Multiplicative Bool
instance (Representable r, Multiplicative a) => Multiplicative (r a)

-- | Non-commutative left divide
class ( MultiplicativeUnital a
      , MultiplicativeAssociative a
      , MultiplicativeInvertible a) =>
      MultiplicativeLeftCancellative a where
    infixl 7 ~/
    (~/) :: a -> a -> a
    a ~/ b = recip b `times` a

-- | Non-commutative right divide
class ( MultiplicativeUnital a
      , MultiplicativeAssociative a
      , MultiplicativeInvertible a) =>
      MultiplicativeRightCancellative a where
    infixl 7 /~
    (/~) :: a -> a -> a
    a /~ b = a `times` recip b

-- | MultiplicativeGroup
--
-- > a / a = one
--
-- > recip a = one / a
--
-- > recip a * a = one
--
class ( Multiplicative a
      , MultiplicativeInvertible a) =>
      MultiplicativeGroup a where
    infixl 7 /
    (/) :: a -> a -> a
    (/) a b = a `times` recip b

instance MultiplicativeGroup Double
instance MultiplicativeGroup Float
instance (Representable r, MultiplicativeGroup a) => MultiplicativeGroup (r a)

