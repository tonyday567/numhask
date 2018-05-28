{-# OPTIONS_GHC -Wall #-}

-- | A magma heirarchy for addition. The basic magma structure is repeated and prefixed with 'Additive-'.
module NumHask.Algebra.Additive
  ( AdditiveMagma(..)
  , AdditiveUnital(..)
  , AdditiveAssociative
  , AdditiveCommutative
  , AdditiveInvertible(..)
  , AdditiveIdempotent
  , sum
  , Additive(..)
  , AdditiveRightCancellative(..)
  , AdditiveLeftCancellative(..)
  , AdditiveGroup(..)
  , subtract
  ) where

import Data.Complex (Complex(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Natural (Natural(..))

import qualified Prelude as P
import Prelude (Bool(..), Double, Float, Int, Integer)

-- | 'plus' is used as the operator for the additive magma to distinguish from '+' which, by convention, implies commutativity
--
-- > ∀ a,b ∈ A: a `plus` b ∈ A
--
-- law is true by construction in Haskell
class AdditiveMagma a where
  plus :: a -> a -> a

instance AdditiveMagma Double where
  plus = (P.+)

instance AdditiveMagma Float where
  plus = (P.+)

instance AdditiveMagma Int where
  plus = (P.+)

instance AdditiveMagma Integer where
  plus = (P.+)

instance AdditiveMagma Bool where
  plus = (P.||)

instance (AdditiveMagma a) => AdditiveMagma (Complex a) where
  (rx :+ ix) `plus` (ry :+ iy) = (rx `plus` ry) :+ (ix `plus` iy)

instance AdditiveMagma Natural where
  plus = (P.+)

instance AdditiveMagma Int8 where
  plus = (P.+)

instance AdditiveMagma Int16 where
  plus = (P.+)

instance AdditiveMagma Int32 where
  plus = (P.+)

instance AdditiveMagma Int64 where
  plus = (P.+)

instance AdditiveMagma Word where
  plus = (P.+)

instance AdditiveMagma Word8 where
  plus = (P.+)

instance AdditiveMagma Word16 where
  plus = (P.+)

instance AdditiveMagma Word32 where
  plus = (P.+)

instance AdditiveMagma Word64 where
  plus = (P.+)

-- | Unital magma for addition.
--
-- > zero `plus` a == a
-- > a `plus` zero == a
class AdditiveMagma a =>
      AdditiveUnital a where
  zero :: a

instance AdditiveUnital Double where
  zero = 0

instance AdditiveUnital Float where
  zero = 0

instance AdditiveUnital Int where
  zero = 0

instance AdditiveUnital Integer where
  zero = 0

instance AdditiveUnital Bool where
  zero = False

instance (AdditiveUnital a) => AdditiveUnital (Complex a) where
  zero = zero :+ zero

instance AdditiveUnital Natural where
  zero = 0

instance AdditiveUnital Int8 where
  zero = 0

instance AdditiveUnital Int16 where
  zero = 0

instance AdditiveUnital Int32 where
  zero = 0

instance AdditiveUnital Int64 where
  zero = 0

instance AdditiveUnital Word where
  zero = 0

instance AdditiveUnital Word8 where
  zero = 0

instance AdditiveUnital Word16 where
  zero = 0

instance AdditiveUnital Word32 where
  zero = 0

instance AdditiveUnital Word64 where
  zero = 0

-- | Associative magma for addition.
--
-- > (a `plus` b) `plus` c == a `plus` (b `plus` c)
class AdditiveMagma a =>
      AdditiveAssociative a

instance AdditiveAssociative Double

instance AdditiveAssociative Float

instance AdditiveAssociative Int

instance AdditiveAssociative Integer

instance AdditiveAssociative Bool

instance (AdditiveAssociative a) => AdditiveAssociative (Complex a)

instance AdditiveAssociative Natural

instance AdditiveAssociative Int8

instance AdditiveAssociative Int16

instance AdditiveAssociative Int32

instance AdditiveAssociative Int64

instance AdditiveAssociative Word

instance AdditiveAssociative Word8

instance AdditiveAssociative Word16

instance AdditiveAssociative Word32

instance AdditiveAssociative Word64

-- | Commutative magma for addition.
--
-- > a `plus` b == b `plus` a
class AdditiveMagma a =>
      AdditiveCommutative a

instance AdditiveCommutative Double

instance AdditiveCommutative Float

instance AdditiveCommutative Int

instance AdditiveCommutative Integer

instance AdditiveCommutative Bool

instance (AdditiveCommutative a) => AdditiveCommutative (Complex a)

instance AdditiveCommutative Natural

instance AdditiveCommutative Int8

instance AdditiveCommutative Int16

instance AdditiveCommutative Int32

instance AdditiveCommutative Int64

instance AdditiveCommutative Word

instance AdditiveCommutative Word8

instance AdditiveCommutative Word16

instance AdditiveCommutative Word32

instance AdditiveCommutative Word64

-- | Invertible magma for addition.
--
-- > ∀ a ∈ A: negate a ∈ A
--
-- law is true by construction in Haskell
class AdditiveMagma a =>
      AdditiveInvertible a where
  negate :: a -> a

instance AdditiveInvertible Double where
  negate = P.negate

instance AdditiveInvertible Float where
  negate = P.negate

instance AdditiveInvertible Int where
  negate = P.negate

instance AdditiveInvertible Integer where
  negate = P.negate

instance AdditiveInvertible Bool where
  negate = P.not

instance (AdditiveInvertible a) => AdditiveInvertible (Complex a) where
  negate (rx :+ ix) = negate rx :+ negate ix

instance AdditiveInvertible Int8 where
  negate = P.negate

instance AdditiveInvertible Int16 where
  negate = P.negate

instance AdditiveInvertible Int32 where
  negate = P.negate

instance AdditiveInvertible Int64 where
  negate = P.negate

instance AdditiveInvertible Word where
  negate = P.negate

instance AdditiveInvertible Word8 where
  negate = P.negate

instance AdditiveInvertible Word16 where
  negate = P.negate

instance AdditiveInvertible Word32 where
  negate = P.negate

instance AdditiveInvertible Word64 where
  negate = P.negate

-- | Idempotent magma for addition.
--
-- > a `plus` a == a
class AdditiveMagma a =>
      AdditiveIdempotent a

instance AdditiveIdempotent Bool

-- | sum definition avoiding a clash with the Sum monoid in base
-- fixme: fit in with the Sum monoid
--
sum :: (Additive a, P.Foldable f) => f a -> a
sum = P.foldr (+) zero

-- | Additive is commutative, unital and associative under addition
--
-- > zero + a == a
-- > a + zero == a
-- > (a + b) + c == a + (b + c)
-- > a + b == b + a
class (AdditiveCommutative a, AdditiveUnital a, AdditiveAssociative a) =>
      Additive a where
  infixl 6 +
  (+) :: a -> a -> a
  a + b = plus a b

instance Additive Double

instance Additive Float

instance Additive Int

instance Additive Integer

instance Additive Bool

instance (Additive a) => Additive (Complex a)

instance Additive Natural

instance Additive Int8

instance Additive Int16

instance Additive Int32

instance Additive Int64

instance Additive Word

instance Additive Word8

instance Additive Word16

instance Additive Word32

instance Additive Word64

-- | Non-commutative left minus
--
-- > negate a `plus` a = zero
class (AdditiveUnital a, AdditiveAssociative a, AdditiveInvertible a) =>
      AdditiveLeftCancellative a where
  infixl 6 ~-
  (~-) :: a -> a -> a
  (~-) a b = negate b `plus` a

-- | Non-commutative right minus
--
-- > a `plus` negate a = zero
class (AdditiveUnital a, AdditiveAssociative a, AdditiveInvertible a) =>
      AdditiveRightCancellative a where
  infixl 6 -~
  (-~) :: a -> a -> a
  (-~) a b = a `plus` negate b

-- | Minus ('-') is reserved for where both the left and right cancellative laws hold.  This then implies that the AdditiveGroup is also Abelian.
--
-- Syntactic unary negation - substituting "negate a" for "-a" in code - is hard-coded in the language to assume a Num instance.  So, for example, using ''-a = zero - a' for the second rule below doesn't work.
--
-- > a - a = zero
-- > negate a = zero - a
-- > negate a + a = zero
-- > a + negate a = zero
class (Additive a, AdditiveInvertible a) =>
      AdditiveGroup a where
  infixl 6 -
  (-) :: a -> a -> a
  (-) a b = a `plus` negate b

instance AdditiveGroup Double

instance AdditiveGroup Float

instance AdditiveGroup Int

instance AdditiveGroup Integer

instance (AdditiveGroup a) => AdditiveGroup (Complex a)

instance AdditiveGroup Int8

instance AdditiveGroup Int16

instance AdditiveGroup Int32

instance AdditiveGroup Int64

instance AdditiveGroup Word

instance AdditiveGroup Word8

instance AdditiveGroup Word16

instance AdditiveGroup Word32

instance AdditiveGroup Word64

subtract :: (AdditiveGroup a) => a -> a -> a
subtract = P.flip (-)
