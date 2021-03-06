{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Ring classes
module NumHask.Algebra.Ring
  ( Distributive,
    Ring,
    StarSemiring (..),
    KleeneAlgebra,
    InvolutiveRing (..),
    two,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Natural (Natural (..))
import NumHask.Algebra.Additive
import NumHask.Algebra.Group
import NumHask.Algebra.Multiplicative
import qualified Prelude as P

-- $setup
--
-- >>> :set -XRebindableSyntax
-- >>> :set -XNegativeLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XScopedTypeVariables
-- >>> import NumHask.Prelude
-- >>> import Test.QuickCheck

-- | <https://en.wikipedia.org/wiki/Distributive_property Distributive>
--
-- prop> \a b c -> a * (b + c) == a * b + a * c
-- prop> \a b c -> (a + b) * c == a * c + b * c
-- prop> \a -> zero * a == zero
-- prop> \a -> a * zero == zero
--
-- The sneaking in of the <https://en.wikipedia.org/wiki/Absorbing_element Absorption> laws here glosses over the possibility that the multiplicative zero element does not have to correspond with the additive unital zero.
class
  (Additive a, Multiplicative a) =>
  Distributive a

instance Distributive P.Double

instance Distributive P.Float

instance Distributive P.Int

instance Distributive P.Integer

instance Distributive Natural

instance Distributive Int8

instance Distributive Int16

instance Distributive Int32

instance Distributive Int64

instance Distributive Word

instance Distributive Word8

instance Distributive Word16

instance Distributive Word32

instance Distributive Word64

instance Distributive P.Bool

instance Distributive b => Distributive (a -> b)

-- | A <https://en.wikipedia.org/wiki/Ring_(mathematics) Ring> is an abelian group under addition ('Unital', 'Associative', 'Commutative', 'Invertible') and monoidal under multiplication ('Unital', 'Associative'), and where multiplication distributes over addition.
--
-- prop> \a -> zero + a == a
-- prop> \a -> a + zero == a
-- prop> \a b c -> (a + b) + c == a + (b + c)
-- prop> \a b -> a + b == b + a
-- prop> \a -> a - a == zero
-- prop> \a -> negate a == zero - a
-- prop> \a -> negate a + a == zero
-- prop> \a -> a + negate a == zero
-- prop> \a -> one * a == a
-- prop> \a -> a * one == a
-- prop> \a b c -> (a * b) * c == a * (b * c)
-- prop> \a b c -> a * (b + c) == a * b + a * c
-- prop> \a b c -> (a + b) * c == a * c + b * c
-- prop> \a -> zero * a == zero
-- prop> \a -> a * zero == zero
class
  (Distributive a, Subtractive a) =>
  Ring a

instance
  (Distributive a, Subtractive a) =>
  Ring a

-- | A <https://en.wikipedia.org/wiki/Semiring#Star_semirings StarSemiring> is a semiring with an additional unary operator (star) satisfying:
--
-- > \a -> star a = one + a * star a
class (Distributive a) => StarSemiring a where
  star :: a -> a
  star a = one + plus a

  plus :: a -> a
  plus a = a * star a

instance StarSemiring b => StarSemiring (a -> b)

-- | A <https://en.wikipedia.org/wiki/Kleene_algebra Kleene Algebra> is a Star Semiring with idempotent addition.
--
-- > a * x + x = a ==> star a * x + x = x
-- > x * a + x = a ==> x * star a + x = x
class (StarSemiring a, Idempotent a) => KleeneAlgebra a

instance KleeneAlgebra b => KleeneAlgebra (a -> b)

-- | Involutive Ring
--
-- > adj (a + b) ==> adj a + adj b
-- > adj (a * b) ==> adj a * adj b
-- > adj one ==> one
-- > adj (adj a) ==> a
--
-- Note: elements for which @adj a == a@ are called "self-adjoint".
class (Distributive a) => InvolutiveRing a where
  adj :: a -> a
  adj x = x

instance InvolutiveRing P.Double

instance InvolutiveRing P.Float

instance InvolutiveRing P.Integer

instance InvolutiveRing P.Int

instance InvolutiveRing Natural

instance InvolutiveRing Int8

instance InvolutiveRing Int16

instance InvolutiveRing Int32

instance InvolutiveRing Int64

instance InvolutiveRing Word

instance InvolutiveRing Word8

instance InvolutiveRing Word16

instance InvolutiveRing Word32

instance InvolutiveRing Word64

instance InvolutiveRing b => InvolutiveRing (a -> b)

-- | Defining 'two' requires adding the multiplicative unital to itself. In other words, the concept of 'two' is a Ring one.
--
-- >>> two
-- 2
two :: (Multiplicative a, Additive a) => a
two = one + one
