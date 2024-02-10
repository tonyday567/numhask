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
import NumHask.Algebra.Additive (Additive ((+)), Subtractive)
import NumHask.Algebra.Group (Idempotent)
import NumHask.Algebra.Multiplicative (Multiplicative (..))
import Prelude qualified as P

-- $setup
--
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude

-- | <https://en.wikipedia.org/wiki/Distributive_property Distributive>
--
-- prop> \a b c -> a * (b + c) == a * b + a * c
-- prop> \a b c -> (a + b) * c == a * c + b * c
-- prop> \a -> zero * a == zero
-- prop> \a -> a * zero == zero
--
-- The sneaking in of the <https://en.wikipedia.org/wiki/Absorbing_element Absorption> laws here glosses over the possibility that the multiplicative zero element does not have to correspond with the additive unital zero.
type Distributive a = (Additive a, Multiplicative a)

-- | A <https://en.wikipedia.org/wiki/Ring_(mathematics) Ring> is an abelian group under addition ('NumHask.Algebra.Unital', 'NumHask.Algebra.Associative', 'NumHask.Algebra.Commutative', 'NumHask.Algebra.Invertible') and monoidal under multiplication ('NumHask.Algebra.Unital', 'NumHask.Algebra.Associative'), and where multiplication distributes over addition.
--
-- > \a -> zero + a == a
-- > \a -> a + zero == a
-- > \a b c -> (a + b) + c == a + (b + c)
-- > \a b -> a + b == b + a
-- > \a -> a - a == zero
-- > \a -> negate a == zero - a
-- > \a -> negate a + a == zero
-- > \a -> a + negate a == zero
-- > \a -> one * a == a
-- > \a -> a * one == a
-- > \a b c -> (a * b) * c == a * (b * c)
-- > \a b c -> a * (b + c) == a * b + a * c
-- > \a b c -> (a + b) * c == a * c + b * c
-- > \a -> zero * a == zero
-- > \a -> a * zero == zero
type Ring a = (Distributive a, Subtractive a)

-- | A <https://en.wikipedia.org/wiki/Semiring#Star_semirings StarSemiring> is a semiring with an additional unary operator (star) satisfying:
--
-- > \a -> star a == one + a * star a
class (Distributive a) => StarSemiring a where
  {-# MINIMAL star | plus #-}

  star :: a -> a
  star a = one + plus a

  plus :: a -> a
  plus a = a * star a

-- | A <https://en.wikipedia.org/wiki/Kleene_algebra Kleene Algebra> is a Star Semiring with idempotent addition.
--
-- > a * x + x = a ==> star a * x + x = x
-- > x * a + x = a ==> x * star a + x = x
class (StarSemiring a, Idempotent a) => KleeneAlgebra a

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

-- | Defining 'two' requires adding the multiplicative unital to itself. In other words, the concept of 'two' is a Ring one.
--
-- >>> two
-- 2
two :: (Multiplicative a, Additive a) => a
two = one + one
