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
-- >>> :m -Prelude
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

-- | A <https://en.wikipedia.org/wiki/Semiring#Star_semirings StarSemiring> is a semiring with a unary star operator satisfying the Conway equations:
--
-- > \a -> star a == one + a * star a                                  -- fixpoint
-- > \a b -> star (a * b) == one + a * star (b * a) * b                -- product-star (sliding)
-- > \a b -> star (a + b) == star (star a * b) * star a                -- sum-star (vanishing)
--
-- These three equations are the doctestable core; they are exactly the sliding and vanishing axioms of a traced category in semiring clothing.
class (Distributive a) => StarSemiring a where
  {-# MINIMAL star | plus #-}

  star :: a -> a
  star a = one + plus a

  plus :: a -> a
  plus a = a * star a

-- | A <https://en.wikipedia.org/wiki/Kleene_algebra Kleene Algebra> is a Star Semiring with idempotent addition.
--
-- Idempotent addition gives a natural order @a <= b ⟺ a + b == b@. In that order, Kozen's induction laws hold as derived facts:
--
-- > a * x + x <= x  ==>  star a * x + x <= x
-- > x * a + x <= x  ==>  x * star a + x <= x
--
-- They are stated here as prose rather than class laws because they involve a partial order and Horn clauses, which do not fit the equational/doctest style of the Conway core.
class (StarSemiring a, Idempotent a) => KleeneAlgebra a

instance StarSemiring P.Bool where
  star _ = P.True

instance KleeneAlgebra P.Bool

-- | Conway equations for 'Bool'.
--
-- >>> let a = False; b = True in star (a * b) == one + a * star (b * a) * b
-- True
--
-- >>> let a = False; b = True in star (a + b) == star (star a * b) * star a
-- True

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
