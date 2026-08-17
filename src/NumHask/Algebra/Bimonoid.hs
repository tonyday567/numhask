{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Bimonoid structure at the value level.
--
-- In linear logic the exponentials @!A@ and @?A@ license the structural
-- rules of contraction/weakening and cocontraction/coweakening. These are
-- not properties of the value itself in isolation; they are algebraic
-- structure on how values may be used. Because functions are first-class,
-- that structure can be made explicit as type classes.
--
-- * 'Copyable' is the @!@ fragment: values may be duplicated and discarded.
-- * 'Mergeable' is the @?@ fragment: values may be combined with a unit.
--
-- For the function arrow @(->)@ the cartesian structure gives a canonical
-- 'Copyable' instance, while 'Mergeable' is pointwise whenever the codomain
-- is mergeable.
module NumHask.Algebra.Bimonoid
  ( Copyable (..),
    Mergeable (..),
  )
where

import Data.Bool (Bool (..), (||))
import Data.Set (Set)
import Data.Set qualified as Set
import NumHask.Algebra.Additive (Additive (..))
import Prelude qualified as P

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude

-- | A cocommutative comonoid in the category of Haskell functions.
--
-- This is the value-level content of the linear-logic exponential @!A@:
-- the value may be copied (contraction) and discarded (weakening).
class Copyable a where
  -- | Duplicate a value.
  copy :: a -> (a, a)

  -- | Discard a value.
  discard :: a -> ()

-- | A commutative monoid in the category of Haskell functions.
--
-- This is the value-level content of the linear-logic exponential @?A@:
-- values may be merged (cocontraction) and introduced from nothing
-- (coweakening).
class Mergeable a where
  -- | Combine two values.
  merge :: (a, a) -> a

  -- | The unit value, introduced from the terminal object @()@.
  mergeZero :: () -> a

-- | In the category of Haskell functions every object carries a canonical
-- cocommutative comonoid structure: the diagonal and the terminal map.
instance Copyable a where
  copy x = (x, x)
  discard _ = ()

-- | Function merging is pointwise.
instance (Mergeable b) => Mergeable (a -> b) where
  merge (f, g) x = merge (f x, g x)
  mergeZero () _ = mergeZero ()

-- | Unit merges trivially.
instance Mergeable () where
  merge ((), ()) = ()
  mergeZero () = ()

-- | Booleans merge by disjunction.
instance Mergeable Bool where
  merge (a, b) = a || b
  mergeZero () = False

-- | Numeric types merge by addition.
--
-- This generic instance covers every 'Additive' carrier; the unit is the
-- additive identity 'zero'.
instance (Additive a) => Mergeable a where
  merge (x, y) = x + y
  mergeZero () = zero

-- | Sets merge by union.
instance (P.Ord a) => Mergeable (Set a) where
  merge (s, t) = Set.union s t
  mergeZero () = Set.empty
