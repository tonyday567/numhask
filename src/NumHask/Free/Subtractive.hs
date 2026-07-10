{-# LANGUAGE NoRebindableSyntax #-}
-- | Free abelian group — the initial encoding of 'Subtractive'.
module NumHask.Free.Subtractive
  ( Subtractive (..),
    zero,
    plus,
    negate,
    minus,
    embed,
    eval,
    foldSubtractive,
  )
where

import NumHask.Algebra.Additive qualified as NH
import Prelude (Eq, Show)

-- | Free abelian group over a carrier type.
--
-- The initial encoding of 'NumHask.Algebra.Subtractive.Subtractive'.
-- Extends the free commutative monoid with an antipode.
data Subtractive a
  = Zero
  | Plus (Subtractive a) (Subtractive a)
  | Negate (Subtractive a)
  | Embed a
  deriving (Eq, Show)

-- | Additive identity.
zero :: Subtractive a
zero = Zero

-- | Addition with identity absorption.
plus :: Subtractive a -> Subtractive a -> Subtractive a
plus Zero b = b
plus a Zero = a
plus a b = Plus a b

-- | Antipode with involution cancellation.
--
-- > negate zero = zero
-- > negate (negate a) = a
negate :: Subtractive a -> Subtractive a
negate Zero = Zero
negate (Negate a) = a
negate a = Negate a

-- | Subtraction as addition of the antipode.
minus :: Subtractive a -> Subtractive a -> Subtractive a
minus a b = plus a (negate b)

-- | Embed a carrier value as an atomic generator.
embed :: a -> Subtractive a
embed = Embed

-- | Evaluate a term into any 'NumHask.Algebra.Additive.Subtractive'.
--
-- This is the unique homomorphism out of the free abelian group.
eval :: (NH.Subtractive a) => Subtractive a -> a
eval Zero = NH.zero
eval (Plus a b) = eval a NH.+ eval b
eval (Negate a) = NH.negate (eval a)
eval (Embed a) = a

-- | Universal property: fold with a target abelian group.
foldSubtractive :: b -> (b -> b -> b) -> (b -> b) -> (a -> b) -> Subtractive a -> b
foldSubtractive z p n f = go
  where
    go Zero = z
    go (Plus a b) = p (go a) (go b)
    go (Negate a) = n (go a)
    go (Embed a) = f a
