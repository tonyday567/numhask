{-# LANGUAGE NoRebindableSyntax #-}

-- | Free commutative monoid — the initial encoding of 'Additive'.
module NumHask.Free.Additive
  ( Additive (..),
    zero,
    plus,
    embed,
    eval,
    foldAdditive,
    flatten,
  )
where

import NumHask.Algebra.Additive qualified as NH
import Prelude (Eq, Show)
import Prelude qualified as P

-- | Free commutative monoid over a carrier type.
--
-- The initial encoding of 'NumHask.Algebra.Additive.Additive'.
-- Terms are built from 'zero', 'plus', and 'embed'.
--
-- Use smart constructors 'zero' and 'plus' for identity-normalised
-- terms. 'eval' projects into any target monoid satisfying the
-- 'NumHask.Algebra.Additive.Additive' laws.
data Additive a
  = Zero
  | Plus (Additive a) (Additive a)
  | Embed a
  deriving (Eq, Show)

-- | Additive identity.
zero :: Additive a
zero = Zero

-- | Addition with identity absorption.
--
-- > plus zero a = a
-- > plus a zero = a
plus :: Additive a -> Additive a -> Additive a
plus Zero b = b
plus a Zero = a
plus a b = Plus a b

-- | Embed a carrier value as an atomic generator.
embed :: a -> Additive a
embed = Embed

-- $setup
-- >>> import Prelude (fromInteger)

-- | Evaluate a term into any 'NumHask.Algebra.Additive.Additive'.
--
-- This is the unique homomorphism out of the free commutative monoid.
--
-- >>> eval (plus (embed 1) (embed 2))
-- 3
eval :: (NH.Additive a) => Additive a -> a
eval Zero = NH.zero
eval (Plus a b) = eval a NH.+ eval b
eval (Embed a) = a

-- | Universal property: fold with a target monoid.
--
-- > foldAdditive z p f . embed == f
-- > foldAdditive z p f zero   == z
-- > foldAdditive z p f (plus a b) == p (foldAdditive z p f a) (foldAdditive z p f b)
foldAdditive :: b -> (b -> b -> b) -> (a -> b) -> Additive a -> b
foldAdditive z p f = go
  where
    go Zero = z
    go (Plus a b) = p (go a) (go b)
    go (Embed a) = f a

-- | Flatten a term to a list of embedded generators, discarding
-- identities.  This is the bag-of-generators view of the free
-- commutative monoid; commutativity is not enforced structurally.
flatten :: Additive a -> [a]
flatten = foldAdditive [] (P.++) (: [])
