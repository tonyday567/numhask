{-# LANGUAGE NoRebindableSyntax #-}
-- | Free monoid — the initial encoding of 'Multiplicative'.
module NumHask.Free.Multiplicative
  ( Multiplicative (..),
    one,
    times,
    embed,
    eval,
    foldMultiplicative,
    flatten,
  )
where

import NumHask.Algebra.Multiplicative qualified as NH
import Prelude (Eq, Show)
import Prelude qualified as P

-- | Free monoid over a carrier type.
--
-- The initial encoding of 'NumHask.Algebra.Multiplicative.Multiplicative'.
-- Terms are built from 'one', 'times', and 'embed'.
data Multiplicative a
  = One
  | Times (Multiplicative a) (Multiplicative a)
  | Embed a
  deriving (Eq, Show)

-- | Multiplicative identity.
one :: Multiplicative a
one = One

-- | Multiplication with identity absorption.
--
-- > times one a = a
-- > times a one = a
times :: Multiplicative a -> Multiplicative a -> Multiplicative a
times One b = b
times a One = a
times a b = Times a b

-- | Embed a carrier value as an atomic generator.
embed :: a -> Multiplicative a
embed = Embed

-- | Evaluate a term into any 'NumHask.Algebra.Multiplicative.Multiplicative'.
--
-- This is the unique homomorphism out of the free monoid.
eval :: (NH.Multiplicative a) => Multiplicative a -> a
eval One = NH.one
eval (Times a b) = eval a NH.* eval b
eval (Embed a) = a

-- | Universal property: fold with a target monoid.
foldMultiplicative :: b -> (b -> b -> b) -> (a -> b) -> Multiplicative a -> b
foldMultiplicative z p f = go
  where
    go One = z
    go (Times a b) = p (go a) (go b)
    go (Embed a) = f a

-- | Flatten a term to a list of embedded generators, discarding identities.
flatten :: Multiplicative a -> [a]
flatten = foldMultiplicative [] (P.++) (: [])
