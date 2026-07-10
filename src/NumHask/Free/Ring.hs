{-# LANGUAGE NoRebindableSyntax #-}

-- | Free ring — the initial encoding of 'Ring'.
module NumHask.Free.Ring
  ( Ring (..),
    zero,
    one,
    plus,
    negate,
    minus,
    times,
    embed,
    eval,
    foldRing,
    fromAdditive,
    fromMultiplicative,
    fromSubtractive,
  )
where

import NumHask.Algebra.Additive qualified as NHA
import NumHask.Algebra.Multiplicative qualified as NHM
import NumHask.Algebra.Ring qualified as NHR
import NumHask.Free.Additive qualified as HA
import NumHask.Free.Multiplicative qualified as HM
import NumHask.Free.Subtractive qualified as HS
import Prelude (Eq, Show)

-- | Free ring over a carrier type.
--
-- The initial encoding of 'NumHask.Algebra.Ring.Ring'.
-- Combines additive and multiplicative structure with the
-- antipode.  Distributivity is enforced by 'eval', not by
-- the term structure — 'Times' over 'Plus' is not reduced
-- automatically.  Use 'eval' to project into a lawful ring.
data Ring a
  = Zero
  | One
  | Plus (Ring a) (Ring a)
  | Negate (Ring a)
  | Times (Ring a) (Ring a)
  | Embed a
  deriving (Eq, Show)

-- | Additive identity.
zero :: Ring a
zero = Zero

-- | Multiplicative identity.
one :: Ring a
one = One

-- | Addition with identity absorption.
plus :: Ring a -> Ring a -> Ring a
plus Zero b = b
plus a Zero = a
plus a b = Plus a b

-- | Antipode with involution cancellation.
negate :: Ring a -> Ring a
negate Zero = Zero
negate (Negate a) = a
negate a = Negate a

-- | Subtraction as addition of the antipode.
minus :: Ring a -> Ring a -> Ring a
minus a b = plus a (negate b)

-- | Multiplication with identity absorption.
times :: Ring a -> Ring a -> Ring a
times One b = b
times a One = a
times a b = Times a b

-- | Embed a carrier value as an atomic generator.
embed :: a -> Ring a
embed = Embed

-- | Evaluate a term into any 'NumHask.Algebra.Ring.Ring'.
--
-- This is the unique homomorphism out of the free ring.
eval :: (NHR.Ring a) => Ring a -> a
eval Zero = NHA.zero
eval One = NHM.one
eval (Plus a b) = eval a NHA.+ eval b
eval (Negate a) = NHA.negate (eval a)
eval (Times a b) = eval a NHM.* eval b
eval (Embed a) = a

-- | Universal property: fold with a target ring.
foldRing ::
  b ->
  b ->
  (b -> b -> b) ->
  (b -> b) ->
  (b -> b -> b) ->
  (a -> b) ->
  Ring a ->
  b
foldRing z o p n t f = go
  where
    go Zero = z
    go One = o
    go (Plus a b) = p (go a) (go b)
    go (Negate a) = n (go a)
    go (Times a b) = t (go a) (go b)
    go (Embed a) = f a

-- | Inject an additive term into the free ring.
fromAdditive :: HA.Additive a -> Ring a
fromAdditive HA.Zero = Zero
fromAdditive (HA.Plus a b) = Plus (fromAdditive a) (fromAdditive b)
fromAdditive (HA.Embed a) = Embed a

-- | Inject a multiplicative term into the free ring.
fromMultiplicative :: HM.Multiplicative a -> Ring a
fromMultiplicative HM.One = One
fromMultiplicative (HM.Times a b) = Times (fromMultiplicative a) (fromMultiplicative b)
fromMultiplicative (HM.Embed a) = Embed a

-- | Inject a subtractive term into the free ring.
fromSubtractive :: HS.Subtractive a -> Ring a
fromSubtractive HS.Zero = Zero
fromSubtractive (HS.Plus a b) = Plus (fromSubtractive a) (fromSubtractive b)
fromSubtractive (HS.Negate a) = Negate (fromSubtractive a)
fromSubtractive (HS.Embed a) = Embed a
