{-# LANGUAGE NoRebindableSyntax #-}

-- | Free star semiring — the initial encoding of 'StarSemiring'.
--
-- The counting profile: 'Plus' does /not/ collapse duplicates.
-- For the idempotent profile, see 'kleeneSimplify'.
module NumHask.Free.StarSemiring
  ( StarSemiring (..),
    zero,
    one,
    plus,
    times,
    star,
    plusS,
    embed,
    eval,
    foldStarSemiring,
    fromAdditive,
    fromMultiplicative,
    kleeneSimplify,
  )
where

import Data.Set (Set)
import Data.Set qualified as S
import NumHask.Algebra.Additive qualified as NHA
import NumHask.Algebra.Multiplicative qualified as NHM
import NumHask.Algebra.Ring qualified as NHR
import NumHask.Free.Additive qualified as HA
import NumHask.Free.Multiplicative qualified as HM
import Prelude (Eq, Ord, Show, foldr1, (.))

-- $setup
-- >>> import Prelude (String)
-- >>> import Data.Bool (Bool (..), bool)

-- | Free star semiring over a carrier type.
--
-- The initial encoding of 'NumHask.Algebra.Ring.StarSemiring'.
-- Extends the free semiring with the Kleene star.
--
-- 'Negate' is absent — 'StarSemiring' requires only 'Distributive',
-- not 'Subtractive'.
data StarSemiring a
  = Zero
  | One
  | Plus (StarSemiring a) (StarSemiring a)
  | Times (StarSemiring a) (StarSemiring a)
  | Star (StarSemiring a)
  | Embed a
  deriving (Eq, Ord, Show)

-- | Additive identity.
zero :: StarSemiring a
zero = Zero

-- | Multiplicative identity.
one :: StarSemiring a
one = One

-- | Addition with identity absorption.
plus :: StarSemiring a -> StarSemiring a -> StarSemiring a
plus Zero b = b
plus a Zero = a
plus a b = Plus a b

-- | Multiplication with identity absorption.
times :: StarSemiring a -> StarSemiring a -> StarSemiring a
times One b = b
times a One = a
times a b = Times a b

-- | Kleene star with universally-lawful simplifications.
--
-- @star Zero = One@ holds in every star semiring
-- (@star 0 = 1 + 0·star 0@).  The rules @star One = One@ and
-- @star (star a) = star a@ are /not/ applied — they hold only in
-- Kleene algebra (idempotent profile), not in the counting profile
-- where @star 1 = ∞ ≠ 1@.
--
-- >>> star zero
-- One
star :: StarSemiring a -> StarSemiring a
star Zero = One
star a = Star a

-- | Positive closure: @a⁺ = a · a*@.
plusS :: StarSemiring a -> StarSemiring a
plusS a = times a (star a)

-- | Embed a carrier value as an atomic generator.
embed :: a -> StarSemiring a
embed = Embed

-- | Evaluate a term into any 'NumHask.Algebra.Ring.StarSemiring'.
--
-- This is the unique homomorphism out of the free star semiring.
eval :: (NHR.StarSemiring a) => StarSemiring a -> a
eval Zero = NHA.zero
eval One = NHM.one
eval (Plus a b) = eval a NHA.+ eval b
eval (Times a b) = eval a NHM.* eval b
eval (Star a) = NHR.star (eval a)
eval (Embed a) = a

-- ---------------------------------------------------------------------------
-- The free object is an instance of its own class — that is what
-- \"free\" means.  These instances are what let 'Harpie.NumHask.Matrix.starMatrix'
-- run at carrier @StarSemiring a@: Kleene's state elimination, the
-- fourth face of the four-for-one.

-- | Methods are the smart constructors, so identity absorption
-- happens during a matrix-star computation's block recursion
-- rather than after it.
--
-- >>> import qualified NumHask.Algebra.Ring as NHR
-- >>> NHR.star (one :: StarSemiring String)
-- Star One
instance NHA.Additive (StarSemiring a) where
  zero = Zero
  (+) = plus

instance NHM.Multiplicative (StarSemiring a) where
  one = One
  (*) = times

instance NHR.StarSemiring (StarSemiring a) where
  star = star

-- | Universal property: fold with a target star semiring.
foldStarSemiring ::
  b ->
  b ->
  (b -> b -> b) ->
  (b -> b -> b) ->
  (b -> b) ->
  (a -> b) ->
  StarSemiring a ->
  b
foldStarSemiring z o p t s f = go
  where
    go Zero = z
    go One = o
    go (Plus a b) = p (go a) (go b)
    go (Times a b) = t (go a) (go b)
    go (Star a) = s (go a)
    go (Embed a) = f a

-- | Inject an additive term into the free star semiring.
fromAdditive :: HA.Additive a -> StarSemiring a
fromAdditive HA.Zero = Zero
fromAdditive (HA.Plus a b) = Plus (fromAdditive a) (fromAdditive b)
fromAdditive (HA.Embed a) = Embed a

-- | Inject a multiplicative term into the free star semiring.
fromMultiplicative :: HM.Multiplicative a -> StarSemiring a
fromMultiplicative HM.One = One
fromMultiplicative (HM.Times a b) = Times (fromMultiplicative a) (fromMultiplicative b)
fromMultiplicative (HM.Embed a) = Embed a

-- | ACIZ simplification — Associativity, Commutativity, Idempotence
-- of 'Plus', with 'Zero' absorption.
--
-- Collapses duplicates under 'Plus' and absorbs 'Zero'.  Recurse
-- into 'Times' and 'Star' children.  This is shallow: full Kleene
-- algebra canonical form (automata minimisation) is not attempted.
--
-- Sound only when evaluated into an idempotent (Kleene algebra)
-- target; for counting targets (ℕ), 'kleeneSimplify' changes the
-- value of evaluation.
--
-- >>> let x = embed "x" :: StarSemiring String
-- >>> let y = embed "y"
-- >>> -- top-level duplicate collapses
-- >>> kleeneSimplify (plus x x)
-- Embed "x"
-- >>> -- child duplicate also collapses
-- >>> kleeneSimplify (times (plus x x) y)
-- Times (Embed "x") (Embed "y")
-- >>> -- star child normalised
-- >>> kleeneSimplify (star (plus x x))
-- Star (Embed "x")
--
-- Value-preserving for idempotent targets — executable witness over
-- the 'Warshall' Kleene algebra (a law, not a proof;
-- the @Arbitrary@-powered property awaits a test-suite home rather
-- than a doctest):
--
-- >>> import NumHask.Free.Carriers (Warshall (..))
-- >>> let t = times (plus (embed (Warshall True)) (embed (Warshall True))) (embed (Warshall False))
-- >>> (eval (kleeneSimplify t), eval t)
-- (Warshall False,Warshall False)
kleeneSimplify :: (Ord a) => StarSemiring a -> StarSemiring a
kleeneSimplify = fromSet . toSet
  where
    normChild :: (Ord a) => StarSemiring a -> StarSemiring a
    normChild = kleeneSimplify

    toSet :: (Ord a) => StarSemiring a -> Set (StarSemiring a)
    toSet Zero = S.empty
    toSet (Plus a b) = S.union (toSet a) (toSet b)
    toSet (Times a b) = S.singleton (Times (normChild a) (normChild b))
    toSet (Star a) = S.singleton (Star (normChild a))
    toSet a = S.singleton a

    fromSet :: Set (StarSemiring a) -> StarSemiring a
    fromSet s = case S.toList s of
      [] -> Zero
      xs -> foldr1 Plus xs
