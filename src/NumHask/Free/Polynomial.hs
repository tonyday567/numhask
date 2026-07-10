{-# LANGUAGE NoRebindableSyntax #-}
-- | Polynomial normal forms for 'Ring' terms.
--
-- The free ring on generators @a@ is the /noncommutative/ polynomial
-- ring ℤ⟨a⟩: integer combinations of words in the generators.
-- 'toWord' is its normal form — keys are words, order preserved.
--
-- The free /commutative/ ring is ℤ[a], reached from ℤ⟨a⟩ by
-- 'abelianize' (sort each word into a monomial bag).  'toPolynomial'
-- is the composite:
--
-- > toPolynomial = abelianize . toWord
--
-- The factoring keeps the bookkeeping honest: 'Ring' the class does
-- not require commutativity, so the sorted-key 'Polynomial' alone is
-- a /sound but incomplete/ decision procedure for free-'Ring'
-- equality (it identifies @xy@ with @yx@).  'toWord' is sound and
-- complete for ℤ⟨a⟩; 'toPolynomial' is sound and complete for ℤ[a].
module NumHask.Free.Polynomial
  ( -- * Commutative normal form — ℤ[a]
    Polynomial (..),
    toPolynomial,
    fromPolynomial,
    evalPolynomial,

    -- * Noncommutative normal form — ℤ⟨a⟩
    NCPolynomial (..),
    toWord,
    abelianize,
    evalNCPolynomial,
  )
where

import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as M
import NumHask.Algebra.Additive qualified as NHA
import NumHask.Algebra.Multiplicative qualified as NHM
import NumHask.Algebra.Ring qualified as NHR
import NumHask.Data.Integral qualified as NHI
import NumHask.Free.Ring
import Prelude (Eq, Integer, Ord, Show, foldr, (*), (+), (.), (/=))
import Prelude qualified as P

-- $setup
-- >>> import NumHask.Free.Ring
-- >>> import NumHask.Free.Polynomial
-- >>> import Prelude (fromInteger)
-- >>> import Prelude hiding (negate)

-- | Multivariate polynomial in commutative normal form — ℤ[a].
--
-- Keys are sorted monomials (bags of generators); the empty list is
-- the constant term.  Values are integer coefficients.
--
-- >>> let p = plus (times (embed "x") (embed "y")) (negate (embed "z"))
-- >>> toPolynomial p
-- Polynomial {unPolynomial = fromList [(["x","y"],1),(["z"],-1)]}
newtype Polynomial a = Polynomial {unPolynomial :: Map [a] Integer}
  deriving (Eq, Show)

-- | Noncommutative polynomial in normal form — ℤ⟨a⟩, the monoid ring
-- of the free monoid on the generators.
--
-- Keys are /words/: generator order is preserved, so @xy@ and @yx@
-- are distinct keys.  This is the faithful normal form for the free
-- 'Ring'.  ('Ord' is for the 'Map', not for commutativity.)
newtype NCPolynomial a = NCPolynomial {unNCPolynomial :: Map [a] Integer}
  deriving (Eq, Show)

-- | Normalize a 'Ring' term into ℤ⟨a⟩.
--
-- Distributes 'Times' over 'Plus', pushes 'Negate' to coefficients,
-- collects like /words/ — no sorting, so the commutator survives:
--
-- >>> let comm = minus (times (embed "x") (embed "y")) (times (embed "y") (embed "x"))
-- >>> toWord comm
-- NCPolynomial {unNCPolynomial = fromList [(["x","y"],1),(["y","x"],-1)]}
toWord :: (Ord a) => Ring a -> NCPolynomial a
toWord = NCPolynomial . M.filter (/= (0 :: Integer)) . go
  where
    go Zero = M.empty
    go One = M.singleton [] 1
    go (Embed a) = M.singleton [a] 1
    go (Negate r) = M.map P.negate (go r)
    go (Plus r s) = M.unionWith (+) (go r) (go s)
    go (Times r s) =
      M.fromListWith
        (+)
        [ (m1 P.++ m2, c1 * c2)
        | (m1, c1) <- M.toList (go r),
          (m2, c2) <- M.toList (go s)
        ]

-- | Quotient ℤ⟨a⟩ → ℤ[a]: sort each word into a monomial bag and
-- collect.  Coefficients that cancel under the identification are
-- dropped.
abelianize :: (Ord a) => NCPolynomial a -> Polynomial a
abelianize (NCPolynomial m) =
  Polynomial
    ( M.filter
        (/= (0 :: Integer))
        (M.fromListWith (+) [(sort w, c) | (w, c) <- M.toList m])
    )

-- | Normalize a 'Ring' term to commutative polynomial form.
--
-- The composite of the faithful normal form and the abelianization.
-- Sound for any 'Ring' target; complete only up to commutativity —
-- the commutator vanishes here:
--
-- >>> let comm = minus (times (embed "x") (embed "y")) (times (embed "y") (embed "x"))
-- >>> toPolynomial comm
-- Polynomial {unPolynomial = fromList []}
toPolynomial :: (Ord a) => Ring a -> Polynomial a
toPolynomial = abelianize . toWord

-- | Reconstruct a 'Ring' term from polynomial normal form.
fromPolynomial :: Polynomial a -> Ring a
fromPolynomial (Polynomial m) =
  foldr plus zero [term c ms | (ms, c) <- M.toList m, c P./= (0 :: Integer)]
  where
    term c ms = times (coeffToRing c) (foldr times one [embed x | x <- ms])
    coeffToRing n
      | n P.> 0 = foldr plus one (P.replicate (P.fromIntegral (n P.- 1)) one)
      | n P.< 0 = negate (coeffToRing (P.negate n))
      | P.otherwise = zero

-- | Evaluate a polynomial via a generator assignment.
--
-- >>> let p = toPolynomial (plus (times (embed "x") (embed "y")) (negate (embed "z")))
-- >>> evalPolynomial (\g -> case g of "x" -> 2; "y" -> 3; _ -> 5) p :: Int
-- 1
evalPolynomial ::
  (NHR.Ring b, NHI.FromInteger b) =>
  (a -> b) ->
  Polynomial a ->
  b
evalPolynomial f (Polynomial m) =
  foldr
    (NHA.+)
    NHA.zero
    [ NHI.fromInteger c NHM.* foldr (NHM.*) NHM.one [f x | x <- ms]
    | (ms, c) <- M.toList m
    ]

-- | Evaluate a noncommutative polynomial via a generator assignment.
--
-- Word order is respected, so this is the unique 'Ring' homomorphism
-- ℤ⟨a⟩ → b extending the assignment — lawful for noncommutative
-- targets where 'evalPolynomial' is not.
evalNCPolynomial ::
  (NHR.Ring b, NHI.FromInteger b) =>
  (a -> b) ->
  NCPolynomial a ->
  b
evalNCPolynomial f (NCPolynomial m) =
  foldr
    (NHA.+)
    NHA.zero
    [ NHI.fromInteger c NHM.* foldr (NHM.*) NHM.one [f x | x <- ms]
    | (ms, c) <- M.toList m
    ]
