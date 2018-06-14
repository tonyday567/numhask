{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# OPTIONS_GHC -Wall #-}

-- | Metric classes
module NumHask.Analysis.Banach
  ( Signed(..)
  , Normed(..)
  , Metric(..)
  , Epsilon(..)
  , (≈)
  ) where

import qualified Prelude as P
-- | Banach (with Norm) laws form rules around size and direction of a number, with a potential crossing into another codomain.
--
-- > a == singleton zero || normalizeL2 a *. normL2 a == a
class (ExpField a, Normed (r a) a, MultiplicativeGroupModule r a) =>
      Banach r a where
  normalizeL1 :: r a -> r a
  normalizeL1 a = a ./ normL1 a

  normalizeL2 :: r a -> r a
  normalizeL2 a = a ./ normL2 a

  normalizeLp :: a -> r a -> r a
  normalizeLp p a = a ./ normLp p a

-- | the inner product of a representable over a semiring
--
-- > a <.> b == b <.> a
-- > a <.> (b +c) == a <.> b + a <.> c
-- > a <.> (s *. b + c) == s * (a <.> b) + a <.> c
-- (s0 *. a) <.> (s1 *. b) == s0 * s1 * (a <.> b)
class (Semiring a) =>
      Hilbert r a where
  infix 8 <.>
  (<.>) :: r a -> r a -> a