{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

-- | Metric classes
module NumHask.Analysis.Banach
  ( Banach(..)
  , Hilbert(..)
  )
where

import NumHask.Algebra.Abstract.Action
import NumHask.Algebra.Abstract.Ring
import NumHask.Algebra.Abstract.Field
import NumHask.Analysis.Metric

-- | Banach (with Norm) laws form rules around size and direction of a number, with a potential crossing into another codomain.
--
-- > a == singleton zero || normalizeL2 a *. normL2 a == a
class (ExpField (Actor h), Normed h (Actor h), DivisiveAction h) =>
  Banach h where
  normalizeL1 :: h -> h
  normalizeL1 a = a ./ normL1 a 

  normalizeL2 :: h -> h
  normalizeL2 a = a ./ normL2 a

-- | the inner product: a distributive fold
--
-- > a <.> b == b <.> a
-- > a <.> (b +c) == a <.> b + a <.> c
-- > a <.> (s *. b + c) == s * (a <.> b) + a <.> c
-- (s0 *. a) <.> (s1 *. b) == s0 * s1 * (a <.> b)
class (Distributive (Actor h)) =>
  Hilbert h where
  infix 8 <.>
  (<.>) :: h -> h -> Actor h
