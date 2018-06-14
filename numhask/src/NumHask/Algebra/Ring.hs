{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
-- | The Ring hirarchy
module NumHask.Algebra.Ring
    (
        Distribution
    ,   Semiring
    ,   Ring
    ,   CommutativeRing
    ,   IntegralDomain
    )
    where

import NumHask.Algebra.Group

-- | Distribution laws
--
-- > a * (b + c) == a * b + a * c
-- > (a * b) * c == a * c + b * c
class (Addition a, Multiplication a) =>
    Distribution a

-- | Semiring
-- FIXME: rule zero' = zero. Is this somehow expressible in haskell?
class (Monoid (Add a), Monoid (Mult a), Distribution a) =>
    Semiring a where
instance (Monoid (Add a), Monoid (Mult a), Distribution a) =>
    Semiring a

-- | Ring
class (Semiring a, AbelianGroup (Add a)) =>
    Ring a
instance (Semiring a, AbelianGroup (Add a)) =>
    Ring a

-- | Ring with a commutative Multiplication
class (Ring a, Commutative (Mult a)) =>
    CommutativeRing a
instance (Ring a, Commutative (Mult a)) =>
    CommutativeRing a

-- | generalization of ring of integers
--  rules:
--  product of any two nonzero elements is nonzero, also
--  if a ≠ 0, an equality ab = ac implies b = c.
class (CommutativeRing a) =>
    IntegralDomain a