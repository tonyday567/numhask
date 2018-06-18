{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
-- | The Ring hirarchy
module NumHask.Algebra.Abstract.Ring
    (
        Distribution
    ,   Semiring
    ,   Ring
    ,   CommutativeRing
    ,   IntegralDomain
    )
    where

import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Addition
import NumHask.Algebra.Abstract.Multiplication
import qualified Prelude                       as P
import           Data.Complex                   ( Complex(..) )
import           Data.Int                       ( Int8
                                                , Int16
                                                , Int32
                                                , Int64
                                                )
import           Data.Word                      ( Word
                                                , Word8
                                                , Word16
                                                , Word32
                                                , Word64
                                                )
import           GHC.Natural                    ( Natural(..) )

-- | Distribution laws
--
-- > a * (b + c) == a * b + a * c
-- > (a * b) * c == a * c + b * c
class (Addition a, Multiplication a) =>
    Distribution a

instance Distribution P.Double

instance Distribution P.Float

instance (Distribution a, AbelianGroup (Add a)) => Distribution (Complex a)

instance Distribution P.Int
instance Distribution P.Integer 
instance Distribution Natural 
instance Distribution Int8 
instance Distribution Int16 
instance Distribution Int32 
instance Distribution Int64 
instance Distribution Word 
instance Distribution Word8 
instance Distribution Word16 
instance Distribution Word32 
instance Distribution Word64 

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
--  this essentially is a generalization of division and a fundamental step towards a Field
class (CommutativeRing a, Invertible (Mult a)) =>
    IntegralDomain a

instance IntegralDomain P.Double

instance IntegralDomain P.Float

instance (IntegralDomain a) => IntegralDomain (Complex a)