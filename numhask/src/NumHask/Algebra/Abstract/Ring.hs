{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
-- | The Ring hirarchy
module NumHask.Algebra.Abstract.Ring
    (
        Distributive
    ,   Semiring
    ,   Ring
    ,   CommutativeRing
    ,   IntegralDomain
    )
    where

import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Multiplicative
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

-- | Distributive  laws
--
-- > a * (b + c) == a * b + a * c
-- > (a * b) * c == a * c + b * c
class (Addition a, Multiplication a) =>
    Distributive a

instance Distributive P.Double

instance Distributive P.Float

instance (Distributive a, AbelianGroup (Sum a)) => Distributive (Complex a)

instance Distributive P.Int
instance Distributive P.Integer
instance Distributive Natural
instance Distributive Int8
instance Distributive Int16
instance Distributive Int32
instance Distributive Int64
instance Distributive Word
instance Distributive Word8
instance Distributive Word16
instance Distributive Word32
instance Distributive Word64

-- | Semiring
-- FIXME: rule zero' = zero. Is this somehow expressible in haskell?
class (Associative (Sum a), Unital (Sum a), Associative (Product a), Unital (Product a), Distributive a) =>
    Semiring a where
instance (Associative (Sum a), Unital (Sum a), Associative (Product a), Unital (Product a), Distributive a) =>
    Semiring a

-- | Ring
class (Semiring a, AbelianGroup (Sum a)) =>
    Ring a
instance (Semiring a, AbelianGroup (Sum a)) =>
    Ring a

-- | Ring with a commutative Multiplication
class (Ring a, Commutative (Product a)) =>
    CommutativeRing a
instance (Ring a, Commutative (Product a)) =>
    CommutativeRing a

-- | generalization of ring of integers
--  rules:
--  product of any two nonzero elements is nonzero, also
--  if a ≠ 0, an equality ab = ac implies b = c.
--  this essentially is a generalization of division and a fundamental step towards a Field
class (CommutativeRing a, Invertible (Product a)) =>
    IntegralDomain a

instance IntegralDomain P.Double

instance IntegralDomain P.Float

instance (IntegralDomain a) => IntegralDomain (Complex a)
