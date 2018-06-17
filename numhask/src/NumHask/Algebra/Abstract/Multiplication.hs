{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | The Group hirarchy
module NumHask.Algebra.Abstract.Multiplication
      ( one
      , recip
      , Mult(..)
      , coerceFM
      , coerceFM'
      , coerceTM
      , coerceTM'
      , Multiplication(..)
      , (*)
      , zero'
      , (/)
      , times
      )
      where

import           NumHask.Algebra.Abstract.Group
import           NumHask.Algebra.Abstract.Addition
import qualified Prelude                       as P
import qualified GHC.Generics                  as P
import Data.Coerce
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

newtype Mult a = Mult a
    deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Generic, P.Generic1, P.Functor)

one :: Unital (Mult a) => a
one = let (Mult a) = unit in a

recip :: Invertible (Mult a) => a -> a
recip = coerceFM' inv

class (Absorbing (Mult a), Monoid (Mult a)) => Multiplication a where
    product :: (P.Foldable f) => f a -> a
    product = P.foldr (*) one
instance (Absorbing (Mult a), Monoid (Mult a)) => Multiplication a

times :: Magma (Mult a) => a -> a -> a
times = coerceFM comb

infixl 6 *
(*) :: Multiplication a => a -> a -> a
(*) = coerceFM comb

zero' :: Multiplication a => a
zero' = let (Mult a) = absorb in a

infixl 6 /
(/) :: (Invertible (Mult a), Multiplication a) => a -> a -> a
(/) a b = a * recip b

--less flexible coerces for better inference in instances
coerceFM :: (Mult a -> Mult a -> Mult a) -> a -> a -> a
coerceFM f a b = let (Mult res) = f (Mult a) (Mult b) in res

coerceFM' :: (Mult a -> Mult a) -> a -> a
coerceFM' f a = let (Mult res) = f (Mult a) in res

coerceTM :: (a -> a -> a) -> (Mult a -> Mult a -> Mult a)
coerceTM f (Mult a) (Mult b) = Mult P.$ f a b

coerceTM' :: (a -> a) -> (Mult a -> Mult a)
coerceTM' f (Mult a) = Mult P.$ f a

-- magma
instance Magma (Mult P.Double) where
    comb = coerceTM (P.*)

instance Magma (Mult P.Float) where
    comb = coerceTM (P.*)

instance Magma (Mult P.Int) where
    comb = coerceTM (P.*)

instance Magma (Mult P.Integer) where
    comb = coerceTM (P.*)

instance Magma (Mult P.Bool) where
    comb = coerceTM (P.&&)

instance (Magma (Mult a), Addition a, Invertible (Add a)) =>
    Magma (Mult (Complex a)) where
    (Mult (rx :+ ix)) `comb` (Mult (ry :+ iy)) =
        Mult P.$ (rx `times` ry - ix `times` iy) :+ (ix `times` ry + iy `times` rx)

instance Magma (Mult Natural) where
    comb = coerceTM (P.*)

instance Magma (Mult Int8) where
    comb = coerceTM (P.*)

instance Magma (Mult Int16) where
    comb = coerceTM (P.*)

instance Magma (Mult Int32) where
    comb = coerceTM (P.*)

instance Magma (Mult Int64) where
    comb = coerceTM (P.*)

instance Magma (Mult Word) where
    comb = coerceTM (P.*)

instance Magma (Mult Word8) where
    comb = coerceTM (P.*)

instance Magma (Mult Word16) where
    comb = coerceTM (P.*)

instance Magma (Mult Word32) where
    comb = coerceTM (P.*)

instance Magma (Mult Word64) where
    comb = coerceTM (P.*)

--- Unital

instance Unital (Mult P.Double) where
    unit = coerce (1 :: P.Double)
  
instance Unital (Mult P.Float) where
    unit = coerce (1 :: P.Float)

instance Unital (Mult P.Int) where
    unit = coerce (1 :: P.Int)

instance Unital (Mult P.Integer) where
    unit = coerce (1 :: P.Integer)

instance Unital (Mult P.Bool) where
    unit = coerce P.True

instance (Unital (Mult a), AbelianGroup (Add a)) =>
    Unital (Mult (Complex a)) where
    unit = Mult (one :+ zero)

instance Unital (Mult Natural) where
    unit = coerce (1 :: Natural)

instance Unital (Mult Int8) where
    unit = coerce (1 :: Int8)

instance Unital (Mult Int16) where
    unit = coerce (1 :: Int16)

instance Unital (Mult Int32) where
    unit = coerce (1 :: Int32)

instance Unital (Mult Int64) where
    unit = coerce (1 :: Int64)

instance Unital (Mult Word) where
    unit = coerce (1 :: Word)

instance Unital (Mult Word8) where
    unit = coerce (1 :: Word8)

instance Unital (Mult Word16) where
    unit = coerce (1 :: Word16)

instance Unital (Mult Word32) where
    unit = coerce (1 :: Word32)

instance Unital (Mult Word64) where
    unit = coerce (1 :: Word64)

--- semigroup
instance Semigroup (Mult P.Double)

instance Semigroup (Mult P.Float)

instance Semigroup (Mult P.Int)

instance Semigroup (Mult P.Integer)

instance Semigroup (Mult P.Bool)

instance (AbelianGroup (Add a), Semigroup (Mult a)) =>
    Semigroup (Mult (Complex a))

instance Semigroup (Mult Natural)

instance Semigroup (Mult Int8)

instance Semigroup (Mult Int16)

instance Semigroup (Mult Int32)

instance Semigroup (Mult Int64)

instance Semigroup (Mult Word)

instance Semigroup (Mult Word8)

instance Semigroup (Mult Word16)

instance Semigroup (Mult Word32)

instance Semigroup (Mult Word64)

--- commutative
instance Commutative (Mult P.Double)

instance Commutative (Mult P.Float)

instance Commutative (Mult P.Int)

instance Commutative (Mult P.Integer)

instance Commutative (Mult P.Bool)

instance (AbelianGroup (Add a), Commutative (Mult a)) =>
    Commutative (Mult (Complex a))

instance Commutative (Mult Natural)

instance Commutative (Mult Int8)

instance Commutative (Mult Int16)

instance Commutative (Mult Int32)

instance Commutative (Mult Int64)

instance Commutative (Mult Word)

instance Commutative (Mult Word8)

instance Commutative (Mult Word16)

instance Commutative (Mult Word32)

instance Commutative (Mult Word64)

---invertible
instance Invertible (Mult P.Double) where
    inv = coerceTM' P.recip
  
instance Invertible (Mult P.Float) where
    inv = coerceTM' P.recip

instance (AbelianGroup (Add a), Invertible (Mult a)) =>
    Invertible (Mult (Complex a)) where
    inv (Mult (rx :+ ix)) = Mult ((rx `times` d) :+ (neg ix `times` d))
        where
            d = recip ((rx `times` rx) + (ix `times` ix))

---idempotent
instance Idempotent (Mult P.Bool)