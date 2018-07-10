{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The Group hirarchy
module NumHask.Algebra.Abstract.Additive
      ( zero
      , Sum(..)
      , coerceFA
      , coerceFA'
      , coerceTA
      , coerceTA'
      , Addition(..)
      , (+)
      , (-)
      , negate
      , plus
      )
where

import qualified Prelude                       as P
import qualified GHC.Generics                  as P
import           NumHask.Algebra.Abstract.Group
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

newtype Sum a = Sum a
      deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Generic, P.Generic1, P.Functor)

class (Associative (Sum a), Commutative (Sum a), Unital (Sum a)) => Addition a where
      sum :: (P.Foldable f) => f a -> a
      sum = P.foldr (+) zero
instance (Associative (Sum a), Commutative (Sum a), Unital (Sum a)) => Addition a

plus :: Magma (Sum a) => a -> a -> a
plus = coerceFA magma

infixl 6 +
(+) :: Addition a => a -> a -> a
(+) = coerceFA magma

infixl 6 -
(-) :: (Invertible (Sum a), Addition a) => a -> a -> a
(-) a b = a + negate b

zero :: Unital (Sum a) => a
zero = let (Sum a) = unit in a


negate :: Invertible (Sum a) => a -> a
negate = coerceFA' inv

--less flexible coerces for better inference in instances
coerceFA :: (Sum a -> Sum a -> Sum a) -> a -> a -> a
coerceFA f a b = let (Sum res) = f (Sum a) (Sum b) in res

coerceFA' :: (Sum a -> Sum a) -> a -> a
coerceFA' f a = let (Sum res) = f (Sum a) in res

coerceTA :: (a -> a -> a) -> (Sum a -> Sum a -> Sum a)
coerceTA f (Sum a) (Sum b) = Sum P.$ f a b

coerceTA' :: (a -> a) -> (Sum a -> Sum a)
coerceTA' f (Sum a) = Sum P.$ f a

--instances

--magma
instance Magma (Sum P.Double) where
      magma = coerceTA (P.+)

instance Magma (Sum P.Float) where
      magma = coerceTA (P.+)

instance Magma (Sum P.Int) where
      magma = coerceTA (P.+)

instance Magma (Sum P.Integer) where
      magma = coerceTA (P.+)

instance Magma (Sum P.Bool) where
      magma = coerceTA (P.||)

instance Magma (Sum a) => Magma (Sum (Complex a)) where
      (Sum (rx :+ ix)) `magma` (Sum (ry :+ iy)) = Sum res
            where
                  res = (rx `plus` ry) :+ (ix `plus` iy)

instance Magma (Sum Natural) where
      magma = coerceTA (P.+)

instance Magma (Sum Int8) where
      magma = coerceTA (P.+)

instance Magma (Sum Int16) where
      magma = coerceTA (P.+)

instance Magma (Sum Int32) where
      magma = coerceTA (P.+)

instance Magma (Sum Int64) where
      magma = coerceTA (P.+)

instance Magma (Sum Word) where
      magma = coerceTA (P.+)

instance Magma (Sum Word8) where
      magma = coerceTA (P.+)

instance Magma (Sum Word16) where
      magma = coerceTA (P.+)

instance Magma (Sum Word32) where
      magma = coerceTA (P.+)

instance Magma (Sum Word64) where
      magma = coerceTA (P.+)

--Unital
instance Unital (Sum P.Double) where
      unit = coerce (0 :: P.Double)

instance Unital (Sum P.Float) where
      unit = coerce (0 :: P.Float)

instance Unital (Sum P.Int) where
      unit = coerce (0 :: P.Int)

instance Unital (Sum P.Integer) where
      unit = coerce (0 :: P.Integer)

instance Unital (Sum P.Bool) where
      unit = coerce P.False

instance Unital (Sum a) => Unital (Sum (Complex a)) where
      unit = Sum P.$ elem :+ elem
            where
                  elem = let (Sum x) = unit in x

instance Unital (Sum Natural) where
      unit = coerce (0 :: Natural)

instance Unital (Sum Int8) where
      unit = coerce (0 :: Int8)

instance Unital (Sum Int16) where
      unit = coerce (0 :: Int16)

instance Unital (Sum Int32) where
      unit = coerce (0 :: Int32)

instance Unital (Sum Int64) where
      unit = coerce (0 :: Int64)

instance Unital (Sum Word) where
      unit = coerce (0 :: Word)

instance Unital (Sum Word8) where
      unit = coerce (0 :: Word8)

instance Unital (Sum Word16) where
      unit = coerce (0 :: Word16)

instance Unital (Sum Word32) where
      unit = coerce (0 :: Word32)

instance Unital (Sum Word64) where
      unit = coerce (0 :: Word64)

-- semigroup
instance Associative (Sum P.Double)

instance Associative (Sum P.Float)

instance Associative (Sum P.Int)

instance Associative (Sum P.Integer)

instance Associative (Sum P.Bool)

instance Associative (Sum a) => Associative (Sum (Complex a))

instance Associative (Sum Natural)

instance Associative (Sum Int8)

instance Associative (Sum Int16)

instance Associative (Sum Int32)

instance Associative (Sum Int64)

instance Associative (Sum Word)

instance Associative (Sum Word8)

instance Associative (Sum Word16)

instance Associative (Sum Word32)

instance Associative (Sum Word64)

---commutative magma
instance Commutative (Sum P.Double)

instance Commutative (Sum P.Float)

instance Commutative (Sum P.Int)

instance Commutative (Sum P.Integer)

instance Commutative (Sum P.Bool)

instance Commutative (Sum a) => Commutative (Sum (Complex a))

instance Commutative (Sum Natural)

instance Commutative (Sum Int8)

instance Commutative (Sum Int16)

instance Commutative (Sum Int32)

instance Commutative (Sum Int64)

instance Commutative (Sum Word)

instance Commutative (Sum Word8)

instance Commutative (Sum Word16)

instance Commutative (Sum Word32)

instance Commutative (Sum Word64)

--- invertible
instance Invertible (Sum P.Double) where
  inv = coerceTA' P.negate

instance Invertible (Sum P.Float) where
  inv = coerceTA' P.negate

instance Invertible (Sum P.Int) where
  inv = coerceTA' P.negate

instance Invertible (Sum P.Integer) where
  inv = coerceTA' P.negate

instance Invertible (Sum P.Bool) where
  inv = coerceTA' P.not

instance Invertible (Sum a) => Invertible (Sum (Complex a)) where
  inv (Sum (rx :+ ix)) = Sum P.$ (doInv rx :+ doInv ix)
      where
            doInv = coerceFA' inv

instance Invertible (Sum Int8) where
  inv = coerceTA' P.negate

instance Invertible (Sum Int16) where
  inv = coerceTA' P.negate

instance Invertible (Sum Int32) where
  inv = coerceTA' P.negate

instance Invertible (Sum Int64) where
  inv = coerceTA' P.negate

instance Invertible (Sum Word) where
  inv = coerceTA' P.negate

instance Invertible (Sum Word8) where
  inv = coerceTA' P.negate

instance Invertible (Sum Word16) where
  inv = coerceTA' P.negate

instance Invertible (Sum Word32) where
  inv = coerceTA' P.negate

instance Invertible (Sum Word64) where
  inv = coerceTA' P.negate

--- idempotent
instance Idempotent (Sum P.Bool)
