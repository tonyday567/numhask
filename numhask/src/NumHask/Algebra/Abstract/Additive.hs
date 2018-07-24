{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Additive
module NumHask.Algebra.Abstract.Additive
  ( Sum(..)
  , Additive(..)
  , Subtractive(..)
  )
where

import Data.Coerce
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Natural (Natural(..))
import NumHask.Algebra.Abstract.Group
import qualified GHC.Generics as P
import qualified Prelude as P

newtype Sum a = Sum a
  deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Generic, P.Generic1,
            P.Functor)

class (Associative (Sum a), Commutative (Sum a), Unital (Sum a))
  => Additive a where
  sum :: (P.Foldable f) => f a -> a
  sum = P.foldr (+) zero

  infixl 6 +
  (+) :: a -> a -> a
  (+) = coerceFA magma

  zero :: a
  zero = let (Sum a) = unit in a

instance (Associative (Sum a), Commutative (Sum a), Unital (Sum a))
  => Additive a

class (AbelianGroup (Sum a))
  => Subtractive a where
  negate :: a -> a
  negate = coerceFA' inv

  infixl 6 -
  (-) :: a -> a -> a
  (-) a b = a + negate b

instance (AbelianGroup (Sum a)) => Subtractive a

--less flexible coerces for better inference in instances
coerceFA :: (Sum a -> Sum a -> Sum a) -> a -> a -> a
coerceFA f a b = let (Sum res) = f (Sum a) (Sum b) in res

coerceFA' :: (Sum a -> Sum a) -> a -> a
coerceFA' f a = let (Sum res) = f (Sum a) in res

coerceTA :: (a -> a -> a) -> (Sum a -> Sum a -> Sum a)
coerceTA f (Sum a) (Sum b) = Sum P.$ f a b

coerceTA' :: (a -> a) -> (Sum a -> Sum a)
coerceTA' f (Sum a) = Sum P.$ f a

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

instance Magma (Sum b) => Magma (Sum (a -> b)) where
  (Sum f) `magma` (Sum f') = Sum P.$ \a -> f a `cmagma` f' a 
    where
      cmagma = coerceFA magma

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

instance Unital (Sum b) => Unital (Sum (a -> b)) where
  unit = Sum P.$ \_ -> coerce @(Sum b) @b unit

instance Associative (Sum P.Double)

instance Associative (Sum P.Float)

instance Associative (Sum P.Int)

instance Associative (Sum P.Integer)

instance Associative (Sum P.Bool)

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

instance Associative (Sum b) => Associative (Sum (a -> b))

---commutative magma
instance Commutative (Sum P.Double)

instance Commutative (Sum P.Float)

instance Commutative (Sum P.Int)

instance Commutative (Sum P.Integer)

instance Commutative (Sum P.Bool)

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

instance Commutative (Sum b) => Commutative (Sum (a -> b))

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

instance Invertible (Sum b) => Invertible (Sum (a -> b)) where
  inv (Sum f) = Sum P.$ \a -> coerceFA' inv (f a)

instance Idempotent (Sum P.Bool)

instance Idempotent (Sum b) => Idempotent (Sum (a -> b))
