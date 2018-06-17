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
-- | The Group hirarchy
module NumHask.Algebra.Abstract.Addition
      ( zero
      , Add(..)
      , coerceFA
      , coerceFA'
      , coerceTA
      , coerceTA'
      , Addition(..)
      , (+)
      , (-)
      , neg
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

newtype Add a = Add a
      deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Generic, P.Generic1, P.Functor)

class (Semigroup (Add a), Commutative (Add a)) => Addition a where
      sum :: (P.Foldable f, Unital (Add a)) => f a -> a
      sum = P.foldr (+) zero
instance (Semigroup (Add a), Commutative (Add a)) => Addition a

plus :: Magma (Add a) => a -> a -> a
plus = coerceFA comb

infixl 6 +
(+) :: Addition a => a -> a -> a
(+) = coerceFA comb

infixl 6 -
(-) :: (Invertible (Add a), Addition a) => a -> a -> a
(-) a b = a + neg b

zero :: Unital (Add a) => a
zero = let (Add a) = unit in a


neg :: Invertible (Add a) => a -> a
neg = coerceFA' inv

--less flexible coerces for better inference in instances
coerceFA :: (Add a -> Add a -> Add a) -> a -> a -> a
coerceFA f a b = let (Add res) = f (Add a) (Add b) in res

coerceFA' :: (Add a -> Add a) -> a -> a
coerceFA' f a = let (Add res) = f (Add a) in res

coerceTA :: (a -> a -> a) -> (Add a -> Add a -> Add a)
coerceTA f (Add a) (Add b) = Add P.$ f a b

coerceTA' :: (a -> a) -> (Add a -> Add a)
coerceTA' f (Add a) = Add P.$ f a

--instances

--magma
instance Magma (Add P.Double) where
      comb = coerceTA (P.+)

instance Magma (Add P.Float) where
      comb = coerceTA (P.+)

instance Magma (Add P.Int) where
      comb = coerceTA (P.+)

instance Magma (Add P.Integer) where
      comb = coerceTA (P.+)

instance Magma (Add P.Bool) where
      comb = coerceTA (P.||)

instance Magma (Add a) => Magma (Add (Complex a)) where
      (Add (rx :+ ix)) `comb` (Add (ry :+ iy)) = Add res
            where
                  res = (rx `plus` ry) :+ (ix `plus` iy)

instance Magma (Add Natural) where
      comb = coerceTA (P.+)

instance Magma (Add Int8) where
      comb = coerceTA (P.+)

instance Magma (Add Int16) where
      comb = coerceTA (P.+)

instance Magma (Add Int32) where
      comb = coerceTA (P.+)

instance Magma (Add Int64) where
      comb = coerceTA (P.+)

instance Magma (Add Word) where
      comb = coerceTA (P.+)

instance Magma (Add Word8) where
      comb = coerceTA (P.+)

instance Magma (Add Word16) where
      comb = coerceTA (P.+)

instance Magma (Add Word32) where
      comb = coerceTA (P.+)

instance Magma (Add Word64) where
      comb = coerceTA (P.+)

--Unital
instance Unital (Add P.Double) where
      unit = coerce (0 :: P.Double)
      
instance Unital (Add P.Float) where
      unit = coerce (0 :: P.Float)
      
instance Unital (Add P.Int) where
      unit = coerce (0 :: P.Int)
      
instance Unital (Add P.Integer) where
      unit = coerce (0 :: P.Integer)
      
instance Unital (Add P.Bool) where
      unit = coerce P.False
      
instance Unital (Add a) => Unital (Add (Complex a)) where
      unit = Add P.$ (elem) :+ (elem)
            where
                  elem = let (Add x) = unit in x
      
instance Unital (Add Natural) where
      unit = coerce (0 :: Natural)
      
instance Unital (Add Int8) where
      unit = coerce (0 :: Int8)
      
instance Unital (Add Int16) where
      unit = coerce (0 :: Int16)
      
instance Unital (Add Int32) where
      unit = coerce (0 :: Int32)
      
instance Unital (Add Int64) where
      unit = coerce (0 :: Int64)
      
instance Unital (Add Word) where
      unit = coerce (0 :: Word)
      
instance Unital (Add Word8) where
      unit = coerce (0 :: Word8)
      
instance Unital (Add Word16) where
      unit = coerce (0 :: Word16)
      
instance Unital (Add Word32) where
      unit = coerce (0 :: Word32)
      
instance Unital (Add Word64) where
      unit = coerce (0 :: Word64)

-- semigroup
instance Semigroup (Add P.Double)

instance Semigroup (Add P.Float)

instance Semigroup (Add P.Int)

instance Semigroup (Add P.Integer)

instance Semigroup (Add P.Bool)

instance Semigroup (Add a) => Semigroup (Add (Complex a))

instance Semigroup (Add Natural)

instance Semigroup (Add Int8)

instance Semigroup (Add Int16)

instance Semigroup (Add Int32)

instance Semigroup (Add Int64)

instance Semigroup (Add Word)

instance Semigroup (Add Word8)

instance Semigroup (Add Word16)

instance Semigroup (Add Word32)

instance Semigroup (Add Word64)

---commutative magma
instance Commutative (Add P.Double)

instance Commutative (Add P.Float)

instance Commutative (Add P.Int)

instance Commutative (Add P.Integer)

instance Commutative (Add P.Bool)

instance Commutative (Add a) => Commutative (Add (Complex a))

instance Commutative (Add Natural)

instance Commutative (Add Int8)

instance Commutative (Add Int16)

instance Commutative (Add Int32)

instance Commutative (Add Int64)

instance Commutative (Add Word)

instance Commutative (Add Word8)

instance Commutative (Add Word16)

instance Commutative (Add Word32)

instance Commutative (Add Word64)

--- invertible
instance Invertible (Add P.Double) where
  inv = coerceTA' P.negate

instance Invertible (Add P.Float) where
  inv = coerceTA' P.negate

instance Invertible (Add P.Int) where
  inv = coerceTA' P.negate

instance Invertible (Add P.Integer) where
  inv = coerceTA' P.negate

instance Invertible (Add P.Bool) where
  inv = coerceTA' P.not

instance Invertible (Add a) => Invertible (Add (Complex a)) where
  inv (Add (rx :+ ix)) = Add P.$ (doInv rx :+ doInv ix)
      where
            doInv = coerceFA' inv

instance Invertible (Add Int8) where
  inv = coerceTA' P.negate

instance Invertible (Add Int16) where
  inv = coerceTA' P.negate

instance Invertible (Add Int32) where
  inv = coerceTA' P.negate

instance Invertible (Add Int64) where
  inv = coerceTA' P.negate

instance Invertible (Add Word) where
  inv = coerceTA' P.negate

instance Invertible (Add Word8) where
  inv = coerceTA' P.negate

instance Invertible (Add Word16) where
  inv = coerceTA' P.negate

instance Invertible (Add Word32) where
  inv = coerceTA' P.negate

instance Invertible (Add Word64) where
  inv = coerceTA' P.negate

--- idempotent
instance Idempotent (Add P.Bool)