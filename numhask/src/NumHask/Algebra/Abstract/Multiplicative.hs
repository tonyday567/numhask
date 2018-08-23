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

-- | Multiplicative
module NumHask.Algebra.Abstract.Multiplicative
  ( Multiplicative(..)
  , product
  , Divisive(..)
  )
where

import Data.Coerce
-- import Data.Int (Int8, Int16, Int32, Int64)
-- import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Natural (Natural(..))
import NumHask.Algebra.Abstract.Group
import qualified GHC.Generics as P
import qualified Prelude as P

class Multiplicative a where

  infixl 7 *
  (*) :: a -> a -> a

  one :: a

product :: (Multiplicative a, P.Foldable f) => f a -> a
product = P.foldr (*) one

class (Multiplicative a) => Divisive a where
  recip :: a -> a

  infixl 7 /
  (/) :: a -> a -> a
  (/) a b = a * recip b

instance Multiplicative P.Double where
  (*) = (P.*)
  one = 1.0

instance Divisive P.Double where
  recip = P.recip

instance Multiplicative P.Float where
  (*) = (P.*)
  one = 1.0

instance Divisive P.Float where
  recip = P.recip

instance Multiplicative P.Int where
  (*) = (P.*)
  one = 1

instance Multiplicative P.Integer where
  (*) = (P.*)
  one = 1

{-
instance Magma (Product P.Double) where
  magma = coerceTM (P.*)

instance Magma (Product P.Float) where
  magma = coerceTM (P.*)

instance Magma (Product P.Int) where
  magma = coerceTM (P.*)

instance Magma (Product P.Integer) where
  magma = coerceTM (P.*)

instance Magma (Product P.Bool) where
  magma = coerceTM (P.&&)

instance Magma (Product Natural) where
  magma = coerceTM (P.*)

instance Magma (Product Int8) where
  magma = coerceTM (P.*)

instance Magma (Product Int16) where
  magma = coerceTM (P.*)

instance Magma (Product Int32) where
  magma = coerceTM (P.*)

instance Magma (Product Int64) where
  magma = coerceTM (P.*)

instance Magma (Product Word) where
  magma = coerceTM (P.*)

instance Magma (Product Word8) where
  magma = coerceTM (P.*)

instance Magma (Product Word16) where
  magma = coerceTM (P.*)

instance Magma (Product Word32) where
  magma = coerceTM (P.*)

instance Magma (Product Word64) where
  magma = coerceTM (P.*)

instance Magma (Product b) => Magma (Product (a -> b)) where
  (Product f) `magma` (Product f') = Product P.$ \a -> f a `cmagma` f' a 
    where
      cmagma = coerceFM magma

instance Unital (Product P.Double) where
  unit = coerce (1 :: P.Double)

instance Unital (Product P.Float) where
  unit = coerce (1 :: P.Float)

instance Unital (Product P.Int) where
  unit = coerce (1 :: P.Int)

instance Unital (Product P.Integer) where
  unit = coerce (1 :: P.Integer)

instance Unital (Product P.Bool) where
  unit = coerce P.True

instance Unital (Product Natural) where
  unit = coerce (1 :: Natural)

instance Unital (Product Int8) where
  unit = coerce (1 :: Int8)

instance Unital (Product Int16) where
  unit = coerce (1 :: Int16)

instance Unital (Product Int32) where
  unit = coerce (1 :: Int32)

instance Unital (Product Int64) where
  unit = coerce (1 :: Int64)

instance Unital (Product Word) where
  unit = coerce (1 :: Word)

instance Unital (Product Word8) where
  unit = coerce (1 :: Word8)

instance Unital (Product Word16) where
  unit = coerce (1 :: Word16)

instance Unital (Product Word32) where
  unit = coerce (1 :: Word32)

instance Unital (Product Word64) where
  unit = coerce (1 :: Word64)

instance Unital (Product b) => Unital (Product (a -> b)) where
  unit = Product P.$ \_ -> coerce @(Product b) @b unit

instance Associative (Product P.Double)

instance Associative (Product P.Float)

instance Associative (Product P.Int)

instance Associative (Product P.Integer)

instance Associative (Product P.Bool)

instance Associative (Product Natural)

instance Associative (Product Int8)

instance Associative (Product Int16)

instance Associative (Product Int32)

instance Associative (Product Int64)

instance Associative (Product Word)

instance Associative (Product Word8)

instance Associative (Product Word16)

instance Associative (Product Word32)

instance Associative (Product Word64)

instance Associative (Product b) => Associative (Product (a -> b))

instance Commutative (Product P.Double)

instance Commutative (Product P.Float)

instance Commutative (Product P.Int)

instance Commutative (Product P.Integer)

instance Commutative (Product P.Bool)

instance Commutative (Product Natural)

instance Commutative (Product Int8)

instance Commutative (Product Int16)

instance Commutative (Product Int32)

instance Commutative (Product Int64)

instance Commutative (Product Word)

instance Commutative (Product Word8)

instance Commutative (Product Word16)

instance Commutative (Product Word32)

instance Commutative (Product Word64)

instance Commutative (Product b) => Commutative (Product (a -> b))

instance Invertible (Product P.Double) where
  inv = coerceTM' P.recip

instance Invertible (Product P.Float) where
  inv = coerceTM' P.recip

instance Invertible (Product b) => Invertible (Product (a -> b)) where
  inv (Product f) = Product P.$ \a -> coerceFM' inv (f a)

instance Idempotent (Product P.Bool)

instance Idempotent (Product b) => Idempotent (Product (a -> b))

instance Absorbing (Product P.Double) where
  absorb = coerce (0 :: P.Double)

instance Absorbing (Product P.Float) where
  absorb = coerce (0 :: P.Float)

instance Absorbing (Product P.Int) where
  absorb = coerce (0 :: P.Int)

instance Absorbing (Product P.Integer) where
  absorb = coerce (0 :: P.Integer)

instance Absorbing (Product P.Bool) where
  absorb = coerce P.False

instance Absorbing (Product Natural) where
  absorb = coerce (0 :: Natural)

instance Absorbing (Product Int8) where
  absorb = coerce (0 :: Int8)

instance Absorbing (Product Int16) where
  absorb = coerce (0 :: Int16)

instance Absorbing (Product Int32) where
  absorb = coerce (0 :: Int32)

instance Absorbing (Product Int64) where
  absorb = coerce (0 :: Int64)

instance Absorbing (Product Word) where
  absorb = coerce (0 :: Word)

instance Absorbing (Product Word8) where
  absorb = coerce (0 :: Word8)

instance Absorbing (Product Word16) where
  absorb = coerce (0 :: Word16)

instance Absorbing (Product Word32) where
  absorb = coerce (0 :: Word32)

instance Absorbing (Product Word64) where
  absorb = coerce (0 :: Word64)

instance Absorbing (Product b) => Absorbing (Product (a -> b)) where
  absorb = Product P.$ \_ -> coerce @(Product b) @b absorb

-}
