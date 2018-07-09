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
module NumHask.Algebra.Abstract.Multiplicative
    ( one
    , recip
    , Product(..)
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
import           NumHask.Algebra.Abstract.Additive
import qualified Prelude                       as P
import qualified GHC.Generics                  as P
import           Data.Coerce
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

newtype Product a = Product a
    deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Generic, P.Generic1, P.Functor)

one :: Unital (Product a) => a
one = let (Product a) = unit in a

recip :: Invertible (Product a) => a -> a
recip = coerceFM' inv

class (Absorbing (Product a), Monoid (Product a)) => Multiplication a where
    product :: (P.Foldable f) => f a -> a
    product = P.foldr (*) one
instance (Absorbing (Product a), Monoid (Product a)) => Multiplication a

times :: Magma (Product a) => a -> a -> a
times = coerceFM magma

infixl 6 *
(*) :: Multiplication a => a -> a -> a
(*) = coerceFM magma

zero' :: Multiplication a => a
zero' = let (Product a) = absorb in a

infixl 6 /
(/) :: (Invertible (Product a), Multiplication a) => a -> a -> a
(/) a b = a * recip b

--less flexible coerces for better inference in instances
coerceFM :: (Product a -> Product a -> Product a) -> a -> a -> a
coerceFM f a b = let (Product res) = f (Product a) (Product b) in res

coerceFM' :: (Product a -> Product a) -> a -> a
coerceFM' f a = let (Product res) = f (Product a) in res

coerceTM :: (a -> a -> a) -> (Product a -> Product a -> Product a)
coerceTM f (Product a) (Product b) = Product P.$ f a b

coerceTM' :: (a -> a) -> (Product a -> Product a)
coerceTM' f (Product a) = Product P.$ f a

-- magma
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

instance (Magma (Product a), Addition a, Invertible (Sum a)) =>
    Magma (Product (Complex a)) where
    (Product (rx :+ ix)) `magma` (Product (ry :+ iy)) =
        Product P.$ (rx `times` ry - ix `times` iy) :+ (ix `times` ry + iy `times` rx)

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

--- Unital

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

instance (Unital (Product a), AbelianGroup (Sum a)) =>
    Unital (Product (Complex a)) where
    unit = Product (one :+ zero)

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

--- semigroup
instance Semigroup (Product P.Double)

instance Semigroup (Product P.Float)

instance Semigroup (Product P.Int)

instance Semigroup (Product P.Integer)

instance Semigroup (Product P.Bool)

instance (AbelianGroup (Sum a), Semigroup (Product a)) =>
    Semigroup (Product (Complex a))

instance Semigroup (Product Natural)

instance Semigroup (Product Int8)

instance Semigroup (Product Int16)

instance Semigroup (Product Int32)

instance Semigroup (Product Int64)

instance Semigroup (Product Word)

instance Semigroup (Product Word8)

instance Semigroup (Product Word16)

instance Semigroup (Product Word32)

instance Semigroup (Product Word64)

--- commutative
instance Commutative (Product P.Double)

instance Commutative (Product P.Float)

instance Commutative (Product P.Int)

instance Commutative (Product P.Integer)

instance Commutative (Product P.Bool)

instance (AbelianGroup (Sum a), Commutative (Product a)) =>
    Commutative (Product (Complex a))

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

---invertible
instance Invertible (Product P.Double) where
    inv = coerceTM' P.recip

instance Invertible (Product P.Float) where
    inv = coerceTM' P.recip

instance (AbelianGroup (Sum a), Invertible (Product a)) =>
    Invertible (Product (Complex a)) where
    inv (Product (rx :+ ix)) = Product ((rx `times` d) :+ (negate ix `times` d))
        where
            d = recip ((rx `times` rx) + (ix `times` ix))

---idempotent
instance Idempotent (Product P.Bool)

--absorbing
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

instance (Absorbing (Product a), Addition a, Invertible (Sum a)) => Absorbing (Product (Complex a)) where
    absorb = Product P.$ (elem) :+ (elem)
        where
            elem = let (Product x) = absorb in x

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
