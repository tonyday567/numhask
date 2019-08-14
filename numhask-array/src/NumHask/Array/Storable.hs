{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module NumHask.Array.Storable where

import GHC.Exts (IsList(..))
import GHC.Show (Show(..))
-- import NumHask.Error (impossible)
-- import NumHask.Array.Constraints
--  (Fold, HeadModule, TailModule, IsValidConcat, Concatenate, Transpose, Squeeze)
import NumHask.Prelude as P
import qualified Prelude
-- import NumHask.Shape (HasShape(..))
-- import Numeric.Dimensions as D
-- import qualified Data.Singletons.Prelude as S
import qualified Data.Vector.Storable as V
-- import qualified Data.Vector.Generic.Sized as S
-- import qualified Data.Vector.Generic.Sized.Internal as SI
-- import qualified Data.Vector.Generic.Base as B
-- import Data.Finite
-- import GHC.TypeLits
-- concrete vector based on vector-sized
-- import qualified Prelude (fromIntegral)
-- import qualified NumHask.Vector as NH

newtype Array s a = Array { unArray :: V.Vector a} deriving (Eq, Show, Ord, NFData, Generic)

newtype Shape (s :: [Nat]) = Shape { shapeVal :: [Int] } deriving Show

class HasShape s where
  toShape :: Shape s

instance HasShape '[] where
  toShape = Shape []

instance (KnownNat n, HasShape s) => HasShape (n:s) where
  toShape = Shape $ fromInteger (natVal (Proxy :: Proxy n)) : shapeVal (toShape :: Shape s)

shape :: forall a s. (HasShape s) => Array s a -> [Int]
shape _ = shapeVal $ toShape @s
{-# inline shape #-}

size :: [Int] -> Int
size [] = zero
size [x] = x
size xs = P.product xs
{-# inline size #-}

-- | convert from n-dim shape index to a flat index
--
-- >>> ind [2,3,4] [1,1,1]
-- 17
ind :: [Int] -> [Int] -> Int
ind [] _ = zero
ind _ [x'] = x'
ind ns xs = sum $ P.zipWith (*) xs (Prelude.scanr1 (*) ns)
{-# inline ind #-}

-- | convert from a flat index to a shape index
--
-- >>> unind [2,3,4] 17
-- [1,1,1]
unind :: [Int] -> Int -> [Int]
unind [] _ = []
unind [_] x' = [x']
unind [_,y] x' = let (i,j) = divMod x' y in [i,j]
unind ns x =
  fst $
  foldr
    (\a (acc, r) ->
       let (d, m) = divMod r a
       in (m : acc, d))
    ([], x)
    ns
{-# inline unind #-}

generate :: forall a s. (Storable a, HasShape s) => ([Int] -> a) -> Array s a
generate f =
  Array . V.generate (size s) $ (f . unind s)
    where
      s = shapeVal $ toShape @s

index :: forall a s. (Storable a, HasShape s) => Array s a -> [Int] -> a
index (Array a) i = V.unsafeIndex a (ind s i)
    where
      s = shapeVal (toShape @s)

instance
  ( Additive a
  , HasShape s
  , Storable a
  )
  => Additive (Array s a) where
  (+) (Array x) (Array y) = Array $ V.zipWith (+) x y
  zero = Array $ V.generate p (const zero)
    where
      p = size $ shapeVal (toShape @s)

instance
  ( Multiplicative a
  , HasShape s
  , Storable a
  )
  => Multiplicative (Array s a) where
  (*) (Array x) (Array y) = Array $ V.zipWith (*) x y
  one = Array $ V.generate p (const one)
    where
      p = size $ shapeVal (toShape @s)

type instance Actor (Array s a) = a

instance
  ( HasShape s
  , P.Distributive a
  , CommutativeRing a
  , Semiring a
  , Storable a
  , Num a) =>
  Hilbert (Array s a) where
  (<.>) (Array a) (Array b) = V.sum $ V.zipWith (*) a b
  {-# inline (<.>) #-}

instance
  ( HasShape s
  , Multiplicative a
  , Storable a) =>
  MultiplicativeAction (Array s a) where
  (.*) (Array r) s = Array $ V.map (*s) r
  {-# inline (.*) #-}
  (*.) s (Array r) = Array $ V.map (s*) r
  {-# inline (*.) #-}

-- | from flat list
instance
    ( HasShape s
    , Additive a
    , Storable a
    ) =>
    IsList (Array s a) where
  type Item (Array s a) = a
  fromList l = Array $ V.fromList $ take mn $ l ++ repeat zero
    where
      mn = P.product $ shapeVal (toShape @s)

  toList (Array v) = V.toList v

mmult :: forall m n k a.
  ( KnownNat k
  , KnownNat m
  , KnownNat n
  , HasShape [m,n]
  , Ring a
  , Storable a
  )
  => Array [m, k] a
  -> Array [k, n] a
  -> Array [m, n] a
mmult (Array x) (Array y) = generate go
  where
    go [i,j] = V.foldl' (+) zero $ V.zipWith (*) (V.slice (fromIntegral i * n) n x) (V.generate m (\x' -> y V.! (fromIntegral j + x' * n)))
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy
{-# inline mmult #-}

type instance Actor (Array s a) = a
