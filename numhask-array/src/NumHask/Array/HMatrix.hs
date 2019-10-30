{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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

module NumHask.Array.HMatrix where

import GHC.Exts (IsList(..))
import GHC.Show (Show(..))
import NumHask.Prelude as P
import NumHask.Array.Shape
import qualified Prelude
import qualified Numeric.LinearAlgebra.Data as H
import qualified Numeric.LinearAlgebra as H
import qualified Numeric.LinearAlgebra.Devel as H

newtype HArray s a = HArray { unArray :: H.Matrix a} deriving (Show, NFData, Generic)

shape :: forall a s. (HasShape s) => HArray s a -> [Int]
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


index :: forall s a.
  ( HasShape s
  , H.Element a
  , H.Container H.Vector a
  )
  => HArray s a -> [Int] -> a
index (HArray v) i = H.flatten v `H.atIndex` ind s i
    where
      s = shapeVal (toShape @s)

instance
  ( Additive a
  , HasShape s
  , H.Container H.Vector a
  , Num a
  )
  => Additive (HArray s a) where
  (+) (HArray x1) (HArray x2) = HArray $ H.add x1 x2
  zero = HArray $ H.konst zero (n,m)
    where
      s = shapeVal (toShape @s)
      [n,m] = s

instance
  ( Multiplicative a
  , HasShape s
  , H.Container H.Vector a
  , Num (H.Vector a)
  , Num a
  )
  => Multiplicative (HArray s a) where
  (*) (HArray x1) (HArray x2) = HArray $ H.liftMatrix2 (Prelude.*) x1 x2
  one = HArray $ H.konst one (n,m)
    where
      s = shapeVal (toShape @s)
      [n,m] = s

type instance Actor (HArray s a) = a

instance
  ( HasShape s
  , P.Distributive a
  , CommutativeRing a
  , Semiring a
  , H.Container H.Vector a
  , Num (H.Vector a)
  , Num a
  ) =>
  Hilbert (HArray s a) where
  (<.>) (HArray a) (HArray b) = H.sumElements $ H.liftMatrix2 (Prelude.*) a b
  {-# inline (<.>) #-}

instance
  ( HasShape s
  , Multiplicative a
  , H.Container H.Vector a
  , Num a
  ) =>
  MultiplicativeAction (HArray s a) where
  (.*) (HArray r) s = HArray $ H.cmap (*s) r
  {-# inline (.*) #-}
  (*.) s (HArray r) = HArray $ H.cmap (s*) r
  {-# inline (*.) #-}

-- | from flat list
instance
    ( HasShape s
    , Additive a
    , H.Element a
    ) =>
    IsList (HArray s a) where
  type Item (HArray s a) = a
  fromList l = HArray $ H.reshape n $ H.fromList $ take mn $ l ++ repeat zero
    where
      mn = P.product $ shapeVal (toShape @s)
      s = shapeVal (toShape @s)
      n = Prelude.last s
  toList (HArray v) = H.toList $ H.flatten v

mmult :: forall m n k a.
  ( KnownNat k
  , KnownNat m
  , KnownNat n
  , HasShape [m,n]
  , Ring a
  , H.Numeric a
  )
  => HArray [m, k] a
  -> HArray [k, n] a
  -> HArray [m, n] a
mmult (HArray x) (HArray y) = HArray $ x H.<> y

type instance Actor (HArray s a) = a
