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

module NumHask.Array.HMatrixV where

import Data.Distributive (Distributive(..))
import Data.Functor.Rep
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
import qualified Data.Vector as V
-- import qualified Data.Vector.Generic.Sized as S
-- import qualified Data.Vector.Generic.Sized.Internal as SI
-- import qualified Data.Vector.Generic.Base as B
-- import Data.Finite
-- import GHC.TypeLits
-- concrete vector based on vector-sized
-- import qualified Prelude (fromIntegral)
-- import qualified NumHask.Vector as NH
import qualified Numeric.LinearAlgebra.Data as H
import qualified Numeric.LinearAlgebra as H
-- import qualified Numeric.LinearAlgebra.HMatrix as OldH
import qualified Numeric.LinearAlgebra.Devel as H
newtype Array s a = Array { unArray :: H.Vector a} deriving (Show, NFData, Generic)

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


index :: forall s a.
  ( HasShape s
  , H.Element a
  , H.Container H.Vector a
  )
  => Array s a -> [Int] -> a
index (Array v) i = v `H.atIndex` (ind s i)
    where
      s = shapeVal (toShape @s)

instance
  ( Additive a
  , HasShape s
  , H.Container H.Vector a
  , Num a
  )
  => Additive (Array s a) where
  (+) (Array x1) (Array x2) = Array $ H.add x1 x2
  zero = Array $ H.konst zero (size s)
    where
      s = shapeVal (toShape @s)

instance
  ( Multiplicative a
  , HasShape s
  , H.Container H.Vector a
  , Num (H.Vector a)
  , Num a
  )
  => Multiplicative (Array s a) where
  (*) (Array x1) (Array x2) = Array $ (Prelude.*) x1 x2
  one = Array $ H.konst one (size s)
    where
      s = shapeVal (toShape @s)

type instance Actor (Array s a) = a

instance
  ( HasShape s
  , P.Distributive a
  , CommutativeRing a
  , Semiring a
  , H.Container H.Vector a
  , Num (H.Vector a)
  , Num a
  ) =>
  Hilbert (Array s a) where
  (<.>) (Array a) (Array b) = H.sumElements $ (Prelude.*) a b
  {-# inline (<.>) #-}

instance
  ( HasShape s
  , Multiplicative a
  , H.Container H.Vector a
  , Num a
  ) =>
  MultiplicativeAction (Array s a) where
  (.*) (Array r) s = Array $ H.cmap (*s) r
  {-# inline (.*) #-}
  (*.) s (Array r) = Array $ H.cmap (s*) r
  {-# inline (*.) #-}

-- | from flat list
instance
    ( HasShape s
    , Additive a
    , H.Element a
    ) =>
    IsList (Array s a) where
  type Item (Array s a) = a
  fromList l = Array $ H.fromList $ take mn $ l ++ repeat zero
    where
      mn = P.product $ shapeVal (toShape @s)
      s = shapeVal (toShape @s)
      n = Prelude.last s
  toList (Array v) = H.toList v

mmult :: forall m n k a.
  ( KnownNat k
  , KnownNat m
  , KnownNat n
  , HasShape [m,n]
  , Ring a
  , H.Numeric a
  )
  => Array [m, k] a
  -> Array [k, n] a
  -> Array [m, n] a
mmult (Array x) (Array y) =
  Array $ H.flatten $ 
  (H.matrixFromVector H.RowMajor m k x) H.<>
  (H.matrixFromVector H.RowMajor k n y)
  where
    k = fromIntegral $ natVal @k Proxy
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy

type instance Actor (Array s a) = a
