{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

module NumHask.Array.Unboxed where

import GHC.Show (Show(..))
import GHC.Exts
-- import NumHask.Error (impossible)
-- import NumHask.Array.Constraints
--  (Fold, HeadModule, TailModule, IsValidConcat, Concatenate, Transpose, Squeeze)
import NumHask.Prelude as P
import qualified Prelude
-- import NumHask.Shape (HasShape(..))
-- import Numeric.Dimensions as D
-- import qualified Data.Singletons.Prelude as S
import qualified Data.Vector.Unboxed as V
-- import qualified Data.Vector.Unboxed.Sized as U
-- import qualified Data.Vector.Generic.Sized.Internal as SI
-- import GHC.TypeLits

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

instance
  ( Additive a
  , HasShape s
  , V.Unbox a
  )
  => Additive (Array s a) where
  (+) (Array x) (Array y) = Array $ V.zipWith (+) x y
  zero = Array $ V.generate p (const zero)
    where
      p = size $ shapeVal (toShape @s)

instance
  ( Multiplicative a
  , HasShape s
  , V.Unbox a
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
  , V.Unbox a
  , Num a
  ) =>
  Hilbert (Array s a) where
  (<.>) (Array a) (Array b) = V.sum $ V.zipWith (*) a b
  {-# inline (<.>) #-}

instance
  ( HasShape s
  , Multiplicative a
  , V.Unbox a
  ) =>
  MultiplicativeAction (Array s a) where
  (.*) (Array r) s = Array $ V.map (*s) r
  {-# inline (.*) #-}
  (*.) s (Array r) = Array $ V.map (s*) r
  {-# inline (*.) #-}

mmult :: forall m n k a.
  ( KnownNat k
  , KnownNat m
  , KnownNat n
  , HasShape [m,n]
  , Ring a
  , V.Unbox a
  )
  => Array [m, k] a
  -> Array [k, n] a
  -> Array [m, n] a
mmult (Array x) (Array y) = Array $ V.generate (m*n) go
  where
    go i =
      let (i', j') = i `divMod` n in
        V.foldl' (+) zero $
        V.zipWith (*) (rowi i') (colj j')
    rowi i'' = V.slice (i'' * n) n x
    colj j'' = V.generate m (\x' -> y V.! (j'' + x' * n))
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy
{-# inline mmult #-}

instance
    ( HasShape s
    , Additive a
    , V.Unbox a
    ) =>
    IsList (Array s a) where
  type Item (Array s a) = a
  fromList l = Array $ V.fromList $ take mn $ l ++ repeat zero
    where
      mn = P.product $ shapeVal (toShape @s)

  toList (Array v) = V.toList v

