{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE DataKinds #-}
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

module NumHask.Matrix.Unboxed where

import GHC.Show (Show(..))
import GHC.Exts
-- import NumHask.Error (impossible)
-- import NumHask.Array.Constraints
--  (Fold, HeadModule, TailModule, IsValidConcat, Concatenate, Transpose, Squeeze)
import NumHask.Prelude as P
-- import NumHask.Shape (HasShape(..))
-- import Numeric.Dimensions as D
-- import qualified Data.Singletons.Prelude as S
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Sized as U
import qualified Data.Vector.Generic.Sized.Internal as SI
import GHC.TypeLits

newtype Matrix m n a = Matrix { unUMatrix :: U.Vector (m GHC.TypeLits.* n) a} deriving (Eq, Show)

-- | convert from n-dim shape index to a flat index
--
-- >>> ind [2,3,4] [1,1,1]
-- 17
ind :: [Int] -> [Int] -> Int
ind ns xs = sum $ P.zipWith (*) xs (drop 1 $ scanr (*) 1 ns)
{-# inline ind #-}

-- | convert from a flat index to a shape index
--
-- >>> unind [2,3,4] 17
-- [1,1,1]
unind :: [Int] -> Int -> [Int]
unind ns x =
  fst $
  foldr
    (\a (acc, r) ->
       let (d, m) = divMod r a
       in (m : acc, d))
    ([], x)
    ns
{-# inline unind #-}

unind' :: (Int,Int) -> Int -> (Int,Int)
unind' (_,j) x = divMod x j
{-# inline unind' #-}

unind'' :: Integral a => (a,a) -> a -> (a,a)
unind'' (_,j) x = divMod x j
{-# inline unind'' #-}

instance (NFData a) => NFData (Matrix m n a) where
  rnf (Matrix a) = rnf a

mmult :: forall m n k a.
  ( KnownNat k
  , KnownNat m
  , KnownNat n
  , Ring a
  , U.Unbox a
  )
  => Matrix m k a
  -> Matrix k n a
  -> Matrix m n a
mmult (Matrix (SI.Vector x)) (Matrix (SI.Vector y)) = Matrix $ SI.Vector $ UV.generate (m*n) go
  where
    go i =
      let (i', j') = i `divMod` n in
        UV.foldl' (+) zero $
        UV.zipWith (*) (rowi i') (colj j')
    rowi i'' = UV.slice (i'' * n) n x
    colj j'' = UV.generate m (\x' -> y UV.! (j'' + x' * n))
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy
{-# inline mmult #-}

-- | from flat list
instance
    ( KnownNat m
    , KnownNat n
    , Additive a
    , U.Unbox a
    ) =>
    IsList (Matrix m n a) where
  type Item (Matrix m n a) = a
  fromList l = Matrix $ SI.Vector $ UV.fromList $ take mn $ l ++ repeat zero
    where
      mn = fromIntegral $ natVal @m Proxy * natVal @n Proxy

  toList (Matrix v) = U.toList v

