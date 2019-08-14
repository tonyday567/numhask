{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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

module NumHask.Array.Dynamic where

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
data DArray a = DZero | DOne | DSingleton a | DArray { shape :: [Int], unArray :: H.Matrix a} deriving (Show, NFData, Generic)

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
  ( H.Element a
  , Additive a
  , H.Container H.Vector a
  )
  => DArray a -> [Int] -> a
index DZero _ = zero
index (DArray s v) i = (H.flatten v) `H.atIndex` (ind s i)

instance
  ( Additive a
  , H.Container H.Vector a
  , Num a
  )
  => Additive (DArray a) where
  (+) (DArray s x1) (DArray _ x2) = DArray s $ H.add x1 x2
  zero = DZero

instance
  ( Multiplicative a
  , H.Container H.Vector a
  , Num (H.Vector a)
  , Num a
  )
  => Multiplicative (DArray a) where
  (*) (DArray s x1) (DArray _ x2) = DArray s $ H.liftMatrix2 (Prelude.*) x1 x2
  one = DOne

type instance Actor (DArray a) = a

instance
  ( P.Distributive a
  , CommutativeRing a
  , Semiring a
  , H.Container H.Vector a
  , Num (H.Vector a)
  , Num a
  ) =>
  Hilbert (DArray a) where
  (<.>) (DArray _ a) (DArray _ b) = H.sumElements $ H.liftMatrix2 (Prelude.*) a b
  {-# inline (<.>) #-}

instance
  ( Multiplicative a
  , H.Container H.Vector a
  , Num a
  ) =>
  MultiplicativeAction (DArray a) where
  (.*) (DArray s r) a = DArray s $ H.cmap (*a) r
  {-# inline (.*) #-}
  (*.) a (DArray s r) = DArray s $ H.cmap (a*) r
  {-# inline (*.) #-}

fromFlatList s l = DArray s $ H.reshape n $ H.fromList $ take mn $ l ++ repeat zero
    where
      mn = P.product s
      n = Prelude.last s

toFlatList (DArray _ v) = H.toList $ H.flatten $ v

mmult :: forall a.
  ( Ring a
  , H.Numeric a
  )
  => DArray a
  -> DArray a
  -> DArray a
mmult (DArray s1 x) (DArray s2 y) = DArray s1 $ x H.<> y

type instance Actor (DArray a) = a
