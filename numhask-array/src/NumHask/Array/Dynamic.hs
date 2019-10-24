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

import Control.Category (id)
import GHC.Exts (IsList(..))
import GHC.Show (Show(..))
-- import NumHask.Error (impossible)
-- import NumHask.Array.Constraints
--  (Fold, HeadModule, TailModule, IsValidConcat, Concatenate, Transpose, Squeeze)
import NumHask.Prelude as P
import qualified Prelude
import NumHask.Shape
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

data DArray a = DZero | DOne | DSingleton a | DArray { shape :: [Int], unArray :: V.Vector a} deriving (NFData, Generic)

instance (Show a) => Show (DArray a) where
  show a = pretty a

pretty a@(DArray l _) = go (length l) a
  where
    go n a'@(DArray l' m) =
      case length l' of
        0 -> "[]"
        1 -> "[" ++ intercalate ", " (GHC.Show.show <$> (V.toList m)) ++ "]"
        x ->
          "[" ++
          intercalate
          (",\n" ++ replicate (n - x + 1) ' ')
          (go n <$> flatten1 a') ++
          "]"

-- | convert the first dimension to a list
-- flatten ([1..] :: Array '[2,3,4]) ~ [Array '[3,4]]
flatten1 :: ( ) => DArray a -> [DArray a]
flatten1 a = (\i -> select 0 i a) <$> [0..(x0 - 1)]
  where
    x0 = maybe 0 id (head (shape a))

index :: DArray a -> [Int] -> a
index (DArray s v) i = V.unsafeIndex v (flatten s i)

generate :: ( ) => [Int] -> ([Int] -> a) -> DArray a
generate ds f = DArray ds . V.generate (size ds) $ (f . shapen ds)

select :: ( ) =>
  Int -> Int -> DArray a -> DArray a
select d i da@(DArray s a) = generate news go
  where
    go s' = index da (let (x1,x2) = splitAt d s' in (x1++(i:x2)))
    news = take d s ++ drop (d+1) s

instance
  ( Additive a
  )
  => Additive (DArray a) where
  (+) (DArray s x1) (DArray _ x2) = DArray s $ V.zipWith (+) x1 x2
  zero = DZero

instance
  ( Multiplicative a
  )
  => Multiplicative (DArray a) where
  (*) (DArray s x1) (DArray _ x2) = DArray s $ V.zipWith (*) x1 x2
  one = DOne

type instance Actor (DArray a) = a

instance
  ( P.Distributive a
  , CommutativeRing a
  , Semiring a
  ) =>
  Hilbert (DArray a) where
  (<.>) (DArray _ a) (DArray _ b) = sum $ V.zipWith (*) a b
  {-# inline (<.>) #-}

instance
  ( Multiplicative a
  ) =>
  MultiplicativeAction (DArray a) where
  (.*) (DArray s r) a = DArray s $ fmap (*a) r
  {-# inline (.*) #-}
  (*.) a (DArray s r) = DArray s $ fmap (a*) r
  {-# inline (*.) #-}

mmult :: forall a.
  ( Ring a
  )
  => DArray a
  -> DArray a
  -> DArray a
mmult (DArray s1 x) (DArray s2 y) = generate s3 go
  where
    go [i,j] = V.foldl' (+) zero $ V.zipWith (*) (V.slice (fromIntegral i * n) n x) (V.generate m (\x' -> y V.! (fromIntegral j + x' * n)))
    [m,k] = s1
    [k',n] = s2
    s3 = take 1 s1 ++ drop 1 s2

type instance Actor (DArray a) = a

fromFlatList ds l = DArray ds $ V.fromList $ take (product ds) $ l

toFlatList (DArray _ v) = V.toList v

