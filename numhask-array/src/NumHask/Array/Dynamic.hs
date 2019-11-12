{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module NumHask.Array.Dynamic where

import Control.Category (id)
import GHC.Show (Show(..))
import NumHask.Prelude as P
import NumHask.Array.Shape
import qualified Data.Vector as V

data DArray a =
  DArray { shape :: [Int], unArray :: V.Vector a}
  deriving (NFData, Generic)

instance (Show a) => Show (DArray a) where
  show a@(DArray l _) = go (length l) a
    where
    go n a'@(DArray l' m) =
      case length l' of
        0 -> "[]"
        1 -> "[" ++ intercalate ", " (GHC.Show.show <$> V.toList m) ++ "]"
        x ->
          "[" ++
          intercalate
          (",\n" ++ replicate (n - x + 1) ' ')
          (go n <$> flatten1 a') ++
          "]"

-- FIXME: this is extracts (Proxy :: Proxy '[0])
-- | convert the first dimension to a list
-- flatten ([1..] :: Array '[2,3,4]) ~ [Array '[3,4]]
flatten1 :: ( ) => DArray a -> [DArray a]
flatten1 a = (\i -> select 0 i a) <$> [0..(x0 - 1)]
  where
    x0 = maybe 0 id (head (shape a))

index :: ( ) => DArray a -> [Int] -> a
index (DArray s v) i = V.unsafeIndex v (flatten s i)

tabulate :: ( ) => [Int] -> ([Int] -> a) -> DArray a
tabulate ds f = DArray ds . V.generate (size ds) $ (f . shapen ds)

select :: ( ) =>
  Int -> Int -> DArray a -> DArray a
select d i da@(DArray s _) = tabulate news go
  where
    go s' = index da (let (x1,x2) = splitAt d s' in (x1++(i:x2)))
    news = take d s ++ drop (d+1) s

fromFlatList :: [Int] -> [a] -> DArray a
fromFlatList ds l = DArray ds $ V.fromList $ take (product ds) l

toFlatList :: DArray a -> [a]
toFlatList (DArray _ v) = V.toList v

mmult :: forall a.
  ( Ring a
  )
  => DArray a
  -> DArray a
  -> DArray a
mmult (DArray s1 x) (DArray s2 y) = tabulate s3 go
  where
    go [i,j] = V.foldl' (+) zero $ V.zipWith (*) (V.slice (fromIntegral i * n) n x) (V.generate m (\x' -> y V.! (fromIntegral j + x' * n)))
    [m,k] = s1
    [k',n] = s2
    s3 = take 1 s1 ++ drop 1 s2

