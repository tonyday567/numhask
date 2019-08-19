{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | numbers with a shape
module NumHask.Shape where

import NumHask.Prelude as P hiding (Last)
import qualified Prelude
import GHC.TypeLits as L
import           Data.Type.Bool hiding (If, Not, (>), (<))

newtype Shape (s :: [Nat]) = Shape { shapeVal :: [Int] } deriving Show

class HasShape s where
  toShape :: Shape s

instance HasShape '[] where
  toShape = Shape []

instance (KnownNat n, HasShape s) => HasShape (n:s) where
  toShape = Shape $ fromInteger (natVal (Proxy :: Proxy n)) : shapeVal (toShape :: Shape s)

size :: [Int] -> Int
size [] = zero
size [x] = x
size xs = product xs
{-# inline size #-}

-- | convert from n-dim shape index to a flat index
--
-- >>> flatten [2,3,4] [1,1,1]
-- 17
flatten :: [Int] -> [Int] -> Int
flatten [] _ = zero
flatten _ [x'] = x'
flatten ns xs = sum $ zipWith (*) xs (drop 1 $ scanr (*) one ns)
{-# inline flatten #-}

-- | convert from a flat index to a shape index
--
-- >>> shapen [2,3,4] 17
-- [1,1,1]
shapen :: [Int] -> Int -> [Int]
shapen [] _ = []
shapen [_] x' = [x']
shapen [_,y] x' = let (i,j) = divMod x' y in [i,j]
shapen ns x =
  fst $
  foldr
    (\a (acc, r) ->
       let (d, m) = divMod r a
       in (m : acc, d))
    ([], x)
    ns
{-# inline shapen #-}

-- | Tensor rank.
type family Rank (s :: [Nat]) :: Nat where
  Rank '[] = 0
  Rank (_:s) = Rank s + 1

-- | Tensor size.
type family Size (s :: [Nat]) :: Nat where
  Size '[] = 1
  Size (n:s) = n L.* Size s

type family If (b :: Bool) c d where
  If 'True  c d = c
  If 'False c d = d

type family Not (a :: Bool) where
  Not 'True  = 'False
  Not 'False = 'True


type family Replicate (a :: k) (dim :: Nat) :: [k] where
  Replicate a 0 = '[]
  Replicate a n = a : Replicate a n

type family Dimension (s :: [Nat]) (i :: Nat) :: Nat where
  Dimension (s:_) 0 = s
  Dimension (_:s) n = Dimension s (n - 1)
  Dimension _ _     = L.TypeError ('Text "Index overflow")

type CheckDimension dim s = IsIndex dim (Rank s)
type CheckIndices i j s = IsIndices i j (Rank s) ~ 'True

type IsIndex i n = (0 <=? i) && (i + 1 <=? n)
type IsIndices i j n = (0 <=? i) && (i + 1 <=? j) && (j + 1 <=? n)

type family Take (n :: Nat) (a :: [k]) :: [k] where
  Take 0 _ = '[]
  Take n (x:xs) = x : Take (n - 1) xs

type family Drop (n :: Nat) (a :: [k]) :: [k] where
  Drop 0 xs = xs
  Drop n (_:xs) = Drop (n - 1) xs

type family Tail (a :: [k]) :: [k] where
  Tail '[] = L.TypeError ('Text "No tail")
  Tail (_:xs) = xs

type family Init (a :: [k]) :: [k] where
  Init '[] = L.TypeError ('Text "No init")
  Init '[_] = '[]
  Init (x:xs) = x : Init xs

type family Head (a :: [k]) :: k where
  Head '[] = L.TypeError ('Text "No head")
  Head (x:_) = x

type family Last (a :: [k]) :: k where
  Last '[] = L.TypeError ('Text "No last")
  Last '[x] = x
  Last (_:xs) = Last xs

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ b = b
  (a:as) ++ b = a : (as ++ b)

-- type Transpose (a :: [Nat]) = Reverse a '[]

type Swapaxes i j s = Take i s ++ (Dimension s j : Drop i (Take j s)) ++ (Dimension s j : Tail (Drop j s))

type DropIndex s i = Take i s ++ Drop (i+1) s

type AddIndex s d i = Take d s ++ i ++ Drop d s

type AddIndexI s d i = Take d s ++ (i:Drop d s)

type family (a :: k) > (b :: k) :: Bool
type family (a :: k) < (b :: k) :: Bool

type family BumpDim (d::Nat) (ds::[Nat]) :: [Nat] where
  BumpDim _ '[] = '[]
  BumpDim d (x:xs) = (If ((<) d x) x (x + 1)) : xs

type family AddIndexes (s::[Nat]) (d::[Nat]) (i::[Nat]) where
  AddIndexes s '[] _ = s
  AddIndexes s (d:ds) (i:is) = AddIndexes (AddIndexI s d i) (BumpDim d ds) is

type SelectIndex s i = Take 1 (Drop i s)

type Contraction s x y = DropIndex (DropIndex s y) x

type family IsElement (s::[Nat]) (e::Nat) where
  IsElement '[] _ = 'False
  IsElement (s:xs) e = (s == e) || IsElement xs e

type family Zip lst lst' where Zip lst lst' = ZipWith '(,) lst lst' -- Implemented as TF because #11375

type family ZipWith f lst lst' where
    ZipWith f '[]       lst       = '[]
    ZipWith f lst       '[]       = '[]
    ZipWith f (l ': ls) (n ': ns) = f l n ': ZipWith f ls ns

type family Fst a where
  Fst '(a,_) = a

type family Snd a where
  Snd '(_,a) = a

type family DropIndexesZ (s::[(Nat,Nat)]) (i::[Nat]) where
  DropIndexesZ '[] _ = '[]
  DropIndexesZ (x ': xs) i =
    If (IsElement i (Snd x)) (DropIndexesZ xs i) (Fst x : DropIndexesZ xs i)

type family DropIndexes (s::[Nat]) (i::[Nat]) where
  DropIndexes s i = DropIndexesZ (Zip s (Enumerate (Rank s))) i

type family EnumerateRev (n::Nat) where
  EnumerateRev 0 = '[]
  EnumerateRev n = (n - 1) : EnumerateRev (n - 1)

type family Enumerate (n::Nat) where
  Enumerate n = Reverse (EnumerateRev n)

type family Rev (a :: [k]) (b :: [k]) :: [k] where
  Rev '[] b = b
  Rev (a:as) b = Rev as (a:b)

type Reverse (a :: [k]) = Rev a '[]

type family FMap f lst where
  FMap f '[] = '[]
  FMap f (l ': ls) = f l ': FMap f ls

type family TakeIndexes (s::[Nat]) (i::[Nat]) where
  TakeIndexes '[] _ = '[]
  TakeIndexes _ '[] = '[]
  TakeIndexes s (i:is) =
    (s !! i) ': TakeIndexes s is

type family (a :: [k]) !! (b :: Nat) :: k where
  -- (!!) '[] i = P.undefined -- L.TypeError "beebaaa"
  (!!) (x:_) 0 = x
  (!!) (_:xs) i = (!!) xs (i - 1)



