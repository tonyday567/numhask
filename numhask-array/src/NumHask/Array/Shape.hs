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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | numbers with a shape
module NumHask.Array.Shape where

import NumHask.Prelude as P hiding (Last)
-- import qualified Prelude
import GHC.TypeLits as L
import Data.Type.Bool hiding (If, Not)

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

-- |
isDiag :: (Eq a) => [a] -> Bool
isDiag [] = True
isDiag [_] = True
isDiag [x,y] = x==y
isDiag (x:y:xs) = x == y && isDiag (y:xs)

-- | drop the ith element
-- >>> dropIndex [2, 3, 4] 1
-- [2,4]
dropIndex :: [a] -> Int -> [a]
dropIndex s i = take i s ++ drop (i+1) s

-- | convert a list of indexes that references the final indexes to one that references indexes at removal
-- >>> debumpDropIndexes [0,1]
-- [0,0]
debumpDropIndexes :: (Ord a, Subtractive a, Multiplicative a) => [a] -> [a]
debumpDropIndexes as = reverse (go [] as)
  where
    go r [] = r
    go r (x:xs) = go (x:r) ((\y -> bool (y - one) y (y < x)) <$> xs)

-- | drop elements according to a list of placements (where the placements refer to the initial indexes)
-- >>> dropIndexes [2, 3, 4] [1, 0]
-- [4]
dropIndexes :: [a] -> [Int] -> [a]
dropIndexes rs xs = foldl' dropIndex rs (debumpDropIndexes xs)

-- | insert an element
-- >>> addIndex [2,4] 1 3
-- [2,3,4]
addIndex :: [a] -> Int -> a -> [a]
addIndex s i a = take i s ++ (a:drop i s)

-- | convert a list of indexes that references the final indexes to one that references indexes at insertion.
-- >>> debumpAddIndexes [1,0]
-- [0,0]
debumpAddIndexes :: (Ord a, Subtractive a, Multiplicative a) => [a] -> [a]
debumpAddIndexes as = go [] (reverse as)
  where
    go r [] = r
    go r (x:xs) = go (x:r) ((\y -> bool (y - one) y (y < x)) <$> xs)

-- | insert elements according to a list of indexes.  Note that the list of placements references the final indexes
-- >>> addIndexes [4] [(1,3), (0,2)]
-- [2,3,4]
addIndexes :: ( ) => [a] -> [(Int, a)] -> [a]
addIndexes as adds = foldl' (\s (i,a) -> addIndex s i a) as (zip (debumpAddIndexes . fmap fst $ adds) (fmap snd adds))

-- | Number of Dimensions
type family Rank (s :: [Nat]) :: Nat where
  Rank '[] = 0
  Rank (_:s) = Rank s + 1

-- | Number of Elements
type family Product (s :: [Nat]) :: Nat where
  Product '[] = 1
  Product (n:s) = n L.* Product s

type family If (b :: Bool) c d where
  If 'True  c d = c
  If 'False c d = d

type family Not (a :: Bool) where
  Not 'True  = 'False
  Not 'False = 'True

type family Replicate (a :: k) (dim :: Nat) :: [k] where
  Replicate a 0 = '[]
  Replicate a n = a : Replicate a n

type family Min (s :: [Nat]) :: Nat where
  Min '[] = L.TypeError ('Text "zero dimension")
  Min '[x] = x
  Min (x:xs) = If (x <=? Min xs) x (Min xs)

type family Dimension (s :: [Nat]) (i :: Nat) :: Nat where
  Dimension (s:_) 0 = s
  Dimension (_:s) n = Dimension s (n - 1)
  Dimension _ _     = L.TypeError ('Text "Index overflow")

dimension (s:_) 0 = s
dimension (_:s) n = dimension s (n - 1)
dimension _ _     = throw (NumHaskException "index overflow")

type CheckDimension dim s = IsIndex dim (Rank s)

type CheckIndices i j s = IsIndices i j (Rank s) ~ 'True

type IsIndex i n = (0 <=? i) && (i + 1 <=? n)
type IsIndices i j n = (0 <=? i) && (i + 1 <=? j) && (j + 1 <=? n)

type family CheckIndex (i :: Nat) (n :: Nat) :: Bool where
  CheckIndex i n =
    If ((0 <=? i) && (i + 1 <=? n)) 'True (L.TypeError ('Text "index outside range"))

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

{-
type family (a :: [k]) == (b :: [k]) :: Bool where
  '[] == '[] = True
  (a:as) == (b:bs) = a==b && as == bs

-}

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ b = b
  (a:as) ++ b = a : (as ++ b)

type DropIndex s i = Take i s ++ Drop (i+1) s

type AddIndex s d i = Take d s ++ i ++ Drop d s

type AddIndexI s d i = Take d s ++ (i:Drop d s)

type family (a :: k) > (b :: k) :: Bool where
  (>) a b = a > b

type family (a :: k) < (b :: k) :: Bool where
  (<) a b = a < b

-- let ds = [0,1]
-- let so = [2,3]
-- let si = [4]
-- AddIndexes [4] [0,1] [2,3]
-- AddIndexes (AddIndexI [4] 0 2) [1] [3]
-- AddIndexes (Take 0 [4] ++ (2:Drop 0 [4])) [1] [3]
-- AddIndexes [2,4] [1] [3]
-- AddIndexes (AddIndexI [2,4] 1 3) [] []
-- AddIndexes [2,3,4] [] []
-- [2,3,4]
--
type family AddIndexes (si::[Nat]) (ds::[Nat]) (so::[Nat]) where
  AddIndexes si '[] _ = si
  AddIndexes si (d:ds) (o:os) =
    If ((==) (Rank ds) (Rank os))
    (AddIndexes (AddIndexI si d o) ds os)
    '[]

type family AddDim (d::Nat) (ds::[Nat]) :: [Nat] where
  AddDim _ '[] = '[]
  AddDim d (x:xs) = (If ((<) d x) x (x + 1)) : AddDim d xs

type family SubDim (d::Nat) (ds::[Nat]) :: [Nat] where
  SubDim _ '[] = '[]
  SubDim d (x:xs) = (If ((<=?) d x) (x - 1) x) : SubDim d xs

type family SubDims (ds::[Nat]) (rs :: [Nat]) :: [Nat] where
  SubDims '[] rs = rs
  SubDims (x:xs) rs = SubDims (SubDim x xs) (x:rs)

type family SubDimsReverse (ds::[Nat]) :: [Nat] where
  SubDimsReverse xs = (SubDims (Reverse xs) '[])

-- let ds = [1,0]
-- let so = [3,2]
-- let si = [4]
-- AddIndexes' [4] [1,0] [3,2]
-- AddIndexes'' [4] (SubDimsReverse [1,0]) [3,2]
-- AddIndexes'' [4] ((SubDims (Reverse [1,0]) [])) [3,2]
-- AddIndexes'' [4] ((SubDims [0,1] [])) [3,2]
-- AddIndexes'' [4] ((SubDims (SubDim 0 [1]) [0])) [3,2]
-- AddIndexes'' [4] ((SubDims [0] [0])) [3,2]
-- AddIndexes'' [4] ((SubDims (SubDim 0 []) [0, 0])) [3,2]
-- AddIndexes'' [4] ((SubDims [] [0, 0])) [3,2]
-- AddIndexes'' [4] [0, 0] [3,2]
-- AddIndexes'' [4] [0, 0] [3,2]
-- AddIndexes'' (AddIndexI [4] 0 3) [0] [2]
-- AddIndexes'' Take 0 [4] ++ (3:Drop 0 [4]) [0] [2]
-- AddIndexes'' [3,4] [0] [2]
-- AddIndexes'' (AddIndexI [3,4] 0 2) [] []
-- AddIndexes'' Take 0 [3,4] ++ (2:Drop 0 [3,4]) [] []
-- AddIndexes'' [2,3,4] [] []
-- [2,3,4]
type family AddIndexes' (si::[Nat]) (ds::[Nat]) (so::[Nat]) where
  AddIndexes' si ds os =
    If ((==) (Rank ds) (Rank os))
    (AddIndexes'' si (SubDimsReverse ds) os)
    (L.TypeError ('Text "indices and dimensions need to be the same size"))

type family AddIndexes'' (si::[Nat]) (ds::[Nat]) (so::[Nat]) where
  AddIndexes'' si '[] _ = si
  AddIndexes'' si (d:ds) (o:os) =
    AddIndexes'' (AddIndexI si d o) ds os


type CheckConcatenate i a b = (IsIndex i (Rank a)) ~ 'True

type Concatenate i a b = Take i a ++ (Dimension a i + Dimension b i : Drop (i+1) a)

type CheckInsert dim i b =
  (CheckDimension dim b && IsIndex i (Dimension b dim))  ~ 'True

type Insert dim b = Take dim b ++ (Dimension b dim + 1 : Drop (dim + 1) b)

incAt' dim b = take dim b ++ (dimension b dim + 1 : drop (dim + 1) b)

decAt dim b = take dim b ++ (dimension b dim - 1 : drop (dim + 1) b)


type family ReorderDims (d :: [Nat]) (dims :: [Nat]) :: [Nat] where
  ReorderDims '[] _ = '[]
  ReorderDims _ '[] = '[]
  ReorderDims ss (dim:dims) = (Dimension ss dim) : ReorderDims ss dims

reorderDims :: [Int] -> [Int] -> [Int]
reorderDims [] _ = []
reorderDims _ [] = []
reorderDims ss (dim:dims) = dimension ss dim:reorderDims ss dims

type family CheckReorder (dims :: [Nat]) (d :: [Nat]) (d' :: [Nat]) where
  CheckReorder dims d d' =
    If (ReorderDims d dims == d') 'True
    (L.TypeError ('Text "bad dimensions")) ~ 'True

type family DimSeq (n :: Nat) :: [Nat] where
  DimSeq 0 = '[0]
  DimSeq x = (DimSeq (x - 1)) ++ '[x]

type family Sort (xs :: [k]) :: [k] where
            Sort '[]       = '[]
            Sort (x ': xs) = ((Sort (SFilter 'FMin x xs)) ++ '[x]) ++ (Sort (SFilter 'FMax x xs))

data Flag = FMin | FMax

type family Cmp (a :: k) (b :: k) :: Ordering

type family SFilter (f :: Flag) (p :: k) (xs :: [k]) :: [k] where
            SFilter f p '[]       = '[]
            SFilter FMin p (x ': xs) = If (Cmp x p == LT) (x ': (SFilter FMin p xs)) (SFilter FMin p xs)
            SFilter FMax p (x ': xs) = If (Cmp x p == GT || Cmp x p == EQ) (x ': (SFilter FMax p xs)) (SFilter FMax p xs)
            
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

type family Filter (r::[Nat]) (xs::[Nat]) (i::Nat) where
  Filter r '[] _ = Reverse r
  Filter r (x:xs) i = Filter (If (x==i) r (x:r)) xs i

type family Squeeze (a :: [Nat]) where
  Squeeze '[] = '[]
  Squeeze a = Filter '[] a 1
