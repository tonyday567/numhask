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

module NumHask.Array.Simple where

import GHC.TypeLits
import Data.Distributive (Distributive(..))
import Data.Functor.Rep
import GHC.Exts (IsList(..))
import GHC.Show (Show(..))
import NumHask.Prelude as P hiding (outer, identity, singleton)
import NumHask.Shape
import NumHask.Array.Constraints
import qualified NumHask.Array.Dynamic as D
import qualified Data.Vector as V
import qualified Data.List as List

-- $setup 
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeFamilies
-- >>> :set -XFlexibleContexts
-- >>> :set -XAllowAmbiguousTypes
-- >>> let a = [1..24] :: Array '[2,3,4] Int
-- >>> let v = [1,2,3] :: Array '[3] Int

-- | a multidimensional array with shape at the type level
-- underlying container is a boxed Vector
--
-- >>> a
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]
newtype Array s a = Array { unArray :: V.Vector a} deriving (Eq, Ord, NFData, Functor, Foldable, Generic, Traversable)

-- | <https://en.wikipedia.org/wiki/Scalarr_(mathematics) Scalar>
type Scalar a  = Array '[] a

-- | <https://en.wikipedia.org/wiki/Vector_(mathematics_and_physics) Vector>
type Vector s a = Array '[s] a

-- | <https://en.wikipedia.org/wiki/Matrix_(mathematics) Matrix>
type Matrix m n a = Array '[m,n] a

shape :: forall a s. (HasShape s) => Array s a -> [Int]
shape _ = shapeVal $ toShape @s
{-# inline shape #-}

toDArray a = D.fromFlatList (shape a) (P.toList a)

instance (HasShape s, Show a) => Show (Array s a) where
  show a = GHC.Show.show (toDArray a)

instance
  ( HasShape s
  ) => Data.Distributive.Distributive (Array s) where
  distribute = distributeRep
  {-# inline distribute #-}

instance forall s.
  ( HasShape s
  ) => Representable (Array s) where
  type Rep (Array s) = [Int]
  tabulate f =
    Array . V.generate (size s) $ (f . shapen s)
    where
      s = shapeVal $ toShape @s
  {-# inline tabulate #-}
  index (Array v) i = V.unsafeIndex v (flatten s i)
    where
      s = shapeVal (toShape @s)
  {-# inline index #-}

reshape :: forall a s s'. (Size s ~ Size s', HasShape s, HasShape s') => Array s a -> Array s' a
reshape a = tabulate (index a . shapen s' . flatten s )
    where
      s = shapeVal (toShape @s)
      s' = shapeVal (toShape @s')

-- reverse indices eg transpose Aijk to Akji
transpose :: forall a s. (HasShape s, HasShape (Reverse s)) => Array s a -> Array (Reverse s) a
transpose a = tabulate (index a . shapen s' . flatten s)
  where
    s = shapeVal (toShape @s)
    s' = reverse s


all' :: (Eq a) => [a] -> Bool
all' [] = True
all' [_] = True
all' [x,y] = x==y
all' (x:y:xs) = x == y && all' (y:xs)

-- | 
-- >>> ident :: Array '[3,2] Int
-- [[1, 0],
--  [0, 1],
--  [0, 0]]
--
ident :: forall a s. (HasShape s, Additive a, Multiplicative a) => Array s a
ident = tabulate (bool zero one . all')

-- |
-- >>> singleton one :: Array '[3,2] Int
-- [[1, 1],
--  [1, 1],
--  [1, 1]]
--
singleton :: (HasShape s) => a -> Array s a
singleton a = tabulate (const a)

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
addIndexes :: (Ord a, Subtractive a) => [a] -> [(Int, a)] -> [a]
addIndexes as adds = foldl' (\s (i,a) -> addIndex s i a) as (zip (debumpAddIndexes . fmap fst $ adds) (fmap snd adds))

-- | select an index along dimensions
-- >>> let s = selects (Proxy :: Proxy '[0,1]) [1,1] ([1..] :: Array '[2,3,4] Int)
-- >>> :t s
-- s :: Array '[4] Int
--
-- >>> s
-- [17, 18, 19, 20]
selects :: forall ds s s' a.
  ( HasShape s
  , HasShape ds
  , HasShape s'
  , s' ~ DropIndexes s ds
  ) => Proxy ds
  -> [Int]
  -> Array s a
  -> Array s' a
selects _ i a = tabulate go
  where
    go s' = index a (addIndexes s' (zip ds i))
    ds = shapeVal (toShape @ds)

-- | folds along specified dimensions
-- >>> foldsAlong sum (Proxy :: Proxy '[1]) ([1..] :: Array '[2,3,4] Int)
-- [68, 100, 132]
--
foldsAlong :: forall ds st si so a b.
  ( HasShape st
  , HasShape ds
  , HasShape si
  , HasShape so
  , si ~ DropIndexes st ds
  , so ~ TakeIndexes st ds
  )
  => (Array si a -> b)
  -> Proxy ds
  -> Array st a
  -> Array so b
foldsAlong f d a = tabulate go
  where
    go s' = f (selects d s' a)

-- | maps along specified dimensions
-- > mapsAlong (transpose) (Proxy :: Proxy '[1]) ([1..] :: Array '[2,3,4] Int)
--
mapsAlong :: forall ds st st' si si' so a b.
  ( HasShape st
  , HasShape st'
  , HasShape ds
  , HasShape si
  , HasShape si'
  , HasShape so
  , si ~ DropIndexes st ds
  , so ~ TakeIndexes st ds
  , st ~ AddIndexes' si ds so
  , st' ~ AddIndexes' si' ds so
  , st ~ AddIndexes si ds so
  , st' ~ AddIndexes si' ds so
  )
  => (Array si a -> Array si' b)
  -> Proxy ds
  -> Array st a
  -> Array st' b
mapsAlong f d a = joins d (fmapRep f (extracts d a))

-- | extracts dimensions
-- >>> let e = extracts (Proxy :: Proxy '[1,2]) ([1..] :: Array '[2,3,4] Int)
-- >>> :t e
-- e :: Array '[3, 4] (Array '[2] Int)
--
extracts :: forall ds st si so a.
  ( HasShape st
  , HasShape ds
  , HasShape si
  , HasShape so
  , si ~ DropIndexes st ds
  , so ~ TakeIndexes st ds
  ) => Proxy ds
  -> Array st a
  -> Array so (Array si a)
extracts d a = tabulate go
  where
    go s' = selects d s' a

-- | join inner and outer dimension layers
-- >>> let a = [1..] :: Array '[2,3,4] Int
-- >>> let e = extracts (Proxy :: Proxy '[1,0]) a
--
-- >>> :t e
-- e :: Array '[3, 2] (Array '[4] Int)
--
-- >>> let j = joins (Proxy :: Proxy '[1,0]) e
-- >>> :t j
-- j :: Array '[2, 3, 4] Int
--
-- >>> a == j
-- True
--
joins :: forall ds si st so a.
  ( HasShape st
  , HasShape ds
  , st ~ AddIndexes' si ds so
  , HasShape si
  , HasShape so
  )
  => Proxy ds
  -> Array so (Array si a)
  -> Array st a
joins _ a = tabulate go
  where
    go s' = index (index a (fmap (s' List.!!) ds)) (dropIndexes s' ds)
    ds = shapeVal (toShape @ds)

{-
-- | Join a sequence of arrays along an existing axis.
--
-- > λ> a = [1..4] :: Tensor '[2,2] Int
-- > λ> a
-- > [[1,2],
-- > [3,4]]
-- > λ> b = [1,1,1,1] :: Tensor '[2,2] Int
-- > λ> b
-- > [[1,1],
-- > [1,1]]
-- > λ> concentrate i0 a b
-- > [[1,2],
-- > [3,4],
-- > [1,1],
-- > [1,1]]
-- > λ> concentrate i1 a b
-- > [[1,2,1,1],
-- > [3,4,1,1]]
concatenate
  :: ( TensorRank a ~ TensorRank b
    , DropIndex a i ~ DropIndex b i
    , CheckConcatenate i a b
    , Concatenate i a b ~ c
    , HasShape a
    , HasShape b
    , KnownNat i)
  => Proxy i
  -> Tensor a n
  -> Tensor b n
  -> Tensor c n
concatenate p ta@(Tensor a) tb@(Tensor b) =
  let i  = toNat p
      sa = shape ta
      sb = shape tb
      n  = sa !! i
  in Tensor $ \_ ind -> let (ai,x:bi) = splitAt i ind in if x >= n then b sb (ai ++ (x-n):bi) else a sa ind


-}

-- | extract specialised to a matrix
--
-- >>> row 1 ([0..12] :: Array '[3,4] Int)
-- [4, 5, 6, 7]
--
row :: forall m n a. (KnownNat m, KnownNat n, HasShape '[m,n]) => Int -> Matrix m n a -> Vector n a
row i (Array a) = Array $ V.slice (i * n) n a
  where
    n = fromIntegral $ natVal @n Proxy

-- | row extraction checked at type level
--
-- >>> safeRow (Proxy :: Proxy 1) ([0..12] :: Array '[3,4] Int)
-- [4, 5, 6, 7]
--
-- >>> safeRow (Proxy :: Proxy 1) ([1,2,3] :: Array '[1,3] Int)
-- ...
-- ... index outside range
-- ...

safeRow :: forall m n a j. ('True ~ CheckIndex j m, KnownNat j, KnownNat m, KnownNat n, HasShape '[m,n]) => Proxy j -> Matrix m n a -> Vector n a
safeRow _j (Array a) = Array $ V.slice (j * n) n a
  where
    n = fromIntegral $ natVal @n Proxy
    j = fromIntegral $ natVal @j Proxy

-- | extract specialised to a matrix
--
-- >>> col 1 ([0..12] :: Array '[3,4] Int)
-- [1, 5, 9]
--
col  :: forall m n a. (KnownNat m, KnownNat n, HasShape '[m,n]) => Int -> Matrix m n a -> Vector n a
col i (Array a) = Array $ V.generate m (\x -> V.unsafeIndex a (i + x * n))
  where
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy

-- | column extraction checked at type level
--
-- >>> safeCol (Proxy :: Proxy 1) ([1..12] :: Array '[3,4] Int)
-- [2, 6, 10]
--
-- >>> safeCol (Proxy :: Proxy 4) ([1..12] :: Array '[3,4] Int)
-- ...
-- ... index outside range
-- ...
--
safeCol :: forall m n a j. ('True ~ CheckIndex j n, KnownNat j, KnownNat m, KnownNat n, HasShape '[m,n]) => Proxy j -> Matrix m n a -> Vector n a
safeCol _j (Array a) = Array $ V.generate m (\x -> V.unsafeIndex a (j + x * n))
  where
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy
    j = fromIntegral $ natVal @j Proxy

instance
  ( Additive a
  , HasShape s
  )
  => Additive (Array s a) where
  (+) = liftR2 (+)
  zero = pureRep zero

instance
  ( Subtractive a
  , HasShape s
  )
  => Subtractive (Array s a) where
  negate = fmapRep negate

instance
  ( Multiplicative a
  , P.Distributive a
  , Subtractive a
  , KnownNat m
  , HasShape '[m,m]
  )
  => Multiplicative (Matrix m m a) where
  (*) = mmult
  one = ident

type instance Actor (Array s a) = a

instance
  ( Multiplicative a
  , HasShape s
  )
  => HadamardMultiplication (Array s) a where
  (.*.) = liftR2 (*)

instance
  ( Divisive a
  , HasShape s
  )
  => HadamardDivision (Array s) a where
  (./.) = liftR2 (/)

instance
  ( HasShape s
  , Foldable (Array s)
  , P.Distributive a
  , CommutativeRing a
  , Semiring a) =>
  Hilbert (Array s a) where
  (<.>) a b = sum $ liftR2 (*) a b
  {-# inline (<.>) #-}

instance (HasShape s, Multiplicative a) =>
  MultiplicativeAction (Array s a) where
  (.*) r s = fmap (*s) r
  {-# inline (.*) #-}
  (*.) s = fmap (s*)
  {-# inline (*.) #-}

instance
  ( Foldable (Array s)
  , HasShape s
  , CommutativeRing a
  , Multiplicative a
  ) =>
  TensorProduct (Array s a) where
  (><) m n = tabulate (\i -> index m i *. n)
  timesleft v m = tabulate (\i -> v <.> index m i)
  timesright m v = tabulate (\i -> v <.> index m i)

-- | from flat list
instance
    ( HasShape s
    , Additive a
    ) =>
    IsList (Array s a) where
  type Item (Array s a) = a
  fromList l = Array $ V.fromList $ take mn $ l ++ repeat zero
    where
      mn = P.product $ shapeVal (toShape @s)

  toList (Array v) = V.toList v

-- | matrix multiplication
--
-- >>> let a = [1, 2, 3, 4] :: Array '[2, 2] Int
-- >>> let b = [5, 6, 7, 8] :: Array '[2, 2] Int
-- >>> a
-- [[1, 2],
--  [3, 4]]
--
-- >>> b
-- [[5, 6],
--  [7, 8]]
--
-- >>> mmult a b
-- [[19, 22],
--  [43, 50]]
--
mmult :: forall m n k a.
  ( KnownNat k
  , KnownNat m
  , KnownNat n
  , HasShape [m,n]
  , Ring a
  )
  => Array [m, k] a
  -> Array [k, n] a
  -> Array [m, n] a
mmult (Array x) (Array y) = tabulate go
  where
    go [] = throw (NumHaskException "Needs two dimensions")
    go [_] = throw (NumHaskException "Needs two dimensions")
    go (i:j:_) = sum $ V.zipWith (*) (V.slice (fromIntegral i * k) k x) (V.generate k (\x' -> y V.! (fromIntegral j + x' * n)))
    n = fromIntegral $ natVal @n Proxy
    k = fromIntegral $ natVal @k Proxy
{-# inline mmult #-}

type instance Actor (Array s a) = a

-- | outer product
-- >>> outer v v
-- [[1, 2, 3],
--  [2, 4, 6],
--  [3, 6, 9]]
--
outer :: forall s s' a.
  ( CommutativeRing a
  , HasShape s
  , HasShape s'
  , HasShape ((++) s s'))
  => Array s a
  -> Array s' a
  -> Array ((++) s s') a
outer m n = tabulate (\i -> index m (take dimm i) * index n (drop dimm i))
  where
    dimm = length (shape m)

-- |
--
-- >>> slice [[0,1],[2],[1,2]] a :: Array '[2,1,2] Int
-- [[[10, 11]],
--  [[22, 23]]]
slice ::
     (HasShape s, HasShape s')
  => [[Int]]
  -> Array s a
  -> Array s' a
slice s t = Array (fromList [index t i | i <- sequence s])

-- |
--
-- >>> let a = [1..24] :: Array '[2,1,3,4,1] Int
-- >>> a
-- [[[[[1],
--     [2],
--     [3],
--     [4]],
--    [[5],
--     [6],
--     [7],
--     [8]],
--    [[9],
--     [10],
--     [11],
--     [12]]]],
--  [[[[13],
--     [14],
--     [15],
--     [16]],
--    [[17],
--     [18],
--     [19],
--     [20]],
--    [[21],
--     [22],
--     [23],
--     [24]]]]]
-- >>> squeeze a
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]
--
squeeze ::
     forall s t a. (t ~ Squeeze s)
  => Array s a
  -> Array t a
squeeze (Array x) = Array x


