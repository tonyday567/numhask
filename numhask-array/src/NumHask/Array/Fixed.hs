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

module NumHask.Array.Fixed where

import GHC.TypeLits
import Data.Distributive (Distributive(..))
import Data.Functor.Rep
import GHC.Exts (IsList(..))
import GHC.Show (Show(..))
import NumHask.Prelude as P hiding (transpose, outer, identity, singleton)
import NumHask.Array.Shape
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
-- >>> let m = [0..11] :: Array '[3,4] Int

-- | a multidimensional array with a type-level shape
--
-- >>> a
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]
-- >>> [1,2,3] :: Array '[2,2] Int
-- [[*** Exception: NumHaskException {errorMessage = "shape mismatch"}
--
newtype Array s a = Array { unArray :: V.Vector a} deriving (Eq, Ord, NFData, Functor, Foldable, Generic, Traversable)

-- | <https://en.wikipedia.org/wiki/Scalarr_(mathematics) Scalar>
type Scalar a  = Array '[] a

-- | <https://en.wikipedia.org/wiki/Vector_(mathematics_and_physics) Vector>
type Vector s a = Array '[s] a

-- | <https://en.wikipedia.org/wiki/Matrix_(mathematics) Matrix>
type Matrix m n a = Array '[m,n] a

-- | get shape as a value
-- >>> shape a
-- [2,3,4]
shape :: forall a s. (HasShape s) => Array s a -> [Int]
shape _ = shapeVal $ toShape @s
{-# inline shape #-}

toDArray :: (HasShape s) => Array s a -> D.DArray a
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

-- | from flat list
instance
    ( HasShape s
    , Additive a
    ) =>
    IsList (Array s a) where
  type Item (Array s a) = a
  fromList l =
    bool
    (throw (NumHaskException "shape mismatch"))
    (Array $ V.fromList l)
    (length l == mn)
    where
      mn = P.product $ shapeVal (toShape @s)

  toList (Array v) = V.toList v

-- operations

-- >>> reshape a :: Array '[4,3,2] Int
-- [[[1, 2],
--   [3, 4],
--   [5, 6]],
--  [[7, 8],
--   [9, 10],
--   [11, 12]],
--  [[13, 14],
--   [15, 16],
--   [17, 18]],
--  [[19, 20],
--   [21, 22],
--   [23, 24]]]
reshape :: forall a s s'. (Product s ~ Product s', HasShape s, HasShape s') => Array s a -> Array s' a
reshape a = tabulate (index a . shapen s . flatten s')
    where
      s = shapeVal (toShape @s)
      s' = shapeVal (toShape @s')

-- reverse indices eg transpose Aijk to Akji
-- >>> index (transpose a) [1,0,0] == index a [0,0,1]
-- True
transpose :: forall a s. (HasShape s, HasShape (Reverse s)) => Array s a -> Array (Reverse s) a
transpose a = tabulate (index a . reverse)

-- |
-- >>> ident :: Array '[3,2] Int
-- [[1, 0],
--  [0, 1],
--  [0, 0]]
--
ident :: forall a s. (HasShape s, Additive a, Multiplicative a) => Array s a
ident = tabulate (bool zero one . isDiag)

-- |
-- >>> diag (ident :: Array '[3,2] Int)
-- [1, 1]
--
diag :: forall a s.
  ( HasShape s
  , HasShape '[Min s]
  )
  => Array s a
  -> Array '[Min s] a
diag a = tabulate go where
  go [s'] = index a (replicate (length ds) s')
  ds = shapeVal (toShape @s)

-- |
-- >>> singleton one :: Array '[3,2] Int
-- [[1, 1],
--  [1, 1],
--  [1, 1]]
--
singleton :: (HasShape s) => a -> Array s a
singleton a = tabulate (const a)

-- | select an index along dimensions
-- >>> let s = selects (Proxy :: Proxy '[0,1]) [1,1] a
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

-- | select an index except along dimensions
-- >>> let s = selects' (Proxy :: Proxy '[2]) [1,1] a
-- >>> :t s
-- s :: Array '[4] Int
--
-- >>> s
-- [17, 18, 19, 20]
selects' :: forall ds s s' a.
  ( HasShape s
  , HasShape ds
  , HasShape s'
  , s' ~ TakeIndexes s ds
  ) => Proxy ds
  -> [Int]
  -> Array s a
  -> Array s' a
selects' _ i a = tabulate go
  where
    go s' = index a (addIndexes i (zip ds s'))
    ds = shapeVal (toShape @ds)

-- | fold along specified dimensions
-- >>> folds sum (Proxy :: Proxy '[1]) a
-- [68, 100, 132]
--
folds :: forall ds st si so a b.
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
folds f d a = tabulate go
  where
    go s' = f (selects d s' a)

-- | maps along specified dimensions
-- > maps (transpose) (Proxy :: Proxy '[1]) a
--
maps :: forall ds st st' si si' so a b.
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
maps f d a = joins d (fmapRep f (extracts d a))

-- | extracts dimensions to an outer layer
-- >>> let e = extracts (Proxy :: Proxy '[1,2]) a
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

-- | extracts dimensions to an inner layer
-- >>> let e = extracts' (Proxy :: Proxy '[1,2]) a
-- >>> :t e
-- e :: Array '[2] (Array '[3, 4] Int)
--
extracts' :: forall ds st si so a.
  ( HasShape st
  , HasShape ds
  , HasShape si
  , HasShape so
  , so ~ DropIndexes st ds
  , si ~ TakeIndexes st ds
  ) => Proxy ds
  -> Array st a
  -> Array so (Array si a)
extracts' d a = tabulate go
  where
    go s' = selects' d s' a

-- | join inner and outer dimension layers
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

-- | concatenate along a dimension
-- >>>  :t concatenate (Proxy :: Proxy 1) a a
-- concatenate (Proxy :: Proxy 1) a a :: Array '[2, 6, 4] Int
concatenate :: forall a s0 s1 d s.
  ( Rank s0 ~ Rank s1
    , DropIndex s0 d ~ DropIndex s1 d
    , CheckConcatenate d s0 s1
    , Concatenate d s0 s1 ~ s
    , HasShape s0
    , HasShape s1
    , HasShape s
    , KnownNat d)
  => Proxy d
  -> Array s0 a
  -> Array s1 a
  -> Array s a
concatenate _ s0 s1 = tabulate go where
  go s' = bool (index s0 s') (index s1 (addIndex (dropIndex s' d) d (i' s'))) ((s' List.!! d) >= (ds0 List.!! d))
  ds0 = shapeVal (toShape @s0)
  d = fromIntegral $ natVal @d Proxy
  i' s'' = (s'' List.!! d) - (ds0 List.!! d)

-- | `insert (Proxy :: Proxy dim) (Proxy :: Proxy i)` insert along the dimension dim at position i
-- >>> insert (Proxy :: Proxy 2) (Proxy :: Proxy 0) a ([100..105])
-- [[[100, 1, 2, 3, 4],
--   [101, 5, 6, 7, 8],
--   [102, 9, 10, 11, 12]],
--  [[103, 13, 14, 15, 16],
--   [104, 17, 18, 19, 20],
--   [105, 21, 22, 23, 24]]]
insert :: forall a d d' dim i.
  ( DropIndex d dim ~ d'
    , CheckInsert dim i d
    , KnownNat i
    , KnownNat dim
    , HasShape d
    , HasShape d'
    , HasShape (Insert dim d)
    )
  => Proxy dim
  -> Proxy i
  -> Array d a
  -> Array d' a
  -> Array (Insert dim d) a
insert _ _ a b = tabulate go where
  go s
    | s List.!! dim == i = index b (dropIndex s dim)
    | s List.!! dim < i = index a s
    | otherwise = index a (decAt dim s)
  -- ds0 = shapeVal (toShape @s0)
  dim = fromIntegral $ natVal @dim Proxy
  i = fromIntegral $ natVal @i Proxy

-- | insert along a dimension at the end
-- >>>  :t append (Proxy :: Proxy 0) a
-- append (Proxy :: Proxy 0) a
--   :: Array '[3, 4] Int -> Array '[3, 3, 4] Int
append
  :: forall a d d' dim.
    ( DropIndex d dim ~ d'
    , CheckInsert dim (Dimension d dim - 1) d
    , KnownNat (Dimension d dim - 1)
    , KnownNat dim
    , HasShape d
    , HasShape d'
    , HasShape (Insert dim d)
    )
  => Proxy dim
  -> Array d a
  -> Array d' a
  -> Array (Insert dim d) a
append dim = insert dim (Proxy :: Proxy (Dimension d dim - 1))

-- | change the order of dimensions
-- >>> let r = reorder (Proxy :: Proxy '[2,0,1]) a
-- >>> :t r
-- r :: Array '[4, 2, 3] Int
reorder
  :: forall a d dims.
    ( HasShape dims
    , HasShape d
    , HasShape (ReorderDims d dims)
    )
  => Proxy dims
  -> Array d a
  -> Array (ReorderDims d dims) a
reorder _ a = tabulate go where
  go s' = index a (addIndexes [] (zip dims s'))
  dims = shapeVal (toShape @dims)


-- | product two arrays using the supplied binary function
-- If the function is multiply, and the arrays are tensors,
-- then this can be interpreted as a tensor product.
-- https://en.wikipedia.org/wiki/Tensor_product
-- The concept of a tensor product is a dense crossroad, and a complete treatment is elsewhere.  To quote:
-- ... the tensor product can be extended to other categories of mathematical objects in addition to vector spaces, such as to matrices, tensors, algebras, topological vector spaces, and modules. In each such case the tensor product is characterized by a similar universal property: it is the freest bilinear operation. The general concept of a "tensor product" is captured by monoidal categories; that is, the class of all things that have a tensor product is a monoidal category.
-- >>> expand (*) v v
-- [[1, 2, 3],
--  [2, 4, 6],
--  [3, 6, 9]]
--
expand :: forall s s' a b c.
  ( HasShape s
  , HasShape s'
  , HasShape ((++) s s'))
  => (a -> b -> c)
  -> Array s a
  -> Array s' b
  -> Array ((++) s s') c
expand f m n = tabulate (\i -> f (index m (take dimm i)) (index n (drop dimm i)))
  where
    dimm = length (shape m)

-- | contract an array by applying the supplied (folding) function on diagonal elements of the dimensions
-- This generalises a tensor contraction by allowing the number of contracting diagonals to be other than 2, and allowing another binary other than addition
-- >>> let b = [1..6] :: Array '[2,3] Int
-- >>> contract sum (Proxy :: Proxy '[1,2]) (expand (*) b (transpose b))
-- [[14, 32],
--  [32, 77]]
--
contract :: forall a b s ss s' ds.
  ( KnownNat (Min (TakeIndexes s ds))
  , HasShape (TakeIndexes s ds)
  , HasShape s
  , HasShape ds
  , HasShape ss
  , HasShape s'
  , s' ~ DropIndexes s ds
  , ss ~ '[Min (TakeIndexes s ds)]
  )
  => (Array ss a -> b)
  -> Proxy ds
  -> Array s a
  -> Array s' b
contract f xs a = f . diag <$> extracts' xs a

-- | a generalisation of a dot operation, which is a multiplicative expansion of two arrays and sum contraction along the middle two dimensions.
-- >>> let b = [1..6] :: Array '[2,3] Int
-- >>> dot sum (*) b (transpose b)
-- [[14, 32],
--  [32, 77]]
--
-- dot sum (*) on two matrices is known as matrix multipliocation
-- dot sum (*) on two vectors is known as the inner product
-- dot sum (*) m v on a matrix and a vector is matrix-vector multiplication
dot :: forall a b c d sa sb s' ss se.
  ( HasShape sa
  , HasShape sb
  , HasShape (sa ++ sb)
  , se ~ TakeIndexes (sa ++ sb) '[Rank sa - 1, Rank sa]
  , HasShape se
  , KnownNat (Min se)
  , KnownNat (Rank sa - 1)
  , KnownNat (Rank sa)
  , ss ~ '[Min se]
  , HasShape ss
  , s' ~ DropIndexes (sa ++ sb) '[Rank sa - 1, Rank sa]
  , HasShape s'
  )
  => (Array ss c -> d)
  -> (a -> b -> c)
  -> Array sa a
  -> Array sb b
  -> Array s' d
dot f g a b = contract f (Proxy :: Proxy '[Rank sa - 1, Rank sa]) (expand g a b)

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


-- * NumHask heirarchy
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

instance (HasShape s, Multiplicative a) =>
  MultiplicativeAction (Array s a) where
  (.*) r s = fmap (*s) r
  {-# inline (.*) #-}
  (*.) s = fmap (s*)
  {-# inline (*.) #-}

-- * Matrix specializations
-- | extract specialised to a matrix
-- >>> row 1 m
-- [4, 5, 6, 7]
--
row :: forall m n a. (KnownNat m, KnownNat n, HasShape '[m,n]) => Int -> Matrix m n a -> Vector n a
row i (Array a) = Array $ V.slice (i * n) n a
  where
    n = fromIntegral $ natVal @n Proxy

-- | row extraction checked at type level
--
-- >>> safeRow (Proxy :: Proxy 1) m
-- [4, 5, 6, 7]
--
-- >>> safeRow (Proxy :: Proxy 3) m
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
-- >>> col 1 m
-- [1, 5, 9]
--
col  :: forall m n a. (KnownNat m, KnownNat n, HasShape '[m,n]) => Int -> Matrix m n a -> Vector n a
col i (Array a) = Array $ V.generate m (\x -> V.unsafeIndex a (i + x * n))
  where
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy

-- | column extraction checked at type level
--
-- >>> safeCol (Proxy :: Proxy 1) m
-- [1, 5, 9]
--
-- >>> safeCol (Proxy :: Proxy 4) m
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

-- | matrix multiplication
-- This is dot sum (*) specialised to matrices
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
