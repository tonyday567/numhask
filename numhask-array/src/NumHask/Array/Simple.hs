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

-- import Control.Category (id)
import qualified GHC.TypeNats
import Data.Distributive (Distributive(..))
import Data.Functor.Rep
import GHC.Exts (IsList(..))
import GHC.Show (Show(..))
-- import Data.List (last)
-- import NumHask.Error (impossible)
-- import NumHask.Array.Constraints
--  (Fold, HeadModule, TailModule, IsValidConcat, Concatenate, Transpose, Squeeze)
import NumHask.Prelude as P hiding (outer)
import NumHask.Shape
import NumHask.Array.Constraints
import qualified NumHask.Array.Dynamic as D
-- import qualified Numeric.LinearAlgebra.Data as H
-- import qualified Numeric.LinearAlgebra as H
-- import qualified Numeric.LinearAlgebra.HMatrix as OldH
-- import qualified Numeric.LinearAlgebra.Devel as H
-- import qualified Prelude
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

-- | <https://en.wikipedia.org/wiki/Scalarr_(mathematics) Scalar> is rank 0 of tensor
type Scalar a  = Array '[] a

-- | <https://en.wikipedia.org/wiki/Vector_(mathematics_and_physics) Vector> is rank 1 of tensor
type Vector s a = Array '[s] a

-- | <https://en.wikipedia.org/wiki/Matrix_(mathematics) Matrix> is rank 2 of tensor
type Matrix m n a = Array '[m,n] a

shape :: forall a s. (HasShape s) => Array s a -> [Int]
shape _ = shapeVal $ toShape @s
{-# inline shape #-}

toDArray a = D.fromFlatList (shape a) (P.toList a)

instance (HasShape s, Show a, Additive a) => Show (Array s a) where
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

-- revrse indices eg copy Aijk to Akji
transpose :: forall a s. (HasShape s, HasShape (Reverse s)) => Array s a -> Array (Reverse s) a
transpose a = tabulate (index a . shapen s' . flatten s)
  where
    s = shapeVal (toShape @s)
    s' = reverse s

identity :: forall a s. (HasShape s, Additive a, Multiplicative a) => Array s a
identity = tabulate (bool zero one (all' s))
  where
    s = shapeVal (toShape @s)
    all' [] = True
    all' [_] = True
    all' [x,y] = x==y
    all' (x:y:xs) = all' [x,y] && all' (y:xs)

singleton :: (HasShape s) => a -> Array s a
singleton a = tabulate (const a)

-- | select an index along a (single) dimension
-- >>> let s = select (Proxy :: Proxy 1) 1 ([1..] :: Array '[2,3,4] Int)
-- >>> :t s
-- s :: Array '[2, 4] Int
--
-- >>> s
-- [[5, 6, 7, 8],
--  [17, 18, 19, 20]]
select :: forall dim s s' a.
  ( HasShape s'
  , s' ~ DropIndex s dim
  , HasShape s
  , KnownNat dim
  ) => Proxy dim
  -> Int
  -> Array s a
  -> Array (DropIndex s dim) a
select _ i a = tabulate go
  where
    go s' = index a (let (x1,x2) = splitAt dim s' in x1++(i:x2))
    dim = fromIntegral $ natVal @dim Proxy

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
    go s' = index a (xs s')
    ds = shapeVal (toShape @ds)
    xs s' =
      foldl' (\acc (x, i0) ->
                take x acc ++ (List.!!) i i0:drop x acc) s' (zip ds [0..])

-- | extract a dimension
-- >>> let e = extract (Proxy :: Proxy 1) ([1..] :: Array '[2,3,4] Int)
-- >>> :t e
-- e :: Array '[3] (Array '[2, 4] Int)
--
extract :: forall dim s s' ss a.
  ( HasShape s'
  , s' ~ DropIndex s dim
  , ss ~ SelectIndex s dim
  , HasShape s
  , HasShape ss
  , KnownNat dim
  ) => Proxy dim
  -> Array s a
  -> Array ss (Array s' a)
extract d a = tabulate go
  where
    go [s'] = select d s' a

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

-- | join a single outer dimension layer
-- >>> let e = extract (Proxy :: Proxy 1) ([1..] :: Array '[2,3,4] Int)
-- >>> let j = NumHask.Array.Simple.join (Proxy :: Proxy 1) e
-- >>> :t j
-- j :: Array '[2, 3, 4] Int
--
join :: forall dim s s' si a.
  ( HasShape s'
  , s' ~ AddIndex s dim si
  , HasShape s
  , HasShape si
  , KnownNat dim
  ) => Proxy dim
  -> Array si (Array s a)
  -> Array s' a
join _ a = tabulate go
  where
    go s' =
      index
      (index a (take (length si) (drop dim s')))
      (take dim s' ++ drop (dim + length si) s')
    dim = fromIntegral $ natVal @dim Proxy
    si = shapeVal (toShape @si)

-- | join an inner and outer dimension layers
-- >>> let e = extracts (Proxy :: Proxy '[2,0]) ([1..] :: Array '[2,3,4] Int)
--
-- >>> :t e
-- e :: Array '[4, 2] (Array '[3] Int)
--
-- > let j = joins (Proxy :: Proxy '[2,0]) e
-- > :t j
-- j :: Array '[2, 3, 4] Int
--
joins :: forall ds si st so a.
  ( HasShape st
  , HasShape ds
  , st ~ AddIndexes si ds so
  , HasShape si
  , HasShape so
  )
  => Proxy ds
  -> Array so (Array si a)
  -> Array st a
joins _ a = tabulate go
  where
    go s' = index (index a (outer' s')) (inner' s')
    ds = shapeVal (toShape @ds)
    outer' s' = fmap (s' List.!!) ds
    inner' s' =
      reverse $
      foldl' (\acc (x, i0) -> bool (x:acc) acc (i0 `List.elem` ds))
      [] (zip s' [0..])
-- let ds = [2,0]
-- let si = [4,2]
-- let s = [3]
-- s' = [2,3,4]

-- |
--
-- > foldAlong (Proxy :: Proxy 1) (\_ -> ([0..3] :: Array '[4] Int)) a
-- [[0, 1, 2, 3],
--  [0, 1, 2, 3]]
--
foldAlong ::
     forall dim s s' ss a b.
     ( KnownNat dim
     , HasShape s'
     , s' ~ DropIndex s dim
     , ss ~ SelectIndex s dim
     , HasShape s
     , HasShape ss
     )
  => Proxy dim
  -> (Array s' a -> b)
  -> Array s a
  -> Array ss b
foldAlong dim_ f a = fmap f (extract dim_ a)

{-
-- | select an index along a dimension
-- > :t select (Proxy 1) 1 ([1..] :: Array '[2,3,4] Int)
-- Array '[2,4]
-- [[5, 6, 7, 8],
--  [17, 18, 19, 20]
-- ]
unsafeSelect :: forall s s' a. (HasShape s', HasShape s) => Int -> Int -> Array s a -> Array s' a
unsafeSelect d i (Array a) = Array $ V.generate (size s) go
  where
    go flats = V.unsafeIndex a (let (x1,x2) = splitAt d (shapen s' flats) in flatten s (x1++(i:x2)))
    s = shapeVal (toShape @s)
    s' = shapeVal (toShape @s')
-}


row  :: forall m n a. (KnownNat m, KnownNat n, HasShape '[m,n]) => Int -> Matrix m n a -> Vector n a
row i (Array a) = Array $ V.slice (i * n) n a
  where
    n = fromIntegral $ natVal @n Proxy

col  :: forall m n a. (KnownNat m, KnownNat n, HasShape '[m,n]) => Int -> Matrix m n a -> Vector n a
col i (Array a) = Array $ V.generate m (\x -> V.unsafeIndex a (i + x * n))
  where
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy

-- swapaxes
-- dyad (outer product?), ><
-- dot (mmult + vector dot?)
-- contraction
-- > dot a b == contraction (dyad a b) (rank a - 1, rank a)
-- slice
-- expand ???
-- concatenate
-- insert
-- append
-- squeeze
-- foldAlong
-- mapAlong

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
  , HasShape s
  )
  => Multiplicative (Array s a) where
  (*) = liftR2 (*)
  one = pureRep one

instance
  ( Divisive a
  , HasShape s
  )
  => Divisive (Array s a) where
  recip = fmapRep recip


instance
  ( P.Distributive a
  , HasShape s
  )
  => P.Distributive (Array s a) where

instance
  ( IntegralDomain a
  , HasShape s
  )
  => IntegralDomain (Array s a) where


instance
  ( Field a
  , HasShape s
  )
  => Field (Array s a) where


instance
  ( ExpField a
  , HasShape s
  )
  => ExpField (Array s a) where
  exp = fmapRep exp
  log = fmapRep log
  (**) = liftR2 (**)

type instance Actor (Array s a) = a

instance
  ( Multiplicative a
  , HasShape s
  )
  => HadamardMultiplication (Array s) a where

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
    go [i,j] = V.foldl' (+) zero $ V.zipWith (*) (V.slice (fromIntegral i * k) k x) (V.generate k (\x' -> y V.! (fromIntegral j + x' * n)))
    -- m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy
    k = fromIntegral $ natVal @k Proxy
{-# inline mmult #-}

mmult' :: forall m n k a.
  ( KnownNat k
  , KnownNat m
  , KnownNat n
  , HasShape [m,n]
  , Ring a
  )
  => Array [m, k] a
  -> Array [k, n] a
  -> Array [m, n] a
mmult' (Array x) (Array y) = tabulate go
  where
    go [i,j] = sum $ V.zipWith (*) (V.slice (i * n) n x) (V.generate m (\x' -> y V.! (j + x' * n)))
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy
{-# inline mmult' #-}

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

