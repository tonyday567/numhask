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
import Data.Distributive (Distributive(..))
import Data.Functor.Rep
import GHC.Exts (IsList(..))
import GHC.Show (Show(..))
-- import Data.List (last)
-- import NumHask.Error (impossible)
-- import NumHask.Array.Constraints
--  (Fold, HeadModule, TailModule, IsValidConcat, Concatenate, Transpose, Squeeze)
import NumHask.Prelude as P
import NumHask.Shape
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

type Transpose (s :: [Nat]) = Reverse s '[]

-- revrse indices eg copy Aijk to Akji
transpose :: forall a s. (HasShape s, HasShape (Reverse s '[])) => Array s a -> Array (Transpose s) a
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

-- | select an index along a dimension
-- >>> :t select (Proxy 1) 1 ([1..] :: Array '[2,3,4])
-- Array '[2,4]
-- [[5, 6, 7, 8],
--  [17, 18, 19, 20]
-- ]
select :: forall dim s s' a. (HasShape s', s' ~ DropIndex s dim, HasShape s, KnownNat dim) => Proxy dim -> Int -> Array s a -> Array (DropIndex s dim) a
select _ i a = tabulate go
  where
    go s' = index a (let (x1,x2) = splitAt dim s' in x1++(i:x2))
    dim = fromIntegral $ natVal @dim Proxy

-- | select an index along a dimension
-- >>> :t select (Proxy 1) 1 ([1..] :: Array '[2,3,4])
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
