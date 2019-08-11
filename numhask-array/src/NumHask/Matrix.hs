{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
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

module NumHask.Matrix where

import Data.Distributive (Distributive(..))
import Data.Functor.Rep
import GHC.Exts (IsList(..))
import GHC.Show (Show(..))
-- import NumHask.Error (impossible)
-- import NumHask.Array.Constraints
--  (Fold, HeadModule, TailModule, IsValidConcat, Concatenate, Transpose, Squeeze)
import NumHask.Prelude as P
-- import NumHask.Shape (HasShape(..))
-- import Numeric.Dimensions as D
-- import qualified Data.Singletons.Prelude as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic.Sized as S
import qualified Data.Vector.Unboxed.Sized as U
import qualified Data.Vector.Generic.Sized.Internal as SI
-- import qualified Data.Vector.Generic.Base as B
import Data.Finite
import GHC.TypeLits
-- concrete vector based on vector-sized
import qualified Prelude (fromIntegral)
import qualified NumHask.Vector as NH
import qualified Data.Vec.Lazy as L
import qualified Data.Nat as N
import qualified Data.Fin as Fin
import qualified Data.Type.Nat as N
import qualified Data.List as List

newtype Matrix m n a = Matrix { unMatrix :: S.Vector V.Vector (m GHC.TypeLits.* n) a} deriving (Eq, Show, Ord, NFData, Functor, Foldable, Generic, Traversable)

newtype UMatrix m n a = UMatrix { unUMatrix :: U.Vector (m GHC.TypeLits.* n) a} deriving (Eq, Show)

newtype LMatrix m n a = LMatrix { unLMatrix :: L.Vec (N.Mult m n) a} deriving (Eq, Show, Ord, NFData, Functor, Foldable, Generic, Traversable)

shape :: forall a m n. (KnownNat m, KnownNat n) => Matrix m n a -> [Integer]
shape _ = [m, n]
  where
    m = natVal @m Proxy
    n = natVal @n Proxy

product :: forall a m n. (KnownNat m, KnownNat n) => Matrix m n a -> Integer
product _ = m * n
  where
    m = natVal @m Proxy
    n = natVal @n Proxy

instance
  ( KnownNat m
  , KnownNat n
  ) => Data.Distributive.Distributive (Matrix m n) where
  distribute = distributeRep
  {-# inline distribute #-}

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

instance forall m n.
  ( KnownNat m
  , KnownNat n
  ) => Representable (Matrix m n) where
  type Rep (Matrix m n) = (Integer, Integer)
  tabulate f =
    Matrix . S.generate $ (f . (`divMod` n) . getFinite)
    where
      -- m = natVal @m Proxy
      n = natVal @n Proxy
  {-# inline tabulate #-}
  index (Matrix s) i = S.index s (Prelude.fromIntegral $ ind [m,n] $ (\(x,y) -> [Prelude.fromIntegral x, Prelude.fromIntegral y]) i)
    where
      m = fromIntegral $ natVal @m Proxy
      n = fromIntegral $ natVal @n Proxy
  {-# inline index #-}

instance
  ( N.SNatI m
  , N.SNatI n
  , N.SNatI (N.Mult m n)
  ) => Data.Distributive.Distributive (LMatrix m n) where
  distribute = distributeRep
  {-# inline distribute #-}

instance forall m n.
  ( N.SNatI m
  , N.SNatI n
  , N.SNatI (N.Mult m n)
  ) => Representable (LMatrix m n) where
  type Rep (LMatrix m n) = (Integer, Integer)
  tabulate f =
    -- Matrix . S.generate $ (f . (`divMod` n) . getFinite)
    LMatrix . tabulate $ (f . (`divMod` n) . Prelude.fromIntegral)
    where
      -- m = Prelude.fromIntegral $ N.reflect (Proxy::Proxy m)
      n = Prelude.fromIntegral $ N.reflect (Proxy::Proxy n)
  {-# inline tabulate #-}
  index = undefined

instance (NFData a) => NFData (UMatrix m n a) where
  rnf (UMatrix a) = rnf a

instance
  ( Additive a
  , KnownNat m
  , KnownNat n
  )
  => Additive (Matrix m n a) where
  (+) = liftR2 (+)
  zero = pureRep zero

instance
  ( Multiplicative a
  , KnownNat m
  , KnownNat n
  )
  => Multiplicative (Matrix m n a) where
  (*) = liftR2 (*)
  one = pureRep one

type instance Actor (Matrix m n a) = a

instance
  ( KnownNat m
  , KnownNat n
  , Foldable (Matrix m n)
  , P.Distributive a
  , CommutativeRing a
  , Semiring a) =>
  Hilbert (Matrix m n a) where
  (<.>) a b = sum $ (*) a b
  {-# inline (<.>) #-}

instance (KnownNat m, KnownNat n, Multiplicative a) =>
  MultiplicativeAction (Matrix m n a) where
  (.*) r s = fmap (*s) r
  (*.) s = fmap (s*)

instance
  ( Foldable (Matrix m n)
  , KnownNat m
  , KnownNat n
  , CommutativeRing a
  , Multiplicative a
  ) =>
  TensorProduct (Matrix m n a) where
  (><) m n = tabulate (\i -> index m i *. n)
  timesleft v m = tabulate (\i -> v <.> index m i)
  timesright m v = tabulate (\i -> v <.> index m i)

-- | from flat list
instance
    ( KnownNat m
    , KnownNat n
    , Additive a
    ) =>
    IsList (Matrix m n a) where
  type Item (Matrix m n a) = a
  fromList l = Matrix $ SI.Vector $ V.fromList $ take mn $ l ++ repeat zero
    where
      mn = fromIntegral $ natVal @m Proxy * natVal @n Proxy

  toList (Matrix v) = S.toList v

-- | from flat list
instance
    ( KnownNat m
    , KnownNat n
    , Additive a
    , U.Unbox a
    ) =>
    IsList (UMatrix m n a) where
  type Item (UMatrix m n a) = a
  fromList l = UMatrix $ SI.Vector $ UV.fromList $ take mn $ l ++ repeat zero
    where
      mn = fromIntegral $ natVal @m Proxy * natVal @n Proxy

  toList (UMatrix v) = S.toList v

-- | from flat list
instance
    ( Additive a
    , N.SNatI m
    , N.SNatI n
    , N.SNatI (N.Mult m n)
    ) =>
    IsList (LMatrix m n a) where
  type Item (LMatrix m n a) = a
  fromList l = LMatrix l'
    where
      m = Prelude.fromIntegral $ N.reflect (Proxy::Proxy m)
      n = Prelude.fromIntegral $ N.reflect (Proxy::Proxy n)
      (Just l') = L.fromList $ take (m * n) $ l ++ repeat zero
  toList (LMatrix v) = L.toList v

mmult :: forall m n k a.
  ( KnownNat k
  , KnownNat m
  , KnownNat n
  , Ring a
  )
  => Matrix m k a
  -> Matrix k n a
  -> Matrix m n a
mmult (Matrix (SI.Vector x)) (Matrix (SI.Vector y)) = tabulate go
  where
    go (i,j) = V.foldl' (+) zero $ V.zipWith (*) (V.slice (fromIntegral i * n) n x) (V.generate m (\x' -> y V.! (fromIntegral j + x' * n)))
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy
      -- unsafeRow (Prelude.fromIntegral i) x <.> unsafeCol (Prelude.fromIntegral j) y
{-# inline mmult #-}

mmult' :: forall m n k a.
  ( KnownNat k
  , KnownNat m
  , KnownNat n
  , Ring a
  , U.Unbox a
  )
  => UMatrix m k a
  -> UMatrix k n a
  -> UMatrix m n a
mmult' (UMatrix (SI.Vector x)) (UMatrix (SI.Vector y)) = UMatrix $ SI.Vector $ UV.generate (m*n) go
  where
    go i = let (i', j') = i `divMod` n in UV.foldl' (+) zero $ UV.zipWith (*) (UV.slice (i' * n) n x) (UV.generate m (\x' -> y UV.! (j' + x' * n)))
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy
      -- unsafeRow (Prelude.fromIntegral i) x <.> unsafeCol (Prelude.fromIntegral j) y
{-# inline mmult' #-}

mmultVec :: forall m n k a.
  ( N.SNatI m
  , N.SNatI n
  , N.SNatI (N.Mult m n)
  , Ring a
  )
  => LMatrix m k a
  -> LMatrix k n a
  -> LMatrix m n a
mmultVec (LMatrix x) (LMatrix y) = tabulate go
  where
    go (i,j) = foldl' (+) zero $ zipWith (*) (drop (fromIntegral i * n) $ take n (L.toList x)) (fmap (\x' -> L.toList y List.!! (fromIntegral j + x' * n)) [1..m])
    m = Prelude.fromIntegral $ N.reflect (Proxy::Proxy m)
    n = Prelude.fromIntegral $ N.reflect (Proxy::Proxy n)

{-# inline mmultVec #-}



unsafeRow :: forall a m n.
  ( KnownNat m
  , KnownNat n)
  => Int
  -> Matrix m n a
  -> NH.Vector n a
unsafeRow i (Matrix (SI.Vector a)) = NH.Vector $ SI.Vector $ V.slice (i * n) n a
  where
    n = fromIntegral $ natVal @n Proxy
{-# inline unsafeRow #-}

unsafeCol ::
     forall a m n. (KnownNat m, KnownNat n)
  => Int
  -> Matrix m n a
  -> NH.Vector m a
unsafeCol j (Matrix (SI.Vector a)) =
  NH.Vector $ SI.Vector $ V.generate m (\x -> a V.! (j + x * n))
  where
    m = fromIntegral $ natVal @m Proxy
    n = fromIntegral $ natVal @n Proxy
{-# inline unsafeCol #-}
