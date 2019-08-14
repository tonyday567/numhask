{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module NumHask.Vector where

import Data.Distributive (Distributive(..))
import Data.Functor.Rep
import GHC.Show (Show(..))
-- import NumHask.Error (impossible)
-- import NumHask.Array.Constraints
--  (Fold, HeadModule, TailModule, IsValidConcat, Concatenate, Transpose, Squeeze)
import NumHask.Prelude as P
-- import NumHask.Shape (HasShape(..))
-- import Numeric.Dimensions as D
-- import qualified Data.Singletons.Prelude as S
import qualified Data.Vector as V
-- import qualified Data.Vector.Generic.Sized as S
-- import qualified Data.Vector.Generic.Base as B
-- import Data.Finite
import GHC.Exts

-- concrete vector based on vector-sized

{-
-}

newtype Vector n a = Vector { unVector :: V.Vector a} deriving (Eq, Show, Functor, Foldable)

instance (KnownNat n) => Data.Distributive.Distributive (Vector n) where
  distribute = distributeRep
  {-# inline distribute #-}

instance (KnownNat n) => Representable (Vector n) where
  type Rep (Vector n) = Int
  tabulate = Vector . V.generate n
    where
      n = fromIntegral $ natVal @n Proxy
  {-# inline tabulate #-}
  index (Vector v) = V.unsafeIndex v
  {-# inline index #-}

instance (NFData a) => NFData (Vector n a) where
  rnf (Vector a) = rnf a

-- lift2 :: (a -> a -> a) -> Vector n a -> Vector n a -> Vector n a
-- lift2 f (Vector v) (Vector v') = Vector $ S.zipWith f v v'

instance
  ( Additive a
  , KnownNat n)
  => Additive (Vector n a) where
  (+) (Vector v) (Vector v') = Vector $ V.zipWith (+) v v'
  zero = Vector $ V.replicate n zero
    where
      n = fromIntegral $ natVal @n Proxy

instance
  ( Multiplicative a
  , KnownNat n)
  => Multiplicative (Vector n a) where
  (*) (Vector v) (Vector v') = Vector $ V.zipWith (*) v v'
  one = Vector $ V.replicate n one
    where
      n = fromIntegral $ natVal @n Proxy

type instance Actor (Vector n a) = a

instance (KnownNat n, Foldable (Vector n), P.Distributive a, CommutativeRing a, Semiring a) =>
  Hilbert (Vector n a) where
  a <.> b = sum $ liftR2 (*) a b
  {-# inline (<.>) #-}

-- version with Zero and Singleton constructors
data Vector' n a =
  Zero |
  Singleton a |
  Vector' (V.Vector a) deriving (Eq, Show, Functor, Foldable)

instance KnownNat n => Data.Distributive.Distributive (Vector' n) where
  distribute = distributeRep
  {-# inline distribute #-}

instance KnownNat n => Representable (Vector' n) where
  type Rep (Vector' n) = Int
  tabulate = Vector' . V.generate n
    where
      n = fromIntegral $ natVal @n Proxy
  {-# inline tabulate #-}
  index (Vector' s) = V.unsafeIndex s
  {-# inline index #-}

instance (NFData a) => NFData (Vector' n a) where
  rnf (Vector' a) = rnf a

instance
  ( Additive a
  , KnownNat n)
  => Additive (Vector' n a) where
  (+) (Vector' v) (Vector' v') = Vector' $ V.zipWith (+) v v'
  zero = Vector' $ V.replicate n zero
    where
      n = fromIntegral $ natVal @n Proxy

instance
  ( Multiplicative a
  , KnownNat n)
  => Multiplicative (Vector' n a) where
  (*) (Vector' v) (Vector' v') = Vector' $ V.zipWith (*) v v'
  one = Vector' $ V.replicate n one
    where
      n = fromIntegral $ natVal @n Proxy

type instance Actor (Vector' n a) = a

instance (KnownNat n, Foldable (Vector' n), P.Distributive a, CommutativeRing a, Semiring a) =>
  Hilbert (Vector' n a) where
  a <.> b = sum $ liftR2 (*) a b
  {-# inline (<.>) #-}

-- | from flat list
instance
    ( KnownNat n
    , Additive a
    ) =>
    IsList (Vector n a) where
  type Item (Vector n a) = a
  fromList l = Vector $ V.fromList $ take n $ l ++ repeat zero
    where
      n = fromIntegral $ natVal @n Proxy

  toList (Vector v) = V.toList v

-- | from flat list
instance
    ( KnownNat n
    , Additive a
    ) =>
    IsList (Vector' n a) where
  type Item (Vector' n a) = a
  fromList l =
    Vector' $ V.fromList $ take n $ l ++ repeat zero
    where
      n = fromIntegral $ natVal @n Proxy

  toList (Vector' v) = V.toList v
  toList Zero = []
  toList (Singleton s) = [s]

