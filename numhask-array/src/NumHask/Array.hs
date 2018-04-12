{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- fixme
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE DatatypeContexts #-}

module NumHask.Array where

import Data.Distributive
import Data.Functor.Rep
import Data.Kind
import Data.List ((!!))
import Data.Promotion.Prelude
import Data.Singletons as S
import Data.Singletons.TypeLits as S
import GHC.Exts
import GHC.Show
import NumHask.Array.Constraints
import NumHask.Prelude as P
import NumHask.Shape
import Numeric.Dimensions as D
import Numeric.Dimensions.XDim
import qualified Data.Singletons.Prelude as S
import qualified Data.Vector as V
import qualified Protolude as Proto
import qualified Test.QuickCheck as QC

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeFamilies
-- >>> let a = [1..24] :: Array [] '[2,3,4] Int
-- >>> let v = [1,2,3] :: Array [] '[3] Int

-- | an array polymorphic in container and shape
--
-- >>> a
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]
data family Array (c :: Type -> Type) (ds :: [k]) (a :: Type)

-- | instance where dimensions are known at compile time
newtype instance (Dimensions ds) =>
  Array c (ds :: [Nat]) t =
    Array { _getContainer :: c t}
    deriving (Functor, Foldable)

instance NFData (Array c ds t) where
  rnf a = seq a ()

-- | instance of array where some of the dimensions are known at compile time
-- it wraps an Array with some weird magic
data instance Array c (xds :: [XNat]) t = forall (ds :: [Nat]).
  ( FixedDim xds ds ~ ds
  , FixedXDim xds ds ~ xds
  , Dimensions ds) =>
  SomeArray (Array c ds t)

-- | an array with dimensions represented at the value level
newtype AnyArray c a = AnyArray ([Int], c a)

-- | convert an array with type-level shape to value-level shape
anyArray :: (Dimensions ds) => Array c ds a -> AnyArray c a
anyArray arr@(Array c) = AnyArray (shape arr, c)

-- | a sweet class of container with attributes necessary to supply the set of operations here
class (Functor f) => Container f where
  generate :: Int -> (Int -> a) -> f a
  idx :: f a -> Int -> a
  cslice :: Int -> Int -> f a -> f a
  zipWith :: (a -> a -> a) -> f a -> f a -> f a
    -- Chunks a container into a list of containers whose dimension are each i
  chunkItUp :: [f a] -> Int -> f a -> [f a]
  cfoldl' :: (b -> a -> b) -> b -> f a -> b
  cfoldr :: (a -> b -> b) -> b -> f a -> b
  cconcat :: [f a] -> f a

instance Container V.Vector where
  generate = V.generate
  idx = V.unsafeIndex
  cslice = V.unsafeSlice
  zipWith = V.zipWith
  chunkItUp acc i v =
    if null v
      then acc
      else let (c, r) = V.splitAt i v
           in chunkItUp (c : acc) i r
  cfoldl' = V.foldl'
  cfoldr = V.foldr
  cconcat = V.concat

instance Container [] where
  generate n g = take n $ g <$> [0 ..]
  idx = (!!)
  cslice d t = take t . drop d
  zipWith = P.zipWith
  chunkItUp acc i v =
    if null v
      then acc
      else let (c, r) = splitAt i v
           in chunkItUp (c : acc) i r
  cfoldl' = foldl'
  cfoldr = foldr
  cconcat = mconcat

instance (Eq (c t), Dimensions ds) => Eq (Array c ds t) where
    (Array a) == (Array b) = a == b

xdimList :: XDim ds -> [Int]
xdimList (XDim d) = dimList d

dimList :: Dim ds -> [Int]
dimList D = []
dimList (d :* ds) = dimList d ++ dimList ds
dimList (Dn :: Dim m) = [dimVal' @m]
dimList (Dx (Dn :: Dim m)) = [dimVal' @m]

instance (Dimensions r) => HasShape (Array c r) where
  type Shape (Array c r) = [Int]
  shape _ = dimList $ dim @r

instance HasShape (Array c (xds :: [XNat])) where
  type Shape (Array c xds) = [Int]
  shape (SomeArray a) = shape a

-- * shape helpers where dimensions ~ [Int]

-- | convert from n-dim shape index to a flat index
--
-- >>> ind [2,3,4] [1,1,1]
-- 17
ind :: [Int] -> [Int] -> Int
ind ns xs = sum $ P.zipWith (*) xs (drop 1 $ scanr (*) 1 ns)

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

instance forall r c. (Dimensions r, Container c) =>
  Distributive (Array c r) where
  distribute f = Array $ generate n $ \i -> fmap (\(Array v) -> idx v i) f
    where
      n = dimVal $ dim @r

instance forall r c. (Dimensions r, Container c) =>
  Representable (Array c r) where
  type Rep (Array c r) = [Int]
  tabulate f = Array $ generate (product ns) (f . unind ns)
    where
      ns = dimList $ dim @r
  index (Array xs) rs = xs `idx` ind ns rs
    where
      ns = dimList $ dim @r

-- | from flat list
instance
    ( Item (Array c r a) ~ Item (c a)
    , Dimensions r
    , AdditiveUnital a
    , IsList (c a)
    ) =>
    IsList (Array c r a) where
  type Item (Array c r a) = a
  fromList l = Array $ fromList $ take n $ l ++ repeat zero
    where
      n = dimVal (dim @r)
  toList (Array v) = GHC.Exts.toList v

instance (Show a, Show (Item (c a)), Container c, IsList (c a)) => Show (AnyArray c a) where
  show aa@(AnyArray (l,_)) = go (length l) aa
    where
      go n aa'@(AnyArray (l', c')) =
        case length l' of
          0 -> "[]"
          1 -> "[" ++ intercalate ", " (GHC.Show.show <$> GHC.Exts.toList c') ++ "]"
          x ->
            "[" ++
            intercalate
              (",\n" ++ replicate (n - x + 1) ' ')
              (go n <$> flatten1 aa') ++
            "]"

-- | convert the top layer of a SomeArray to a [SomeArray]
flatten1 :: (Container c) => AnyArray c a -> [AnyArray c a]
flatten1 (AnyArray (rep, v)) =
  (\s -> AnyArray (drop 1 rep, cslice (s * l) l v)) <$> ss
  where
    (n, l) =
      case rep of
        [] -> (0, 1)
        x:r -> (x, product r)
    ss = take n [0 ..]

instance (Show a, Show (Item (c a)), IsList (c a), Container c, Dimensions ds) => Show (Array c ds a) where
  show = GHC.Show.show . anyArray

type Vector c n = Array c '[ n]

type Matrix c m n = Array c '[ m, n]

instance
  ( IsList (c a)
  , Item (c a) ~ a
  , KnownNat n
  , AdditiveUnital (Vector c n a)
  , QC.Arbitrary a
  , AdditiveUnital a
  , Num a
  ) =>
  QC.Arbitrary (Vector c n a) where
  arbitrary = QC.frequency [(1, pure zero), (9, fromList <$> QC.vector n)]
    where
      n = fromInteger $ P.natVal (Proxy :: Proxy n)

instance
  ( IsList (c a)
  , Item (c a) ~ a
  , AdditiveUnital (Matrix c m n a)
  , KnownNat m
  , KnownNat n
  , QC.Arbitrary a
  , AdditiveUnital a
  , Num a
  ) =>
  QC.Arbitrary (Matrix c m n a) where
  arbitrary = QC.frequency [(1, pure zero), (9, fromList <$> QC.vector (m * n))]
    where
      n = fromInteger $ P.natVal (Proxy :: Proxy n)
      m = fromInteger $ P.natVal (Proxy :: Proxy m)


-- ** Operations
-- | outer product
--
-- todo: reconcile with numhask version
--
-- >>> v NumHask.Array.>< v
-- [[1, 2, 3],
--  [2, 4, 6],
--  [3, 6, 9]]
(><) :: forall c (r :: [Nat]) (s :: [Nat]) a.
  ( Container c
  , CRing a
  , Dimensions r
  , Dimensions s
  , Dimensions ((D.++) r s))
  => Array c r a
  -> Array c s a
  -> Array c ((D.++) r s) a
(><) m n = tabulate (\i -> index m (take dimm i) * index n (drop dimm i))
  where
    dimm = length (shape m)

-- | matrix multiplication
--
-- >>> let a = [1, 2, 3, 4] :: Array [] '[2, 2] Int
-- >>> let b = [5, 6, 7, 8] :: Array [] '[2, 2] Int
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
mmult :: forall c m n k a.
  ( Hilbert (Vector c k) a
  , Dimensions '[ m, k]
  , Dimensions '[ k, n]
  , Dimensions '[ m, n]
  , Container c
  , KnownNat m
  , KnownNat n
  , KnownNat k
  )
  => Matrix c m k a
  -> Matrix c k n a
  -> Matrix c m n a
mmult x y = tabulate (\[i, j] -> unsafeRow i x <.> unsafeCol j y)

-- | extract the row of a matrix
row :: forall c i a m n.
  ( Dimensions '[ m, n]
  , Container c
  , KnownNat m
  , KnownNat n
  , KnownNat i
  , ((S.<) i m) ~ 'True
  )
  => Proxy i
  -> Matrix c m n a
  -> Vector c n a
row i_ = unsafeRow i
  where
    i = (Proto.fromIntegral . S.fromSing . S.singByProxy) i_

unsafeRow :: forall c a m n.
  ( Container c
  , Dimensions '[ m, n])
  => Int
  -> Matrix c m n a
  -> Vector c n a
unsafeRow i t@(Array a) = Array $ cslice (i * n) n a
  where
    [_, n] = shape t

-- | extract the column of a matrix
col :: forall c j a m n.
  ( Dimensions '[ m, n]
  , Container c
  , KnownNat m
  , KnownNat n
  , KnownNat j
  , ((S.<) j n) ~ 'True
  )
  => Proxy j
  -> Matrix c m n a
  -> Vector c m a
col j_ = unsafeCol j
  where
    j = (Proto.fromIntegral . S.fromSing . S.singByProxy) j_

unsafeCol ::
     forall c a m n. (Container c, Dimensions '[ m, n])
  => Int
  -> Matrix c m n a
  -> Vector c m a
unsafeCol j t@(Array a) = Array $ generate m (\x -> a `idx` (j + x * n))
  where
    [m, n] = shape t

-- |
--
-- >>> unsafeIndex a [0,2,1]
-- 10
unsafeIndex :: (Container c, Dimensions r) => Array c r a -> [Int] -> a
unsafeIndex t@(Array a) i = a `idx` ind (shape t) i

-- |
--
-- >>> unsafeSlice [[0,1],[2],[1,2]] a :: Array [] '[2,1,2] Int
-- [[[10, 11]],
--  [[22, 23]]]
unsafeSlice ::
     (Container c, IsList (c a), Item (c a) ~ a, Dimensions r, Dimensions r0)
  => [[Int]]
  -> Array c r a
  -> Array c r0 a
unsafeSlice s t = Array (fromList [unsafeIndex t i | i <- sequence s])

-- | Slice xs = Map Length xs
type family Slice (xss :: [[Nat]]) :: [Nat] where
  Slice xss = Data.Promotion.Prelude.Map LengthSym0 xss

-- | AllLT xs n = All (n >) xs
data AllLTSym0 (a :: S.TyFun [Nat] (S.TyFun Nat Bool -> Type))

data AllLTSym1 (l :: [Nat]) (a :: S.TyFun Nat Bool)

type instance S.Apply AllLTSym0 l = AllLTSym1 l

type instance S.Apply (AllLTSym1 l) n =
     Data.Promotion.Prelude.All ((S.>@#@$$) n) l

-- |
--
-- todo: an ambiguous type variable has snuck in here somewhere
--
-- > slice (Proxy :: Proxy '[ '[0,1],'[2],'[1,2]]) a
-- [[[10, 11]],
--  [[22, 23]]]
{-
todo:
    • Expected kind ‘[[Nat]]’, but ‘s’ has kind ‘[Nat]’
    • In the first argument of ‘Slice’, namely ‘s’
      In the first argument of ‘Array’, namely ‘(Slice s)’
      In the type signature:
        slice :: forall c s r a.
                 (Container c,
                  Dimensions s,
                  Dimensions r,
                  And (ZipWith AllLTSym0 s r) ~  'True) =>
                 Proxy s -> Array c r a -> Array (Slice s) c a
-}
{-
slice ::
     forall c s r a. (Container c, Dimensions s, Dimensions r, S.And (S.ZipWith AllLTSym0 s r) ~ 'True)
  => Proxy s
  -> Array c r a
  -> Array (Slice s) c a
-}
slice s_ = unsafeSlice s
  where
    s = ((fmap . fmap) fromInteger . fromSing . singByProxy) s_

-- |
--
-- >>> foldAlong (Proxy :: Proxy 1) (\_ -> ([0..3] :: Array [] '[4] Int)) a
-- [[0, 1, 2, 3],
--  [0, 1, 2, 3]]
--
-- todo: resolution of a primitive and a scalar eg
--        Expected type: Array '[10] Int -> Array '[] Int
--        Actual type: Array '[10] (Array '[] Int) -> Array '[] Int
--
foldAlong ::
     forall c s vw uvw uw w a.
     ( Container c
     , KnownNat s
     , Dimensions uvw
     , uw ~ (Fold s uvw)
     , w ~ (Data.Promotion.Prelude.Drop 1 vw)
     , vw ~ (TailModule s uvw)
     )
  => Proxy s
  -> (Array c vw a -> Array c w a)
  -> Array c uvw a
  -> Array c uw a
foldAlong s_ f a@(Array v) =
  Array $
  cconcat
    (cfoldl'
       (\xs x ->
          let (Array vx) = f (Array x)
          in vx : xs)
       []
       md)
  where
    s = (Proto.fromIntegral . fromSing . singByProxy) s_
    md = chunkItUp [] (product $ drop s $ shape a) v

-- |
--
-- todo: No instance for (Container (Array [] '[]) error
--
-- > mapAlong (Proxy :: Proxy 0) (\x -> NumHask.Array.zipWith (*) x x) a
-- [[[1, 4, 9, 16],
--   [25, 36, 49, 64],
--   [81, 100, 121, 144]],
--  [[169, 196, 225, 256],
--   [289, 324, 361, 400],
--   [441, 484, 529, 576]]]
--
mapAlong ::
     forall c s uvw vw a.
     (Container c, KnownNat s, Dimensions uvw, vw ~ (HeadModule s uvw))
  => Proxy s
  -> (Array c vw a -> Array c vw a)
  -> Array c uvw a
  -> Array c uvw a
mapAlong s_ f a@(Array v) =
  Array $
  cconcat
    (cfoldl'
       (\xs x ->
          let (Array vx) = f (Array x)
          in vx : xs)
       []
       md)
  where
    s = (Proto.fromIntegral . fromSing . singByProxy) s_
    md = chunkItUp [] (product $ drop s $ shape a) v

-- |
--
-- >>> concatenate (Proxy :: Proxy 2) a a
-- [[[1, 2, 3, 4, 1, 2, 3, 4],
--   [5, 6, 7, 8, 5, 6, 7, 8],
--   [9, 10, 11, 12, 9, 10, 11, 12]],
--  [[13, 14, 15, 16, 13, 14, 15, 16],
--   [17, 18, 19, 20, 17, 18, 19, 20],
--   [21, 22, 23, 24, 21, 22, 23, 24]]]
--
concatenate ::
     forall c s r t a.
     ( Container c
     , SingI s
     , Dimensions r
     , Dimensions t
     , (IsValidConcat s t r) ~ 'True
     )
  => Proxy s
  -> Array c r a
  -> Array c t a
  -> Array c (Concatenate s t r) a
concatenate s_ r@(Array vr) t@(Array vt) =
  Array . cconcat $ (concat . reverse . P.transpose) [rm, tm]
  where
    s = (Proto.fromIntegral . fromSing . singByProxy) s_
    rm = chunkItUp [] (product $ drop s $ shape t) vt
    tm = chunkItUp [] (product $ drop s $ shape r) vr

-- |
--
-- >>> NumHask.Array.transpose a
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
--
transpose ::
     forall c s t a. (t ~ Transpose s, Container c, Dimensions s, Dimensions t)
  => Array c s a
  -> Array c t a
transpose (Array x) = Array x

-- |
--
-- >>> let a = [1..24] :: Array [] '[2,1,3,4,1] Int
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
     forall c s t a. (t ~ Squeeze s)
  => Array c s a
  -> Array c t a
squeeze (Array x) = Array x

instance (Dimensions r, Container c, AdditiveMagma a) =>
         AdditiveMagma (Array c r a) where
  plus = liftR2 plus

instance (Dimensions r, Container c, AdditiveUnital a) =>
         AdditiveUnital (Array c r a) where
  zero = pureRep zero

instance (Dimensions r, Container c, AdditiveAssociative a) =>
         AdditiveAssociative (Array c r a)

instance (Dimensions r, Container c, AdditiveCommutative a) =>
         AdditiveCommutative (Array c r a)

instance (Dimensions r, Container c, AdditiveInvertible a) =>
         AdditiveInvertible (Array c r a) where
  negate = fmapRep negate

instance (Dimensions r, Container c, Additive a) => Additive (Array c r a)

instance (Dimensions r, Container c, AdditiveGroup a) =>
         AdditiveGroup (Array c r a)

instance (Dimensions r, Container c, MultiplicativeMagma a) =>
         MultiplicativeMagma (Array c r a) where
  times = liftR2 times

instance (Dimensions r, Container c, MultiplicativeUnital a) =>
         MultiplicativeUnital (Array c r a) where
  one = pureRep one

instance (Dimensions r, Container c, MultiplicativeAssociative a) =>
         MultiplicativeAssociative (Array c r a)

instance (Dimensions r, Container c, MultiplicativeCommutative a) =>
         MultiplicativeCommutative (Array c r a)

instance (Dimensions r, Container c, MultiplicativeInvertible a) =>
         MultiplicativeInvertible (Array c r a) where
  recip = fmapRep recip

instance (Dimensions r, Container c, Multiplicative a) =>
         Multiplicative (Array c r a)

instance (Dimensions r, Container c, MultiplicativeGroup a) =>
         MultiplicativeGroup (Array c r a)

instance (Dimensions r, Container c, MultiplicativeMagma a, Additive a) =>
         Distribution (Array c r a)

instance (Dimensions r, Container c, Semiring a) => Semiring (Array c r a)

instance (Dimensions r, Container c, Ring a) => Ring (Array c r a)

instance (Dimensions r, Container c, CRing a) => CRing (Array c r a)

instance (Dimensions r, Container c, Field a) => Field (Array c r a)

instance (Dimensions r, Container c, ExpField a) => ExpField (Array c r a) where
  exp = fmapRep exp
  log = fmapRep log

instance (Foldable (Array c r), Dimensions r, Container c, BoundedField a) =>
         BoundedField (Array c r a) where
  isNaN f = or (fmapRep isNaN f)

instance (Dimensions r, Container c, Signed a) => Signed (Array c r a) where
  sign = fmapRep sign
  abs = fmapRep abs

instance (Functor (Array c r), Foldable (Array c r), Normed a a, ExpField a) =>
         Normed (Array c r a) a where
  normL1 r = foldr (+) zero $ normL1 <$> r
  normL2 r = sqrt $ foldr (+) zero $ (** (one + one)) <$> r
  normLp p r = (** (one / p)) $ foldr (+) zero $ (** p) . normL1 <$> r

instance (Eq (c a), Foldable (Array c r), Dimensions r, Container c, Epsilon a) =>
         Epsilon (Array c r a) where
  nearZero f = and (fmapRep nearZero f)
  aboutEqual a b = and (liftR2 aboutEqual a b)

instance (Foldable (Array c r), Dimensions r, Container c, ExpField a, Normed a a) =>
         Metric (Array c r a) a

instance (Dimensions r, Container c, Integral a) => Integral (Array c r a) where
  divMod a b = (d, m)
    where
      x = liftR2 divMod a b
      d = fmap fst x
      m = fmap snd x

instance (Foldable (Array c r), CRing a, Semiring a, Dimensions r, Container c) =>
         Hilbert (Array c r) a where
  a <.> b = sum $ liftR2 (*) a b

instance (Dimensions r, Container c, Additive a) =>
         AdditiveBasis (Array c r) a where
  (.+.) = liftR2 (+)

instance (Dimensions r, Container c, AdditiveGroup a) =>
         AdditiveGroupBasis (Array c r) a where
  (.-.) = liftR2 (-)

instance (Dimensions r, Container c, Multiplicative a) =>
         MultiplicativeBasis (Array c r) a where
  (.*.) = liftR2 (*)

instance (Dimensions r, Container c, MultiplicativeGroup a) =>
         MultiplicativeGroupBasis (Array c r) a where
  (./.) = liftR2 (/)

instance (Dimensions r, Container c, Additive a) =>
         AdditiveModule (Array c r) a where
  (.+) r s = fmap (s +) r
  (+.) s = fmap (s +)

instance (Dimensions r, Container c, AdditiveGroup a) =>
         AdditiveGroupModule (Array c r) a where
  (.-) r s = fmap (\x -> x - s) r
  (-.) s = fmap (\x -> x - s)

instance (Dimensions r, Container c, Multiplicative a) =>
         MultiplicativeModule (Array c r) a where
  (.*) r s = fmap (s *) r
  (*.) s = fmap (s *)

instance (Dimensions r, Container c, MultiplicativeGroup a) =>
         MultiplicativeGroupModule (Array c r) a where
  (./) r s = fmap (/ s) r
  (/.) s = fmap (/ s)

instance (Dimensions r, Container c) => Singleton (Array c r) where
  singleton = pureRep

instance ( Foldable (Array c r)
         , Dimensions r
         , Container c
         , CRing a
         , Multiplicative a
         ) =>
         TensorProduct (Array c r a) where
  (><) m n = tabulate (\i -> index m i *. n)
  timesleft v m = tabulate (\i -> v <.> index m i)
  timesright m v = tabulate (\i -> v <.> index m i)
