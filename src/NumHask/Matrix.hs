{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

-- | Two-dimensional arrays. Two classes are supplied
--
-- - 'Matrix' where shape information is held at type level, and
-- - 'SomeMatrix' where shape is held at the value level.
--
-- In both cases, the underlying data is contained as a flat vector for efficiency purposes.
module NumHask.Matrix
  ( Matrix(..)
  , SomeMatrix(..)
    -- ** Conversion
  , someMatrix
  , unsafeToMatrix
  , toMatrix
  , unsafeFromVV
  , toVV
  , toCol
  , toRow
  , fromCol
  , fromRow
  , col
  , row
  , joinc
  , joinr
  , resize
  , reshape
    -- ** Operations
  , mmult
  , trans
  , getDiag
  , diagonal
  , mapc
  , mapr
    -- ** Arbitrary
  , ShapeM(..)
  ) where

import Data.Distributive as D
import Data.Functor.Rep
import Data.Proxy (Proxy(..))
import qualified Data.Vector as V
import GHC.Exts
import GHC.Show
import GHC.TypeLits
import Protolude (Ord(..), Fractional, Functor, Eq, Foldable, Traversable, Maybe(..), ($), (<$>), fmap, (.))
import NumHask.Algebra
import NumHask.Shape
import NumHask.Vector
import qualified Protolude as P
import Test.QuickCheck hiding (resize)
import Data.Singletons.Prelude.Num
import qualified Data.Matrix as Matrix

-- | A two-dimensional array where shape is specified at the type level
-- The main purpose of this, beyond safe typing, is to supply the Representable instance with an initial object.
-- A single Boxed 'Data.Vector.Vector' is used underneath for efficient slicing, but this may change or become polymorphic in the future.
--
-- todo: the natural type for a matrix, the output from a vector outer product for example, is a 'Vector' ('Vector' a).  We should be able to unify to a different representation such as this, using type families.
newtype Matrix m n a = Matrix
  { flattenMatrix :: V.Vector a
  } deriving (Functor, Eq, Foldable, Traversable)

instance forall m n. (KnownNat m, KnownNat n) =>
         HasShape (Matrix (m :: Nat) (n :: Nat)) where
  type Shape (Matrix m n) = (Int, Int)
  shape _ =
    ( P.fromInteger $ natVal (Proxy :: Proxy m)
    , P.fromInteger $ natVal (Proxy :: Proxy n))

instance (Show a, KnownNat m, KnownNat n) =>
         Show (Matrix (m :: Nat) (n :: Nat) a) where
  show m = "[" P.++ P.intercalate "\n " (P.toList (show <$> toVV m)) P.++ "]"

instance (KnownNat m, KnownNat n, Arbitrary a, AdditiveUnital a) =>
         Arbitrary (Matrix m n a) where
  arbitrary = frequency [(1, P.pure zero), (9, fromList <$> vector (m * n))]
    where
      n = P.fromInteger $ natVal (Proxy :: Proxy n)
      m = P.fromInteger $ natVal (Proxy :: Proxy m)

instance (KnownNat m, KnownNat n) => Distributive (Matrix m n) where
  distribute f =
    Matrix $
    V.generate (n * m) $ \i -> fmap (\(Matrix v) -> V.unsafeIndex v i) f
    where
      m = P.fromInteger $ natVal (Proxy :: Proxy m)
      n = P.fromInteger $ natVal (Proxy :: Proxy n)

instance (KnownNat m, KnownNat n) => Representable (Matrix m n) where
  type Rep (Matrix m n) = (P.Int, P.Int)
  tabulate f = Matrix $ V.generate (m * n) (\x -> f (divMod x n))
    where
      m = P.fromInteger $ natVal (Proxy :: Proxy m)
      n = P.fromInteger $ natVal (Proxy :: Proxy n)
  index (Matrix xs) (i0, i1) = xs V.! (i0 * n + i1)
    where
      n = P.fromInteger $ natVal (Proxy :: Proxy n)

-- | a two-dimensional array where shape is specified at the value level as a '(Int,Int)'
-- Use this to avoid type-level hasochism by demoting a 'Matrix' with 'someMatrix'
data SomeMatrix a =
  SomeMatrix (Int, Int)
             (V.Vector a)
  deriving (Functor, Eq, Foldable)

instance HasShape SomeMatrix where
  type Shape SomeMatrix = (Int, Int)
  shape (SomeMatrix sh _) = sh

instance (Show a) => Show (SomeMatrix a) where
  show (SomeMatrix _ v) = show (P.toList v)

-- ** conversion
-- | convert from a 'Matrix' to a 'SomeMatrix'
someMatrix ::
     (KnownNat m, KnownNat n) => Matrix (m :: Nat) (n :: Nat) a -> SomeMatrix a
someMatrix v = SomeMatrix (shape v) (flattenMatrix v)

-- | convert from a 'SomeMatrix' to a 'Matrix' with no shape check
unsafeToMatrix :: SomeMatrix a -> Matrix (m :: Nat) (n :: Nat) a
unsafeToMatrix (SomeMatrix _ v) = Matrix v

-- | convert from a 'SomeMatrix' to a 'Matrix', checking shape
toMatrix ::
     forall a m n. (KnownNat m, KnownNat n)
  => SomeMatrix a
  -> Maybe (Matrix (m :: Nat) (n :: Nat) a)
toMatrix (SomeMatrix s v) =
  if s P.== (m, n)
    then Just $ Matrix v
    else Nothing
  where
    m = P.fromInteger $ natVal (Proxy :: Proxy m)
    n = P.fromInteger $ natVal (Proxy :: Proxy n)

toDMatrix :: forall a m n. (KnownNat m, KnownNat n)
  => Matrix (m :: Nat) (n :: Nat) a
    -> Matrix.Matrix a
toDMatrix x = Matrix.matrix m n (\(i,j) -> index x (i - 1,j - 1))
  where
    m = P.fromInteger $ natVal (Proxy :: Proxy m)
    n = P.fromInteger $ natVal (Proxy :: Proxy n)

fromDMatrix :: forall a m n. (AdditiveUnital a, KnownNat m, KnownNat n)
  => Matrix.Matrix a
    -> Matrix (m :: Nat) (n :: Nat) a
fromDMatrix x = fromList $ Matrix.toList x

-- | from flat list
instance (KnownNat m, KnownNat n, AdditiveUnital a) =>
         IsList (Matrix m n a) where
  type Item (Matrix m n a) = a
  fromList l = Matrix $ V.fromList $ P.take (m * n) $ l P.++ P.repeat zero
    where
      m = P.fromInteger $ natVal (Proxy :: Proxy m)
      n = P.fromInteger $ natVal (Proxy :: Proxy n)
  toList (Matrix v) = V.toList v

-- | from nested list
instance IsList (SomeMatrix a) where
  type Item (SomeMatrix a) = [a]
  fromList l =
    SomeMatrix (P.length l, P.length $ P.head l) (V.fromList $ P.mconcat l)
  toList (SomeMatrix (m, n) v) =
    (\i -> V.toList $ V.unsafeSlice (i * n) n v) <$> [0 .. (m - 1)]

-- | conversion from a double Vector representation
unsafeFromVV ::
     forall a m n. ()
  => Vector m (Vector n a)
  -> Matrix m n a
unsafeFromVV vv = Matrix $ P.foldr ((V.++) . toVec) V.empty vv

-- | conversion to a double Vector representation
toVV ::
     forall a m n. (KnownNat m, KnownNat n)
  => Matrix m n a
  -> Vector m (Vector n a)
toVV m = tabulate (row m)

-- | convert a 'Vector' to a column 'Matrix'
toCol ::
     forall a n. ()
  => Vector n a
  -> Matrix 1 n a
toCol v = Matrix $ toVec v

-- | convert a 'Vector' to a row 'Matrix'
toRow ::
     forall a m. ()
  => Vector m a
  -> Matrix m 1 a
toRow v = Matrix $ toVec v

-- | convert a row 'Matrix' to a 'Vector'
fromCol ::
     forall a n. ()
  => Matrix 1 n a
  -> Vector n a
fromCol m = Vector $ flattenMatrix m

-- | convert a column 'Matrix' to a 'Vector'
fromRow ::
     forall a m. ()
  => Matrix m 1 a
  -> Vector m a
fromRow m = Vector $ flattenMatrix m

-- | extract a row from a 'Matrix' as a 'Vector'
row ::
     forall a m n. (KnownNat m, KnownNat n)
  => Matrix m n a
  -> Int
  -> Vector n a
row (Matrix a) i = Vector $ V.unsafeSlice (i * n) n a
  where
    n = P.fromInteger $ natVal (Proxy :: Proxy n)

-- | extract a column from a 'Matrix' as a 'Vector'
col ::
     forall a m n. (KnownNat m, KnownNat n)
  => Matrix m n a
  -> Int
  -> Vector m a
col (Matrix a) i = Vector $ V.generate m (\x -> a V.! (i + x * n))
  where
    m = P.fromInteger $ natVal (Proxy :: Proxy m)
    n = P.fromInteger $ natVal (Proxy :: Proxy n)


-- | resize matrix, appending with zero if needed
resize :: forall m0 m1 n0 n1 a. ( KnownNat m0, KnownNat m1
                 , KnownNat n0, KnownNat n1, AdditiveUnital a)
  => Matrix m0 n0 a
  -> Matrix m1 n1 a
resize m = tabulate
    (\(i,j) -> if i < m0 P.&& i < n0 then index m (i,j) else zero)
  where
    m0 = P.fromInteger $ natVal (Proxy :: Proxy m0)
    n0 = P.fromInteger $ natVal (Proxy :: Proxy n0)

-- | reshape matrix, appending with zero if needed
reshape :: forall m0 m1 n0 n1 a. ( KnownNat m0, KnownNat m1
                 , KnownNat n0, KnownNat n1, AdditiveUnital a)
  => Matrix m0 n0 a
  -> Matrix m1 n1 a
reshape (Matrix v) = tabulate
    (\(i,j) -> if i*n0+j < (m0*n0) then v V.! (i*n0+j) else zero)
  where
    m0 = P.fromInteger $ natVal (Proxy :: Proxy m0)
    n0 = P.fromInteger $ natVal (Proxy :: Proxy n0)



-- ** Operations
-- | matrix transposition
--
-- trans . trans == identity
trans ::
     forall m n a. (KnownNat m, KnownNat n)
  => Matrix m n a
  -> Matrix n m a
trans x = tabulate (\(i, j) -> index x (j,i))

-- | extract the matrix diagonal as a vector
--
-- > getDiag one == one
getDiag ::
     forall n a. (KnownNat n)
  => Matrix n n a
  -> Vector n a
getDiag x = tabulate (\i -> index x (i,i))

-- | create a matrix using a vector as the diagonal
--
-- > diagonal one = one
-- > getDiag . diagonal == identity
diagonal :: forall n a. (KnownNat n, AdditiveUnital a)
  => Vector n a
  -> Matrix n n a
diagonal v = tabulate (\(i,j) -> if i P.== j then index v i else zero)

inv ::
     forall n a. (BoundedField a, Eq a, Fractional a, KnownNat n)
  => Matrix n n a
  -> Matrix n n a
inv = P.either (P.const $ singleton nan) fromDMatrix . Matrix.inverse . toDMatrix

-- | map a homomorphic vector function, row-wise
mapr ::
     forall m n a. (KnownNat m, KnownNat n)
  => (Vector n a -> Vector n a)
  -> Matrix m n a
  -> Matrix m n a
mapr f x = unsafeFromVV $ tabulate (f . row x)

-- | map a homomorphic vector function, column-wise
mapc ::
     forall m n a. (KnownNat m, KnownNat n)
  => (Vector m a -> Vector m a)
  -> Matrix m n a
  -> Matrix m n a
mapc f x = unsafeFromVV $ distribute $ tabulate (f . col x)

-- | matrix multiplication
mmult ::
     forall m n k a. (Hilbert (Vector k) a, KnownNat m, KnownNat n, KnownNat k)
  => Matrix m k a
  -> Matrix k n a
  -> Matrix m n a
mmult x y = tabulate (\(i, j) -> row x i <.> col y j)

-- | join column-wise
joinc :: forall m n0 n1 a. ( KnownNat m
                 , KnownNat n0
                 , KnownNat n1
                 , Representable (Matrix m (n0 :+ n1)))
  => Matrix m n0 a
  -> Matrix m n1 a
  -> Matrix m (n0 :+ n1) a
joinc x y = tabulate
    (\(i,j) -> if j<n0 then index x (i,j) else index y (i,j-n0))
  where
    n0 = P.fromInteger $ natVal (Proxy :: Proxy n0)

-- | join row-wise
joinr :: forall m0 m1 n a. ( KnownNat m0
                 , KnownNat m1
                 , KnownNat n
                 , Representable (Matrix (m0 :+ m1) n))
  => Matrix m0 n a
  -> Matrix m1 n a
  -> Matrix (m0 :+ m1) n a
joinr x y = tabulate
    (\(i,j) -> if i<m0 then index x (i,j) else index y (i - m0,j))
  where
    m0 = P.fromInteger $ natVal (Proxy :: Proxy m0)

-- ** Arbitrary
-- | used to get sensible arbitrary instances of SomeMatrix
newtype ShapeM = ShapeM
  { unshapeM :: (Int, Int)
  }

instance Arbitrary ShapeM where
  arbitrary =
    (\m n -> ShapeM (unshapeV m, unshapeV n)) <$> arbitrary P.<*> arbitrary

instance (Arbitrary a) => Arbitrary (SomeMatrix a) where
  arbitrary =
    frequency
      [ (1, P.pure (SomeMatrix (zero, zero) V.empty))
      , ( 9
        , fromList <$>
          (P.take <$>
           ((\m n -> unshapeV m * unshapeV n) <$> arbitrary P.<*> arbitrary) P.<*>
           vector 20))
      ]

-- NumHask heirarchy
instance (KnownNat m, KnownNat n, AdditiveMagma a) =>
         AdditiveMagma (Matrix m n a) where
  plus = liftR2 plus

instance (KnownNat m, KnownNat n, AdditiveUnital a) =>
         AdditiveUnital (Matrix m n a) where
  zero = singleton zero

instance (KnownNat m, KnownNat n, AdditiveAssociative a) =>
         AdditiveAssociative (Matrix m n a)

instance (KnownNat m, KnownNat n, AdditiveCommutative a) =>
         AdditiveCommutative (Matrix m n a)

instance (KnownNat m, KnownNat n, AdditiveInvertible a) =>
         AdditiveInvertible (Matrix m n a) where
  negate = fmapRep negate

instance (KnownNat m, KnownNat n, Additive a) => Additive (Matrix m n a)

instance (KnownNat m, KnownNat n, AdditiveGroup a) =>
         AdditiveGroup (Matrix m n a)

instance (KnownNat n, Semiring a) =>
         MultiplicativeMagma (Matrix n n a) where
  times = mmult

instance (KnownNat n, Semiring a) =>
         MultiplicativeUnital (Matrix n n a) where
  one = tabulate (\(i,j) -> if i P.== j then one else zero)

instance (KnownNat n, Semiring a) =>
         MultiplicativeAssociative (Matrix n n a)

instance (KnownNat n, Fractional a, Eq a, BoundedField a, Semiring a) =>
         MultiplicativeInvertible (Matrix n n a) where
    recip = NumHask.Matrix.inv

instance (KnownNat n, Semiring a) =>
         Distribution (Matrix n n a)

instance (KnownNat n, Semiring a) =>
         Semiring (Matrix n n a)

instance (KnownNat n, AdditiveGroup a, Semiring a) =>
         Ring (Matrix n n a)

instance (Eq a, Fractional a, BoundedField a, KnownNat n, AdditiveGroup a, Semiring a) =>
         Semifield (Matrix n n a)

instance (KnownNat m, KnownNat n, Epsilon a) => Epsilon (Matrix m n a) where
  nearZero f = P.and (fmapRep nearZero f)
  aboutEqual a b = P.and (liftR2 aboutEqual a b)

instance (Semiring a, KnownNat m, KnownNat n) => Hilbert (Matrix m n) a

