{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Two-dimensional arrays. Two classes are supplied
--
-- - 'Matrix' where shape information is held at type level, and
-- - 'SomeMatrix' where shape is held at the value level.
--
-- In both cases, the underlying data is contained as a flat vector for efficiency purposes.

module NumHask.Matrix
  ( Matrix(..)
  , SomeMatrix(..)
  , ShapeM(..)
    -- * Conversion
  , someMatrix
  , unsafeToMatrix
  , toMatrix
  , unsafeFromVV
  , toCol
  , toRow
  , fromCol
  , fromRow
  , col
  , row
    -- * Operations
  , mmult
  ) where

import qualified Protolude as P
import Protolude
    (($), Functor(..), Show, Eq(..), (.), (<$>), Foldable(..), Int, Maybe(..))
import Data.Distributive as D
import Data.Functor.Rep
import Data.Proxy (Proxy(..))
import GHC.TypeLits
import NumHask.Algebra.Additive
import NumHask.Algebra.Integral
import NumHask.Algebra.Module
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import NumHask.HasShape
import NumHask.Vector
import Test.QuickCheck
import qualified Data.Vector as V
import GHC.Show
import GHC.Exts

-- | a two-dimensional array where shape is specified at the type level
-- The main purpose of this, beyond safe typing, is to supply the Representable instance with an initial object.
-- A single Boxed 'Data.Vector.Vector' is used underneath for efficient slicing, but this may change or become polymorphic in the future.
newtype Matrix m n a = Matrix { flattenMatrix :: V.Vector a }
    deriving (Functor, Eq, Foldable)

-- | a two-dimensional array where shape is specified at the value level as a '(Int,Int)'
-- Use this to avoid type-level hasochism by demoting a 'Matrix' with 'someMatrix'
data SomeMatrix a = SomeMatrix (Int,Int) (V.Vector a)
    deriving (Functor, Eq, Foldable)

instance HasShape (SomeMatrix a) where
    type Shape (SomeMatrix a) = (Int,Int)
    shape (SomeMatrix sh _) = sh
    ndim = P.length . shape

instance forall a m n. (KnownNat m, KnownNat n) =>
    HasShape (Matrix (m::Nat) (n::Nat) a) where
    type Shape (Matrix m n a) = (Int,Int)
    shape = shapeM
    ndim = P.length . shape

-- | the shape value demoted from type-level
shapeM :: forall a m n. (KnownNat m, KnownNat n) => Matrix (m::Nat) (n::Nat) a -> (Int, Int)
shapeM _ = ( P.fromInteger $ natVal (Proxy :: Proxy m)
           , P.fromInteger $ natVal (Proxy :: Proxy n))

instance (Show a) => Show (SomeMatrix a) where
    show (SomeMatrix _ v) = show (P.toList v)

instance (Show a, KnownNat m, KnownNat n) => Show (Matrix (m::Nat) (n::Nat) a) where
    show = show . someMatrix

-- ** conversion

-- | convert from a 'Matrix' to a 'SomeMatrix'
someMatrix :: (KnownNat m, KnownNat n) => Matrix (m::Nat) (n::Nat) a -> SomeMatrix a
someMatrix v = SomeMatrix (shape v) (flattenMatrix v)

-- | convert from a 'SomeMatrix' to a 'Matrix' with no shape check
unsafeToMatrix :: SomeMatrix a -> Matrix (m::Nat) (n::Nat) a
unsafeToMatrix (SomeMatrix _ v) = Matrix v

-- | convert from a 'SomeMatrix' to a 'Matrix', checking shape
toMatrix :: forall a m n. (KnownNat m, KnownNat n) => SomeMatrix a ->
    Maybe (Matrix (m::Nat) (n::Nat) a)
toMatrix (SomeMatrix s v) = if s==(m,n) then Just $ Matrix v else Nothing
  where
    m = P.fromInteger $ natVal (Proxy :: Proxy m)
    n = P.fromInteger $ natVal (Proxy :: Proxy n)

-- | from flat list
instance (KnownNat m, KnownNat n, AdditiveUnital a) => IsList (Matrix m n a) where
    type Item (Matrix m n a) = a
    fromList l = Matrix $ V.fromList $ P.take (m*n) $ l P.++ P.repeat zero
      where
        m = P.fromInteger $ natVal (Proxy :: Proxy m)
        n = P.fromInteger $ natVal (Proxy :: Proxy n)
    toList (Matrix v) = V.toList v

-- | from nested list
instance IsList (SomeMatrix a) where
    type Item (SomeMatrix a) = [a]
    fromList l =
        SomeMatrix (P.length l,P.length $ P.head l) (V.fromList $ P.mconcat l)
    toList (SomeMatrix (m,n) v) =
        (\i -> V.toList $ V.unsafeSlice (i*n) n v) <$> [0..(m - 1)]

-- | just used to get sensible arbitrary instances of SomeMatrix
newtype ShapeM = ShapeM { unshapeM :: (Int,Int) }

instance Arbitrary ShapeM where
    arbitrary =
        (\m n -> ShapeM (unshapeV m, unshapeV n)) <$> arbitrary P.<*> arbitrary

instance (Arbitrary a) => Arbitrary (SomeMatrix a) where
    arbitrary = frequency
        [ (1, P.pure (SomeMatrix (zero,zero) V.empty))
        , (9,fromList <$>
             (P.take <$>
              ((\m n -> unshapeV m * unshapeV n) <$> arbitrary P.<*> arbitrary) P.<*>
              vector 20))
        ]

instance (KnownNat m, KnownNat n, Arbitrary a, AdditiveUnital a) => Arbitrary (Matrix m n a) where
    arbitrary = frequency
        [ (1, P.pure zero)
        , (9,fromList <$> vector (m*n))
        ]
      where
        n = P.fromInteger $ natVal (Proxy :: Proxy n)
        m = P.fromInteger $ natVal (Proxy :: Proxy m)

instance (KnownNat m, KnownNat n) => Distributive (Matrix m n) where
    distribute f = Matrix $ V.generate (n*m)
        $ \i -> fmap (\(Matrix v) -> V.unsafeIndex v i) f
      where
        m = P.fromInteger $ natVal (Proxy :: Proxy m)
        n = P.fromInteger $ natVal (Proxy :: Proxy n)

instance (KnownNat m, KnownNat n) => Representable (Matrix m n) where
    type Rep (Matrix m n) = (P.Int, P.Int)
    tabulate f = Matrix $ V.generate (m*n) (\x -> f (divMod x (m*n)))
      where
        m = P.fromInteger $ natVal (Proxy :: Proxy m)
        n = P.fromInteger $ natVal (Proxy :: Proxy n)
    index (Matrix xs) (i0,i1) = xs V.! (i0*m + i1)
      where
        m = P.fromInteger $ natVal (Proxy :: Proxy m)

-- | conversion from a double Vector representation
unsafeFromVV :: forall a m n. ( ) => Vector m (Vector n a) -> Matrix m n a
unsafeFromVV vv = Matrix $ P.foldr ((V.++) . toVec) V.empty vv

-- | convert a 'Vector' to a column 'Matrix'
toCol :: forall a n. ( ) => Vector n a -> Matrix 1 n a
toCol v = Matrix $ toVec v

-- | convert a 'Vector' to a row 'Matrix'
toRow :: forall a m. ( ) => Vector m a -> Matrix m 1 a
toRow v = Matrix $ toVec v

-- | convert a row 'Matrix' to a 'Vector'
fromCol :: forall a n. ( ) => Matrix 1 n a -> Vector n a
fromCol m = Vector $ flattenMatrix m

-- | convert a column 'Matrix' to a 'Vector'
fromRow :: forall a m. ( ) => Matrix m 1 a -> Vector m a
fromRow m = Vector $ flattenMatrix m

-- | extract a row from a 'Matrix' as a 'Vector'
row :: forall a m n. (KnownNat m, KnownNat n) => P.Int -> Matrix m n a -> Vector n a
row i (Matrix a) = Vector $ V.unsafeSlice (i*m) n a
  where
    m = P.fromInteger $ natVal (Proxy :: Proxy m)
    n = P.fromInteger $ natVal (Proxy :: Proxy n)

-- | extract a column from a 'Matrix' as a 'Vector'
col :: forall a m n. (KnownNat m, KnownNat n) => P.Int -> Matrix m n a -> Vector m a
col i (Matrix a) = Vector $ V.generate m (\x -> a V.! (i+x*n))
  where
    m = P.fromInteger $ natVal (Proxy :: Proxy m)
    n = P.fromInteger $ natVal (Proxy :: Proxy n)

-- ** Operations
-- | matrix multiplication for a 'Matrix'
mmult :: forall m n k a. (CRing a, KnownNat m, KnownNat n, KnownNat k) =>
    Matrix m k a -> Matrix k n a -> Matrix m n a
mmult x y = tabulate (\(i,j) -> row i x <.> col j y)
