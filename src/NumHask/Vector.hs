{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wall #-}

-- | Two different classes are supplied:
--
-- - 'Vector' where shape information is held at the type level, and
-- - 'SomeVector' where shape is held at the value level.

module NumHask.Vector
  ( Vector(..)
  , SomeVector(..)
  , ShapeV(..)
  , shapeV
    -- ** Conversion
  , someVector
  , unsafeToVector
  , toVector
  ) where

import qualified Protolude as P
import Protolude
    (($), (<$>), Functor(..), Show, Eq(..), take, Foldable(..), Ord(..), Int, Maybe(..), (.))

import Data.Distributive as D
import Data.Functor.Rep
import Data.Proxy (Proxy(..))
import GHC.Exts
import GHC.Show (show)
import GHC.TypeLits
import NumHask.Algebra.Additive
import NumHask.HasShape
import Test.QuickCheck
import qualified Data.Vector as V

-- | a one-dimensional array where shape is specified at the type level
-- The main purpose of this, beyond safe typing, is to supply the Representable instance with an initial object.
-- A Boxed 'Data.Vector.Vector' is used underneath for efficient slicing, but this may change or become polymorphic in the future.
newtype Vector (n::Nat) a = Vector { toVec :: V.Vector a }
    deriving (Functor, Eq, Foldable, Ord)

-- | a one-dimensional array where shape is specified at the value level
-- Use this to avoid type-level hasochism by demoting a 'Vector' with 'someVector'
data SomeVector a = SomeVector Int (V.Vector a)
    deriving (Functor, Eq, Foldable, Ord)

instance HasShape (SomeVector a) where
    type Shape (SomeVector a) = Int
    shape (SomeVector sh _) = sh
    ndim _ = 1

instance forall a r. (KnownNat r) =>
    HasShape (Vector (r::Nat) a) where
    type Shape (Vector r a) = Int
    shape = shapeV
    ndim _ = 1

instance (Show a) => Show (SomeVector a) where
    show (SomeVector _ v) = show (P.toList v)

instance (Show a, KnownNat n) => Show (Vector (n::Nat) a) where
    show = show . someVector

-- ** conversion
-- | the shape value demoted from type-level
shapeV :: forall a r. (KnownNat r) => Vector (r :: Nat) a -> Int
shapeV _ = P.fromInteger $ natVal (Proxy :: Proxy r)

-- | convert from a 'Vector' to a 'SomeVector'
someVector :: (KnownNat r) => Vector (r::Nat) a -> SomeVector a
someVector v = SomeVector (shapeV v) (toVec v)

-- | convert from a 'SomeVector' to a 'Vector' with no shape check
unsafeToVector :: SomeVector a -> Vector (r::Nat) a
unsafeToVector (SomeVector _ v) = Vector v

-- | convert from a 'SomeVector' to a 'Vector', checking shape
toVector :: forall a r. (KnownNat r) => SomeVector a -> Maybe (Vector (r::Nat) a)
toVector (SomeVector s v) = if s==n then Just $ Vector v else Nothing
  where
    n = P.fromInteger $ natVal (Proxy :: Proxy r)

-- | pads with 'zero' if needed
instance (KnownNat n, AdditiveUnital a) => IsList (Vector n a) where
    type Item (Vector n a) = a
    fromList l = Vector $ V.fromList $ P.take n $ l P.++ P.repeat zero
      where
        n = P.fromInteger $ natVal (Proxy :: Proxy n)
    toList (Vector v) = V.toList v

instance IsList (SomeVector a) where
    type Item (SomeVector a) = a
    fromList l = SomeVector (P.length l) (V.fromList l)
    toList (SomeVector _ v) = V.toList v

-- | used to get sensible arbitrary instances of SomeVector
newtype ShapeV = ShapeV { unshapeV :: Int }

instance Arbitrary ShapeV where
    arbitrary = frequency
        [ (1, P.pure $ ShapeV 0)
        , (1, P.pure $ ShapeV 1)
        , (1, P.pure $ ShapeV 2)
        , (1, P.pure $ ShapeV 3)
        , (1, P.pure $ ShapeV 6)
        , (1, P.pure $ ShapeV 20)
        ]

instance (Arbitrary a) => Arbitrary (SomeVector a) where
    arbitrary = frequency
        [ (1, P.pure (SomeVector 0 V.empty))
        , (9, fromList <$> (take <$> (unshapeV <$> arbitrary) P.<*> vector 20))
        ]

instance (KnownNat n, Arbitrary a, AdditiveUnital a) => Arbitrary (Vector n a) where
    arbitrary = frequency
        [ (1, P.pure zero)
        , (9, fromList <$> vector n)
        ]
      where
        n = P.fromInteger $ natVal (Proxy :: Proxy n)

instance KnownNat n => D.Distributive (Vector n) where
    distribute f = Vector $ V.generate n $ \i -> fmap (\(Vector v) -> V.unsafeIndex v i) f
      where
        n = P.fromInteger $ natVal (Proxy :: Proxy n)

instance KnownNat n => Representable (Vector n) where
    type Rep (Vector n) = P.Int
    tabulate = Vector P.. V.generate n0
      where
        n0 = P.fromInteger $ natVal (Proxy :: Proxy n)
    index (Vector xs) i = xs V.! i
