{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}

-- | Two different classes are supplied:
--
-- - 'Vector' where shape information is held at the type level, and
-- - 'SomeVector' where shape is held at the value level.

module NumHask.Vector
  ( Vector(..)
  , SomeVector(..)
  , ShapeV(..)
    -- ** Conversion
  , someVector
  , unsafeToVector
  , toVector
  ) where

import qualified Protolude as P
import Data.Distributive as D
import Data.Functor.Rep
import Data.Proxy (Proxy(..))
import GHC.Exts
import GHC.Show (show)
import GHC.TypeLits
import NumHask.Prelude hiding (show)
import NumHask.Shape
import Test.QuickCheck
import qualified Data.Vector as V

-- | a one-dimensional array where shape is specified at the type level
-- The main purpose of this, beyond safe typing, is to supply the Representable instance with an initial object.
-- A Boxed 'Data.Vector.Vector' is used underneath for efficient slicing, but this may change or become polymorphic in the future.
newtype Vector (n::Nat) a = Vector { toVec :: V.Vector a }
    deriving (Functor, Eq, Foldable, Ord)

instance forall n. (KnownNat n) =>
    HasShape (Vector (n::Nat)) where
    type Shape (Vector n) = Int
    shape _ = P.fromInteger $ natVal (Proxy :: Proxy n)

instance (Show a, KnownNat n) => Show (Vector (n::Nat) a) where
    show = show . someVector

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

-- | a one-dimensional array where shape is specified at the value level
-- Use this to avoid type-level hasochism by demoting a 'Vector' with 'someVector'
data SomeVector a = SomeVector Int (V.Vector a)
    deriving (Functor, Eq, Foldable, Ord)

instance HasShape SomeVector where
    type Shape SomeVector = Int
    shape (SomeVector sh _) = sh

instance (Show a) => Show (SomeVector a) where
    show (SomeVector _ v) = show (P.toList v)

-- ** conversion
-- | convert from a 'Vector' to a 'SomeVector'
someVector :: (KnownNat r) => Vector (r::Nat) a -> SomeVector a
someVector v = SomeVector (shape v) (toVec v)

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

-- | NumHask heirarchy
instance (KnownNat n, AdditiveMagma a) => AdditiveMagma (Vector n a) where
    plus = liftR2 plus
instance (KnownNat n, AdditiveUnital a) => AdditiveUnital (Vector n a) where
    zero = pureRep zero
instance (KnownNat n, AdditiveAssociative a) => AdditiveAssociative (Vector n a)
instance (KnownNat n, AdditiveCommutative a) => AdditiveCommutative (Vector n a)
instance (KnownNat n, AdditiveInvertible a) => AdditiveInvertible (Vector n a) where
    negate = fmapRep negate
instance (KnownNat n, AdditiveMonoidal a) => AdditiveMonoidal (Vector n a)
instance (KnownNat n, Additive a) => Additive (Vector n a)
instance (KnownNat n, AdditiveGroup a) => AdditiveGroup (Vector n a)

instance (KnownNat n, MultiplicativeMagma a) => MultiplicativeMagma (Vector n a) where
    times = liftR2 times
instance (KnownNat n, MultiplicativeUnital a) => MultiplicativeUnital (Vector n a) where
    one = pureRep one
instance (KnownNat n, MultiplicativeAssociative a) => MultiplicativeAssociative (Vector n a)
instance (KnownNat n, MultiplicativeCommutative a) => MultiplicativeCommutative (Vector n a)
instance (KnownNat n, MultiplicativeInvertible a) => MultiplicativeInvertible (Vector n a) where
    recip = fmapRep recip
instance (KnownNat n, MultiplicativeMonoidal a) => MultiplicativeMonoidal (Vector n a)
instance (KnownNat n, Multiplicative a) => Multiplicative (Vector n a)
instance (KnownNat n, MultiplicativeGroup a) => MultiplicativeGroup (Vector n a)

instance (KnownNat n, MultiplicativeMagma a, Additive a) => Distribution (Vector n a)

instance (KnownNat n, Semiring a) => Semiring (Vector n a)
instance (KnownNat n, Ring a) => Ring (Vector n a)
instance (KnownNat n, CRing a) => CRing (Vector n a)
instance (KnownNat n, Field a) => Field (Vector n a)

instance (KnownNat n, ExpField a) => ExpField (Vector n a) where
    exp = fmapRep exp
    log = fmapRep log

instance (KnownNat n, BoundedField a) => BoundedField (Vector n a) where
    isNaN f = or (fmapRep isNaN f)

instance (KnownNat n, Signed a) => Signed (Vector n a) where
    sign = fmapRep sign
    abs = fmapRep abs

instance (ExpField a) =>
    Normed (Vector n a) a where
    size r = sqrt $ foldr (+) zero $ (**(one+one)) <$> r

instance (KnownNat n, Epsilon a) => Epsilon (Vector n a) where
    nearZero f = and (fmapRep nearZero f)
    aboutEqual a b = and (liftR2 aboutEqual a b)

instance (KnownNat n, ExpField a) => Metric (Vector n a) a where
    distance a b = size (a - b)

instance (KnownNat n, Integral a) => Integral (Vector n a) where
    divMod a b = (d,m)
        where
          x = liftR2 divMod a b
          d = fmap fst x
          m = fmap snd x
