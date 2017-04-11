{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -Wall #-}

-- | N-dimensional arrays. Two classes are supplied:
--
-- - 'Tensor' where shape information is held at type level, and
-- - 'SomeTensor' where shape is held at the value level.
--
-- In both cases, the underlying data is contained as a flat vector for efficiency purposes.

module NumHask.Tensor
  ( Tensor(..)
  , SomeTensor(..)
  -- * Conversion
  , someTensor
  , unsafeToTensor
  , toTensor
  , flatten1
  ) where

import qualified Protolude as P
import Protolude
    (($), (<$>), Functor(..), Show, Eq(..), (.), Maybe(..), Int, reverse, foldr, fst, zipWith, scanr, drop, sum, product, Foldable(..))

import Data.Distributive as D
import Data.Functor.Rep
import Data.Singletons
import Data.Singletons.Prelude
import GHC.Exts
import GHC.Show
import GHC.TypeLits
import NumHask.Algebra.Additive
import NumHask.Algebra.Integral
import NumHask.Algebra.Multiplicative
import Test.QuickCheck
import qualified Data.Vector as V
import NumHask.Naperian

-- | an n-dimensional array where shape is specified at the type level
-- The main purpose of this, beyond safe typing, is to supply the Representable instance with an initial object.
-- A single Boxed 'Data.Vector.Vector' is used underneath for efficient slicing, but this may change or become polymorphic in the future.
newtype Tensor r a = Tensor { flattenTensor :: V.Vector a }
    deriving (Functor, Eq, Foldable)

instance (SingI r) => HasShape (Tensor (r::[Nat])) where
    type Shape (Tensor r) = [Int]
    shape _ = case (sing :: Sing r) of
                SNil -> []
                (SCons x xs) -> fromIntegral <$> (fromSing x: fromSing xs)
    ndim = P.length . shape

instance (SingI r) => Naperian (Tensor (r::[Nat]))

ind :: [Int] -> [Int] -> Int
ind ns xs = sum $ zipWith (*) xs (drop 1 $ scanr (*) 1 (reverse ns))

unfoldI :: forall t. Integral t => [t] -> t -> ([t], t)
unfoldI ns x =
    foldr
    (\a (acc,rem) -> let (d,m) = divMod rem a in (m:acc,d))
    ([],x)
    (P.reverse ns)

unind :: [Int] -> Int -> [Int]
unind ns x= fst $ unfoldI ns x

instance forall r. (SingI r) => Distributive (Tensor (r::[Nat])) where
    distribute f = Tensor $ V.generate n
        $ \i -> fmap (\(Tensor v) -> V.unsafeIndex v i) f
      where
        n = case (sing :: Sing r) of
          SNil -> one
          (SCons x xs) -> product $ fromInteger <$> (fromSing x: fromSing xs)

instance forall (r :: [Nat]). (SingI r) => Representable (Tensor r) where
    type Rep (Tensor r) = [Int]
    tabulate f = Tensor $ V.generate (product ns) (f . unind ns)
      where
        ns = case (sing :: Sing r) of
          SNil -> []
          (SCons x xs) -> fromIntegral <$> (fromSing x: fromSing xs)
    index (Tensor xs) rs = xs V.! ind ns rs
      where
        ns = case (sing :: Sing r) of
          SNil -> []
          (SCons x xs') -> fromIntegral <$> (fromSing x: fromSing xs')

-- | an n-dimensional array where shape is specified at the value level as an '[Int]'
-- Use this to avoid type-level hasochism by demoting a 'Tensor' with 'someTensor'
data SomeTensor a = SomeTensor [Int] (V.Vector a)
    deriving (Functor, Eq, Foldable)

instance HasShape SomeTensor where
    type Shape SomeTensor = [Int]
    shape (SomeTensor sh _) = sh
    ndim = P.length . shape

instance (Show a) => Show (SomeTensor a) where
    show r@(SomeTensor l _) = go (P.length l) r
      where
        go n r'@(SomeTensor l' v') = case P.length l' of
          0 -> show $ V.head v'
          1 -> "[" P.++ P.intercalate ", " (show <$> P.toList v') P.++ "]"
          x -> 
              "[" P.++
              P.intercalate
              (",\n" P.++ P.replicate (n-x+1) ' ')
              (go n <$> flatten1 r') P.++
              "]"

instance (Show a, SingI r) => Show (Tensor (r::[Nat]) a) where
    show = show . someTensor

-- * Conversion
-- | convert a 'Tensor' to a 'SomeTensor', losing the type level shape
someTensor :: (SingI r) => Tensor (r::[Nat]) a -> SomeTensor a
someTensor n = SomeTensor (shape n) (flattenTensor n)

-- | convert a 'SomeTensor' to a 'Tensor' with no checks on shape.
unsafeToTensor :: SomeTensor a -> Tensor (r::[Nat]) a
unsafeToTensor (SomeTensor _ v) = Tensor v

-- | convert a 'SomeTensor' to a 'Tensor', check for shape equality.
toTensor :: forall a r. (SingI r) => SomeTensor a -> Maybe (Tensor (r::[Nat]) a)
toTensor (SomeTensor sh v) = if sh==sh' then Just (Tensor v) else Nothing
  where
    sh' = case (sing :: Sing r) of
            SNil -> []
            (SCons x xs) -> fromIntegral <$> (fromSing x: fromSing xs)

-- | convert the top layer of a SomeTensor to a [SomeTensor]
flatten1 :: SomeTensor a -> [SomeTensor a]
flatten1 (SomeTensor rep v) = (\s -> SomeTensor (drop 1 rep) (V.unsafeSlice (s*l) l v)) <$> ss
    where
      n = P.fromMaybe 0 $ P.head rep
      ss = P.take n [0..]
      l = product $ drop 1 rep

-- | from flat list
instance (SingI r, AdditiveUnital a) => IsList (Tensor (r::[Nat]) a) where
    type Item (Tensor r a) = a
    fromList l = Tensor $ V.fromList $ P.take n $ l P.++ P.repeat zero
      where
        n = case (sing :: Sing r) of
          SNil -> one
          (SCons x xs') -> product $ fromIntegral <$> (fromSing x: fromSing xs')
    toList (Tensor v) = V.toList v

-- | arbitraryly-nested list conversion to fit in with OverloadedLists needs some complex parsing
fromListSomeTensor :: forall a. (AdditiveUnital a) => [Int] -> [a] -> SomeTensor a
fromListSomeTensor ns l =
    SomeTensor ns (V.fromList $ P.take (product ns) $ l P.++ P.repeat zero)

-- not sure how to combine this with HasShape
newtype ShapeT = ShapeT {unshapeT :: [Int]} deriving (Show, Eq)

instance Arbitrary ShapeT where
    arbitrary = frequency
        [ (1, P.pure (ShapeT []))
        -- , (1, Shape . (:[]) <$> arbitrary)
        , (1, ShapeT . (:[]) <$> n)
        , (1, ShapeT <$> ((\x y -> [x,y]) <$> n P.<*> n))
        , (1, ShapeT <$> ((\x y z -> [x,y,z]) <$> n P.<*> n P.<*> n))
        ]
      where
        n = frequency [(1,P.pure 1),(1,P.pure 2),(1,P.pure 3)]

instance forall a (r :: [Nat]). (SingI r, Arbitrary a, AdditiveUnital a) => Arbitrary (Tensor r a) where
    arbitrary = frequency
        [ (1, P.pure zero)
        , (9, fromList <$> vector n)
        ]
      where
        n = case (sing :: Sing r) of
               SNil -> one
               (SCons x xs) -> product $ fromInteger <$> (fromSing x: fromSing xs)

instance forall a. (Arbitrary a, AdditiveUnital a) => Arbitrary (SomeTensor a) where
    arbitrary = frequency
        [ (1, P.pure (SomeTensor [] V.empty))
        , (9, fromListSomeTensor <$> (unshapeT <$> arbitrary) P.<*> vector 48)
        ]
