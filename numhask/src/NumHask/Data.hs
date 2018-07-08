{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module NumHask.Data where

import           Prelude                 hiding ( Num(..)
                                                , sum
                                                , recip
                                                )

-- -- | Monoid under addition.
-- --
-- -- >>> getSum (Sum 1 <> Sum 2 <> mempty)
-- -- 3
-- newtype Sum a = Sum { getSum :: a }
--         deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Functor)

-- -- | @since 4.8.0.0
-- instance Applicative Sum where
--     pure     = Sum
--     (<*>)    = coerce  

-- -- | @since 4.8.0.0
-- instance Monad Sum where
--     m >>= k  = k (getSum m)

-- instance AdditiveMagma a => AdditiveMagma (Sum a) where
--   (Sum x) `plus` (Sum y) = Sum (x `plus` y)

-- instance AdditiveUnital a => AdditiveUnital (Sum a) where
--   zero = Sum zero

-- instance AdditiveMagma a => AdditiveAssociative (Sum a)

-- instance AdditiveInvertible a => AdditiveInvertible (Sum a) where
--   negate (Sum x) = Sum (negate x)

-- instance AdditiveMagma a => AdditiveCommutative (Sum a) where

-- instance (AdditiveUnital a, AdditiveMagma a) => Additive (Sum a) where  

-- instance (AdditiveInvertible a, AdditiveUnital a) => AdditiveGroup (Sum a) where


-- instance AdditiveMagma a => Semigroup (Sum a) where
--   (Sum x) <> (Sum y) = Sum $ x `plus` y

-- instance AdditiveUnital a => Monoid (Sum a) where
--   mempty = Sum zero




-- -- | Monoid under multiplication.
-- --
-- -- >>> getProduct (Product 3 <> Product 4 <> mempty)
-- -- 12
-- newtype Product a = Product { getProduct :: a }
--         deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Functor)

-- -- | @since 4.8.0.0
-- instance Applicative Product where
--     pure     = Product
--     (<*>)    = coerce

-- -- | @since 4.8.0.0
-- instance Monad Product where
--     m >>= k  = k (getProduct m)


-- instance MultiplicativeMagma a => MultiplicativeMagma (Product a) where
--   (Product x) `times` (Product y) = Product (x `times` y)

-- instance MultiplicativeUnital a => MultiplicativeUnital (Product a) where
--   one = Product one

-- instance MultiplicativeMagma a => MultiplicativeAssociative (Product a) 

-- instance MultiplicativeInvertible a => MultiplicativeInvertible (Product a) where
--   recip (Product x) = Product (recip x)

-- instance MultiplicativeMagma a => MultiplicativeCommutative (Product a)

-- instance MultiplicativeUnital a => Multiplicative (Product a) where

-- instance (MultiplicativeUnital a, MultiplicativeInvertible a) => MultiplicativeGroup (Product a) where


-- instance MultiplicativeMagma a => Semigroup (Product a) where
--   (Product x) <> (Product y) = Product $ x `times` y

-- instance MultiplicativeUnital a => Monoid (Product a) where
--   mempty = Product one 
