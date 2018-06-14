{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
-- | The Group hirarchy
module Numhask.Algebra.Addition
      ( Magma(..)
      , Unital(..)
      , one
      , zero
      , Semigroup(..)
      , Commutative
      , Add(..)
      , coerceFA
      , coerceFA'
      , coerceTA
      , coerceTA'
      , Addition(..)
      , Invertible(..)
      , neg
      , recip
      , Idempotent
      , Monoid(..)
      , Mult(..)
      , coerceM
      , coerceM'
      , Multiplication(..)
      , Group
      , groupSwap
      , AbelianGroup
      )
      where

-- FIXME: why can't i coerce? (never used it...)
import           Data.Coerce
import qualified Prelude                       as P
import           Numhask.Algebra.Group

newtype Add a = Add a

--less flexible coerces for better instances
coerceFA :: (Add a -> Add a -> Add a) -> a -> a -> a
coerceFA f a b = let (Add res) = f (Add a) (Add b) in res

coerceFA' :: (Add a -> Add a) -> a -> a
coerceFA' f a = let (Add res) = f (Add a) in res

coerceTA :: (a -> a -> a) -> (Add a -> Add a -> Add a)
coerceTA f (Add a) (Add b) = Add P.$ f a b

coerceTA' :: (a -> a) -> (Add a -> Add a)
coerceTA' f (Add a) = Add P.$ f a

class (Semigroup (Add a), Commutative (Add a)) => Addition a where
      infixl 6 +
      (+) :: a -> a -> a
      (+) = coerceFA comb

      sum :: (P.Foldable f, Unital (Add a)) => f a -> a
      sum = P.foldr (+) zero

      infixl 6 -
      (-) :: Invertible (Add a) => a -> a -> a
      (-) a b = a + neg b

instance (Semigroup (Add a), Commutative (Add a)) => Addition a

zero :: Unital (Add a) => a
zero = let (Add a) = unit in a


neg :: Invertible (Add a) => a -> a
neg = coerceFA' inv
