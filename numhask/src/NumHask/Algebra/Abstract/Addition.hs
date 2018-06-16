{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
-- | The Group hirarchy
module NumHask.Algebra.Abstract.Addition
      ( zero
      , Add(..)
      , coerceFA
      , coerceFA'
      , coerceTA
      , coerceTA'
      , Addition
      , (+)
      , sum
      , (-)
      , neg
      )
      where

import qualified Prelude                       as P
import qualified GHC.Generics                  as P
import           NumHask.Algebra.Abstract.Group

newtype Add a = Add a
      deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Generic, P.Generic1, P.Functor)

class (Semigroup (Add a), Commutative (Add a)) => Addition a
instance (Semigroup (Add a), Commutative (Add a)) => Addition a

infixl 6 +
(+) :: Addition a => a -> a -> a
(+) = coerceFA comb

sum :: (P.Foldable f, Unital (Add a), Addition a) => f a -> a
sum = P.foldr (+) zero

infixl 6 -
(-) :: (Invertible (Add a), Addition a) => a -> a -> a
(-) a b = a + neg b

zero :: Unital (Add a) => a
zero = let (Add a) = unit in a


neg :: Invertible (Add a) => a -> a
neg = coerceFA' inv

--less flexible coerces for better inference in instances
coerceFA :: (Add a -> Add a -> Add a) -> a -> a -> a
coerceFA f a b = let (Add res) = f (Add a) (Add b) in res

coerceFA' :: (Add a -> Add a) -> a -> a
coerceFA' f a = let (Add res) = f (Add a) in res

coerceTA :: (a -> a -> a) -> (Add a -> Add a -> Add a)
coerceTA f (Add a) (Add b) = Add P.$ f a b

coerceTA' :: (a -> a) -> (Add a -> Add a)
coerceTA' f (Add a) = Add P.$ f a
