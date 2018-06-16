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
module NumHask.Algebra.Abstract.Multiplication
      ( one
      , recip
      , Mult(..)
      , coerceFM
      , coerceFM'
      , coerceTM
      , coerceTM'
      , Multiplication
      , (*)
      , zero'
      , product
      , (/)
      )
      where

import           NumHask.Algebra.Abstract.Group
import qualified Prelude                       as P
import qualified GHC.Generics                  as P

newtype Mult a = Mult a
    deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Generic, P.Generic1, P.Functor)

one :: Unital (Mult a) => a
one = let (Mult a) = unit in a

recip :: Invertible (Mult a) => a -> a
recip = coerceFM' inv

class (Absorbing (Mult a), Monoid (Mult a)) => Multiplication a
instance (Absorbing (Mult a), Monoid (Mult a)) => Multiplication a

infixl 6 *
(*) :: Multiplication a => a -> a -> a
(*) = coerceFM comb

zero' :: Multiplication a => a
zero' = let (Mult a) = absorb in a

product :: (P.Foldable f, Multiplication a) => f a -> a
product = P.foldr (*) one

infixl 6 /
(/) :: (Invertible (Mult a), Multiplication a) => a -> a -> a
(/) a b = a * recip b

--less flexible coerces for better inference in instances
coerceFM :: (Mult a -> Mult a -> Mult a) -> a -> a -> a
coerceFM f a b = let (Mult res) = f (Mult a) (Mult b) in res

coerceFM' :: (Mult a -> Mult a) -> a -> a
coerceFM' f a = let (Mult res) = f (Mult a) in res

coerceTM :: (a -> a -> a) -> (Mult a -> Mult a -> Mult a)
coerceTM f (Mult a) (Mult b) = Mult P.$ f a b

coerceTM' :: (a -> a) -> (Mult a -> Mult a)
coerceTM' f (Mult a) = Mult P.$ f a