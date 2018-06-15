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
      , Multiplication(..)
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

class (Absorbing (Mult a), Monoid (Mult a)) =>
      Multiplication a where
    infixl 6 *
    (*) :: a -> a -> a
    (*) = coerceFM comb

    zero' :: a
    zero' = let (Mult a) = absorb in a

    product :: P.Foldable f => f a -> a
    product = P.foldr (*) one

    infixl 6 /
    (/) :: Invertible (Mult a) => a -> a -> a
    (/) a b = a * recip b

instance (Absorbing (Mult a), Monoid (Mult a)) =>
    Multiplication a

--less flexible coerces for better inference in instances
coerceFM :: (Mult a -> Mult a -> Mult a) -> a -> a -> a
coerceFM f a b = let (Mult res) = f (Mult a) (Mult b) in res

coerceFM' :: (Mult a -> Mult a) -> a -> a
coerceFM' f a = let (Mult res) = f (Mult a) in res

coerceTM :: (a -> a -> a) -> (Mult a -> Mult a -> Mult a)
coerceTM f (Mult a) (Mult b) = Mult P.$ f a b

coerceTM' :: (a -> a) -> (Mult a -> Mult a)
coerceTM' f (Mult a) = Mult P.$ f a