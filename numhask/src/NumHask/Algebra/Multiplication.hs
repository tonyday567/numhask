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
module Numhask.Algebra.Multiplication
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

import           Numhask.Algebra.Group
import qualified Prelude                       as P

newtype Mult a = Mult a

one :: Unital (Mult a) => a
one = let (Mult a) = unit in a

recip :: Invertible (Mult a) => a -> a
recip = coerceM' inv

coerceM :: (Mult a -> Mult a -> Mult a) -> a -> a -> a
coerceM f a b = let (Mult res) = f (Mult a) (Mult b) in res

coerceM' :: (Mult a -> Mult a) -> a -> a
coerceM' f a = let (Mult res) = f (Mult a) in res

class (Absorbing (Mult a), Monoid (Mult a)) =>
      Multiplication a where
    infixl 6 *
    (*) :: a -> a -> a
    (*) = coerceM comb

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