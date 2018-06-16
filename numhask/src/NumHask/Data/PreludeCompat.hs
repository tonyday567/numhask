{-# LANGUAGE FlexibleInstances #-}
module NumHask.Data.PreludeCompat where

import NumHask.Algebra.Abstract.Group as N
import NumHask.Algebra.Abstract.Addition as N
import NumHask.Algebra.Abstract.Multiplication as N

import Data.Coerce
import Prelude as P

instance Magma (Mult Int) where
    comb = coerceTM (P.*)

instance Unital (Mult Int) where
    unit = coerce (1 :: Int)

instance N.Semigroup (Mult Int)

instance Commutative (Mult Int)