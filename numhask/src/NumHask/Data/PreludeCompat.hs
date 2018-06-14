{-# LANGUAGE FlexibleInstances #-}
module NumHask.Algebra.PreludeCompat where

import Numhask.Algebra.Group as N
import Numhask.Algebra.Addition as N
import Numhask.Algebra.Multiplication as N

import Data.Coerce
import Prelude as P

instance Magma (Add Int) where
    comb = coerceTA (P.+)

instance Unital (Add Int) where
    unit = coerce (0 :: Int)

instance M.Semigroup (Add Int)

instance Commutative (Add Int)

instance Invertible (Add Int) where
    inv= coerceTA' (-1 P.*)

instance Magma (Mult Int) where
    comb = coerceTA (P.*)

instance Unital (Mult Int) where
    unit = coerce (1 :: Int)

instance M.Semigroup (Mult Int)

instance Commutative (Mult Int)