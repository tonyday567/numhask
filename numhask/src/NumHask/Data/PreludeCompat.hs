{-# LANGUAGE FlexibleInstances #-}
module NumHask.Data.PreludeCompat where

import Numhask.Algebra.Abstract.Group as N
import Numhask.Algebra.Abstract.Addition as N
import Numhask.Algebra.Abstract.Multiplication as N

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