{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Element-by-element operations
module NumHask.Algebra.Linear.Hadamard
  ( Hadamard_(..)
  , HadamardMultiplication(..)
  , HadamardDivision(..)
  , Hadamard
  )
where

import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Ring
import Data.Coerce

newtype Hadamard_ a = Hadamard_ a

--FIXME the hadamard product has a relationship between laws
-- | element by element multiplication
--
-- > (a .*. b) .*. c == a .*. (b .*. c)
-- > singleton one .*. a = a
-- > a .*. singelton one = a
-- > a .*. b == b .*. a
class (Multiplicative a) =>
  HadamardMultiplication m a where
  infixl 7 .*.
  (.*.) :: m a -> m a -> m a

instance HadamardMultiplication m a => Magma (Product (Hadamard_ (m a))) where
  magma = coerce ((.*.) @m @a)

instance HadamardMultiplication m a => Associative (Product (Hadamard_ (m a)))

-- | element by element division
--
-- > a ./. a == singleton one
class (Group (Product a)) =>
  HadamardDivision m a where
  infixl 7 ./.
  (./.) :: m a -> m a -> m a

class (HadamardMultiplication m a, HadamardDivision m a) => Hadamard m a
instance (HadamardMultiplication m a, HadamardDivision m a) => Hadamard m a

instance (Unital (Product (m a)), Hadamard m a) => Invertible (Product (Hadamard_ (m a))) where
  inv = coerce ((./.) @m @a (one @(m a)))

instance (Distributive a, Additive (Hadamard_ (m a))
        , Hadamard m a, Unital (Product (Hadamard_ (m a)))
        , Absorbing (Product (Hadamard_ (m a))))
        => Distributive  (Hadamard_ (m a))
