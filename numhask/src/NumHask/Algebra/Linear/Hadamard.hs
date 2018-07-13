{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Element-by-element operations
module NumHask.Algebra.Linear.Hadamard
  ( HadamardMultiplication(..)
  , HadamardDivision(..)
  , Hadamard
  )
where

import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Multiplicative

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

-- | element by element division
--
-- > a ./. a == singleton one
class (Group (Product a)) =>
  HadamardDivision m a where
  infixl 7 ./.
  (./.) :: m a -> m a -> m a

class (HadamardMultiplication m a, HadamardDivision m a) => Hadamard m a
instance (HadamardMultiplication m a, HadamardDivision m a) => Hadamard m a
