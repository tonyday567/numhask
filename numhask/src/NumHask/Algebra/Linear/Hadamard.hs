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
  ( HadamardMultiplication(..)
  , HadamardDivision(..)
  , Hadamard
  )
where

import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Ring
import Data.Coerce

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
  (.*.) = coerce ((.*.) @m @a)


-- | element by element division
--
-- > a ./. a == singleton one
class (Divisive a) =>
  HadamardDivision m a where
  infixl 7 ./.
  (./.) :: m a -> m a -> m a
  (./.) = coerce ((./.) @m @a)

class (HadamardMultiplication m a, HadamardDivision m a) => Hadamard m a
instance (HadamardMultiplication m a, HadamardDivision m a) => Hadamard m a

