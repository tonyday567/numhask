{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall #-}

-- | Element-by-element operations
module NumHask.Algebra.Linear.Hadamard
  ( 
      HadamardMultiplication(..)
    , HadamardDivision(..)
    , Hadamard
    , AdditiveBasis(..)
    , AdditiveGroupBasis(..)
  ) where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Multiplicative

-- FIXME: cleanup
-- | element by element multiplication
--
-- > (a .*. b) .*. c == a .*. (b .*. c)
-- > singleton one .*. a = a
-- > a .*. singelton one = a
-- > a .*. b == b .*. a
class (Multiplication a) =>
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

--FIXME the hadamard product has a relationship between laws


-- | element by element addition
--
-- > (a .+. b) .+. c == a .+. (b .+. c)
-- > zero .+. a = a
-- > a .+. zero = a
-- > a .+. b == b .+. a
class (Addition a) =>
      AdditiveBasis m a where
  infixl 7 .+.
  (.+.) :: m a -> m a -> m a

-- | element by element subtraction
--
-- > a .-. a = pure zero
class (AbelianGroup (Sum a)) =>
      AdditiveGroupBasis m a where
  infixl 6 .-.
  (.-.) :: m a -> m a -> m a

