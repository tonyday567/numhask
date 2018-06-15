{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-- | Element-by-element operations
module NumHask.Algebra.Linear.Hadamard
  ( 
  --   AdditiveBasis(..)
  -- , AdditiveGroupBasis(..)
  -- , MultiplicativeBasis(..)
  -- , MultiplicativeGroupBasis(..)
  ) where

import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Addition
import NumHask.Algebra.Abstract.Multiplication

-- FIXME: cleanup
-- | element by element multiplication
--
-- > (a .*. b) .*. c == a .*. (b .*. c)
-- > singleton one .*. a = a
-- > a .*. singelton one = a
-- > a .*. b == b .*. a
class (Multiplication a) =>
      MultiplicativeBasis m a where
  infixl 7 .*.
  (.*.) :: m a -> m a -> m a

-- | element by element division
--
-- > a ./. a == singleton one
class (Group (Mult a)) =>
      MultiplicativeGroupBasis m a where
  infixl 7 ./.
  (./.) :: m a -> m a -> m a

--FIXME the hadamard product has a relationship between laws


-- -- | element by element addition
-- --
-- -- > (a .+. b) .+. c == a .+. (b .+. c)
-- -- > zero .+. a = a
-- -- > a .+. zero = a
-- -- > a .+. b == b .+. a
-- class (Additive a) =>
--       AdditiveBasis m a where
--   infixl 7 .+.
--   (.+.) :: m a -> m a -> m a

-- -- | element by element subtraction
-- --
-- -- > a .-. a = singleton zero
-- class (AdditiveGroup a) =>
--       AdditiveGroupBasis m a where
--   infixl 6 .-.
--   (.-.) :: m a -> m a -> m a
