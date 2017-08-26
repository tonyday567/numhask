{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

-- | Element-by-element operation for 'Representable's
module NumHask.Algebra.Basis
  ( AdditiveBasis(..)
  , AdditiveGroupBasis(..)
  , MultiplicativeBasis(..)
  , MultiplicativeGroupBasis(..)
  ) where

import Data.Functor.Rep
import NumHask.Algebra.Additive
import NumHask.Algebra.Multiplicative

-- | element by element addition
--
-- > (a .+. b) .+. c == a .+. (b .+. c)
-- > zero .+. a = a
-- > a .+. zero = a
-- > a .+. b == b .+. a
class (Representable m, Additive a) =>
      AdditiveBasis m a where
  infixl 7 .+.
  (.+.) :: m a -> m a -> m a
  (.+.) = liftR2 (+)

instance (Representable r, Additive a) => AdditiveBasis r a

-- | element by element subtraction
--
-- > a .-. a = singleton zero
class (Representable m, AdditiveGroup a) =>
      AdditiveGroupBasis m a where
  infixl 6 .-.
  (.-.) :: m a -> m a -> m a
  (.-.) = liftR2 (-)

instance (Representable r, AdditiveGroup a) => AdditiveGroupBasis r a

-- | element by element multiplication
--
-- > (a .*. b) .*. c == a .*. (b .*. c)
-- > singleton one .*. a = a
-- > a .*. singelton one = a
-- > a .*. b == b .*. a
class (Representable m, Multiplicative a) =>
      MultiplicativeBasis m a where
  infixl 7 .*.
  (.*.) :: m a -> m a -> m a
  (.*.) = liftR2 (*)

instance (Representable r, Multiplicative a) => MultiplicativeBasis r a

-- | element by element division
--
-- > a ./. a == singleton one
class (Representable m, MultiplicativeGroup a) =>
      MultiplicativeGroupBasis m a where
  infixl 7 ./.
  (./.) :: m a -> m a -> m a
  (./.) = liftR2 (/)

instance (Representable r, MultiplicativeGroup a) =>
         MultiplicativeGroupBasis r a
