{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Highjacking 'Representable's to provide a basis to provide element-by-element operations

module NumHask.Algebra.Basis (
    AdditiveBasis(..)
  , AdditiveGroupBasis(..)
  , MultiplicativeBasis(..)
  , MultiplicativeGroupBasis(..)
  ) where

import Data.Functor.Rep
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Additive

-- | AdditiveBasis
-- element by element addition
class ( Representable m
      , Additive a ) =>
      AdditiveBasis m a where
    infixl 7 .+.
    (.+.) :: m a -> m a -> m a
    (.+.) = liftR2 (+)

instance (Representable r, Additive a) => AdditiveBasis r a

-- | AdditiveGroupBasis
-- element by element subtraction
class ( Representable m
      , AdditiveGroup a ) =>
      AdditiveGroupBasis m a where
    infixl 6 .-.
    (.-.) :: m a -> m a -> m a
    (.-.) = liftR2 (-)

instance (Representable r, AdditiveGroup a) => AdditiveGroupBasis r a

-- | MultiplicativeBasis
-- element by element multiplication
class ( Representable m
      , Multiplicative a ) =>
      MultiplicativeBasis m a where
    infixl 7 .*.
    (.*.) :: m a -> m a -> m a
    (.*.) = liftR2 (*)

instance (Representable r, Multiplicative a) => MultiplicativeBasis r a

-- | MultiplicativeGroupBasis
-- element by element division
class ( Representable m
      , MultiplicativeGroup a ) =>
      MultiplicativeGroupBasis m a where
    infixl 7 ./.
    (./.) :: m a -> m a -> m a
    (./.) = liftR2 (/)

instance (Representable r, MultiplicativeGroup a) => MultiplicativeGroupBasis r a
