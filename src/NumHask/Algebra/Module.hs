{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Algebra for Representable number interaction with elements
module NumHask.Algebra.Module
    -- * Module
  ( AdditiveModule(..)
  , AdditiveGroupModule(..)
  , MultiplicativeModule(..)
  , MultiplicativeGroupModule(..)
    -- * Tensoring
  , Banach(..)
  , Hilbert(..)
  , inner
  , type (><)
  , TensorProduct(..)
  ) where

import Data.Functor.Rep
import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Ring
import NumHask.Algebra.Metric
import NumHask.Algebra.Multiplicative
import Protolude
       (Double, Float, Foldable(..), Functor(..), Int, Integer, ($))

-- * Additive Module Structure
-- | AdditiveModule
class (Representable r, Additive a) =>
      AdditiveModule r a where
  infixl 6 .+
  (.+) :: r a -> a -> r a
  r .+ a = fmap (a +) r
  infixl 6 +.
  (+.) :: a -> r a -> r a
  a +. r = fmap (a +) r

instance (Representable r, Additive a) => AdditiveModule r a

-- | AdditiveGroupModule
class (Representable r, AdditiveGroup a) =>
      AdditiveGroupModule r a where
  infixl 6 .-
  (.-) :: r a -> a -> r a
  r .- a = fmap (\x -> x - a) r
  infixl 6 -.
  (-.) :: a -> r a -> r a
  a -. r = fmap (\x -> a - x) r

instance (Representable r, AdditiveGroup a) => AdditiveGroupModule r a

-- * Multiplicative Module Structure
-- | MultiplicativeModule
class (Representable r, Multiplicative a) =>
      MultiplicativeModule r a where
  infixl 7 .*
  (.*) :: r a -> a -> r a
  r .* a = fmap (a *) r
  infixl 7 *.
  (*.) :: a -> r a -> r a
  a *. r = fmap (a *) r

instance (Representable r, Multiplicative a) => MultiplicativeModule r a

-- | MultiplicativeGroupModule
class (Representable r, MultiplicativeGroup a) =>
      MultiplicativeGroupModule r a where
  infixl 7 ./
  (./) :: r a -> a -> r a
  r ./ a = fmap (/ a) r
  infixl 7 /.
  (/.) :: a -> r a -> r a
  a /. r = fmap (\x -> a / x) r

instance (Representable r, MultiplicativeGroup a) =>
         MultiplicativeGroupModule r a

-- | Banach
class (Representable r, ExpField a, Normed (r a) a) =>
      Banach r a where
  normalize :: r a -> r a
  normalize a = a ./ size a

instance (Normed (r a) a, ExpField a, Representable r) => Banach r a

-- | Hilbert
class ( Semiring a
      , Foldable r
      , Representable r
      ) =>
      Hilbert r a where
  infix 8 <.>
  (<.>) :: r a -> r a -> a
  (<.>) a b = sum $ liftR2 times a b

inner :: (Hilbert r a) => r a -> r a -> a
inner = (<.>)

-- | tensorial type
type family (><) (a :: k1) (b :: k2) :: *

type instance Int >< Int = Int

type instance Integer >< Integer = Integer

type instance Double >< Double = Double

type instance Float >< Float = Float

type family TensorRep k1 k2 where
  TensorRep (r a) (r a) = r (r a)
  TensorRep (r a) (s a) = r (s a)
  TensorRep (r a) a = r a

type instance r a >< b = TensorRep (r a) b

-- | TensorAlgebra
class TensorProduct a where
  infix 8 ><
  (><) :: a -> a -> (a >< a)
  outer :: a -> a -> (a >< a)
  outer = (><)
  timesleft :: a -> (a >< a) -> a
  timesright :: (a >< a) -> a -> a

instance (Hilbert r a, Multiplicative a) =>
         TensorProduct (r a) where
  (><) m n = tabulate (\i -> index m i *. n)
  timesleft v m = tabulate (\i -> v <.> index m i)
  timesright m v = tabulate (\i -> v <.> index m i)
