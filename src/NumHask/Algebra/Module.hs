{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Algebra

module NumHask.Algebra.Module (
    -- * Module
    AdditiveModule(..)
  , AdditiveGroupModule(..)
  , MultiplicativeModule(..)
  , MultiplicativeGroupModule(..)
    -- * Tensoring
  , Banach(..)
  , Hilbert(..)
  , type (><)
  , TensorProduct(..)
  ) where

import Protolude (Double, Float, Int, Integer, Functor(..), ($), Foldable(..))
import Data.Functor.Rep
import NumHask.Algebra.Additive
import NumHask.Algebra.Exponential
import NumHask.Algebra.Metric
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring

-- * Additive Module Structure

-- | AdditiveModule
class ( Representable m
      , Additive a) =>
      AdditiveModule m a where
    infixl 6 .+
    (.+) :: m a -> a -> m a
    m .+ a = fmap (a+) m

    infixl 6 +.
    (+.) :: a -> m a -> m a
    a +. m = fmap (a+) m

instance (Representable r, Additive a) => AdditiveModule r a

-- | AdditiveGroupModule
class ( Representable m
      , AdditiveGroup a) =>
      AdditiveGroupModule m a where
    infixl 6 .-
    (.-) :: m a -> a -> m a
    m .- a = fmap (\x -> x - a) m

    infixl 6 -.
    (-.) :: a -> m a -> m a
    a -. m = fmap (\x -> a - x) m

instance (Representable r, AdditiveGroup a) => AdditiveGroupModule r a

-- * Multiplicative Module Structure
-- | MultiplicativeModule
class ( Representable m
      , Multiplicative a) =>
      MultiplicativeModule m a where
    infixl 7 .*
    (.*) :: m a -> a -> m a
    m .* a = fmap (a*) m

    infixl 7 *.
    (*.) :: a -> m a -> m a
    a *. m = fmap (a*) m

instance (Representable r, Multiplicative a) => MultiplicativeModule r a

-- | MultiplicativeGroupModule
class ( Representable m
      , MultiplicativeGroup a) =>
      MultiplicativeGroupModule m a where
    infixl 7 ./
    (./) :: m a -> a -> m a
    m ./ a = fmap (/ a) m

    infixl 7 /.
    (/.) :: a -> m a -> m a
    a /. m = fmap (\x -> a / x) m

instance (Representable r, MultiplicativeGroup a) => MultiplicativeGroupModule r a

-- | Banach
class ( Representable m
      , ExpField a
      , Normed (m a) a) =>
      Banach m a where
    normalize :: m a -> m a
    normalize a = a ./ size a

instance (Normed (r a) a, ExpField a, Representable r) => Banach r a

-- | Hilbert
class (Additive (m a)) => Hilbert m a where
    infix 8 <.>
    (<.>) :: m a -> m a -> a

instance (Additive (r a), Foldable r, Representable r, CRing a) =>
    Hilbert r a where
    (<.>) a b = foldl' (+) zero $ liftR2 (*) a b

-- | tensorial tomfoolery
type family (><) (a::k1) (b::k2) :: *

type instance Int >< Int = Int
type instance Integer >< Integer = Integer
type instance Double >< Double = Double
type instance Float >< Float = Float

type family TensorRep k1 k2 where
    TensorRep (r a) (r a) = r (r a)
    TensorRep (r a) a = r a

type instance r a >< b = TensorRep (r a) b

-- | TensorAlgebra
class TensorProduct a where
    infix 8 ><
    (><) :: a -> a -> (a><a)
    timesleft :: a -> (a><a) -> a
    timesright :: (a><a) -> a -> a

instance (AdditiveGroup (r a), Foldable r, Representable r, CRing a ) =>
    TensorProduct (r a)
  where
    (><) m n = tabulate (\i -> index m i *. n)
    timesleft v m = tabulate (\i -> v <.> index m i)
    timesright m v = tabulate (\i -> v <.> index m i)
