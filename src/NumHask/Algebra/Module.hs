{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Algebra for Representable numbers
module NumHask.Algebra.Module
  ( AdditiveModule(..)
  , AdditiveGroupModule(..)
  , MultiplicativeModule(..)
  , MultiplicativeGroupModule(..)
  , Banach(..)
  , Hilbert(..)
  , inner
  , type (><)
  , TensorProduct(..)
  ) where

import Data.Functor.Rep
import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Metric
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import Protolude
       (Double, Float, Foldable(..), Functor(..), Int, Integer, ($))

-- | Additive Module Laws
--
-- > (a + b) .+ c == a + (b .+ c)
-- > (a + b) .+ c == (a .+ c) + b
-- > a .+ zero == a
-- > a .+ b == b +. a
class (Representable r, Additive a) =>
      AdditiveModule r a where
  infixl 6 .+
  (.+) :: r a -> a -> r a
  r .+ a = fmap (a +) r
  infixl 6 +.
  (+.) :: a -> r a -> r a
  a +. r = fmap (a +) r

instance (Representable r, Additive a) => AdditiveModule r a

-- | Subtraction Module Laws
--
-- > (a + b) .- c == a + (b .- c)
-- > (a + b) .- c == (a .- c) + b
-- > a .- zero == a
-- > a .- b == negate b +. a
class (Representable r, AdditiveGroup a) =>
      AdditiveGroupModule r a where
  infixl 6 .-
  (.-) :: r a -> a -> r a
  r .- a = fmap (\x -> x - a) r
  infixl 6 -.
  (-.) :: a -> r a -> r a
  a -. r = fmap (\x -> a - x) r

instance (Representable r, AdditiveGroup a) => AdditiveGroupModule r a

-- | Multiplicative Module Laws
--
-- > a .* one == a
-- > (a + b) .* c == (a .* c) + (b .* c)
-- > c *. (a + b) == (c *. a) + (c *. b)
-- > a .* zero == zero
-- > a .* b == b *. a
class (Representable r, Multiplicative a) =>
      MultiplicativeModule r a where
  infixl 7 .*
  (.*) :: r a -> a -> r a
  r .* a = fmap (a *) r
  infixl 7 *.
  (*.) :: a -> r a -> r a
  a *. r = fmap (a *) r

instance (Representable r, Multiplicative a) => MultiplicativeModule r a

-- | Division Module Laws
--
-- > nearZero a || a ./ one == a
-- > b == zero || a ./ b == recip b *. a
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

-- | Banach (with Norm) laws form rules around size and direction of a number, with a potential crossing into another codomain.
--
-- > a == singleton zero || normalize a *. size a == a
class (Representable r, ExpField a, Normed (r a) a) =>
      Banach r a where
  normalize :: r a -> r a
  normalize a = a ./ size a

instance (Normed (r a) a, ExpField a, Representable r) => Banach r a

-- | the inner product of a representable over a semiring
--
-- > a <.> b == b <.> a
-- > a <.> (b +c) == a <.> b + a <.> c
-- > a <.> (s *. b + c) == s * (a <.> b) + a <.> c
-- (s0 *. a) <.> (s1 *. b) == s0 * s1 * (a <.> b)
class (Semiring a, Foldable r, Representable r) =>
      Hilbert r a where
  infix 8 <.>
  (<.>) :: r a -> r a -> a
  (<.>) a b = sum $ liftR2 times a b

-- | synonym for (<.>)
inner :: (Hilbert r a) => r a -> r a -> a
inner = (<.>)

-- | tensorial type
type family (><) (a :: k1) (b :: k2) :: *

type instance Int >< Int = Int

type instance Integer >< Integer = Integer

type instance Double >< Double = Double

type instance Float >< Float = Float

-- | representation synthesis
type family TensorRep k1 k2 where
  TensorRep (r a) (r a) = r (r a)
  TensorRep (r a) (s a) = r (s a)
  TensorRep (r a) a = r a

type instance r a >< b = TensorRep (r a) b

-- | generalised outer product
--
-- > a><b + c><b == (a+c) >< b
-- > a><b + a><c == a >< (b+c)
--
-- todo: work out why these laws down't apply
-- > a *. (b><c) == (a><b) .* c
-- > (a><b) .* c == a *. (b><c)
class TensorProduct a where
  infix 8 ><
  (><) :: a -> a -> (a >< a)
  outer :: a -> a -> (a >< a)
  outer = (><)
  timesleft :: a -> (a >< a) -> a
  timesright :: (a >< a) -> a -> a

instance (Hilbert r a, Multiplicative a) => TensorProduct (r a) where
  (><) m n = tabulate (\i -> index m i *. n)
  timesleft v m = tabulate (\i -> v <.> index m i)
  timesright m v = tabulate (\i -> v <.> index m i)
