{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Algebra for Modules
module NumHask.Algebra.Module
  ( AdditiveModule(..)
  , AdditiveGroupModule(..)
  , MultiplicativeModule(..)
  , MultiplicativeGroupModule(..)
  , Banach(..)
  , Hilbert(..)
  , type (><)
  , TensorProduct(..)
  ) where

import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Metric
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Natural
import Prelude
       (Double, Float, Int, Integer)

-- | Additive Module Laws
--
-- > (a + b) .+ c == a + (b .+ c)
-- > (a + b) .+ c == (a .+ c) + b
-- > a .+ zero == a
-- > a .+ b == b +. a
class (Additive a) =>
      AdditiveModule r a where
  infixl 6 .+
  (.+) :: r a -> a -> r a

  infixl 6 +.
  (+.) :: a -> r a -> r a

-- | Subtraction Module Laws
--
-- > (a + b) .- c == a + (b .- c)
-- > (a + b) .- c == (a .- c) + b
-- > a .- zero == a
-- > a .- b == negate b +. a
class (AdditiveGroup a, AdditiveModule r a) =>
      AdditiveGroupModule r a where
  infixl 6 .-
  (.-) :: r a -> a -> r a

  infixl 6 -.
  (-.) :: a -> r a -> r a

-- | Multiplicative Module Laws
--
-- > a .* one == a
-- > (a + b) .* c == (a .* c) + (b .* c)
-- > c *. (a + b) == (c *. a) + (c *. b)
-- > a .* zero == zero
-- > a .* b == b *. a
class (Multiplicative a) =>
      MultiplicativeModule r a where
  infixl 7 .*
  (.*) :: r a -> a -> r a
  infixl 7 *.
  (*.) :: a -> r a -> r a

-- | Division Module Laws
--
-- > nearZero a || a ./ one == a
-- > b == zero || a ./ b == recip b *. a
class (MultiplicativeGroup a, MultiplicativeModule r a) =>
      MultiplicativeGroupModule r a where
  infixl 7 ./
  (./) :: r a -> a -> r a
  infixl 7 /.
  (/.) :: a -> r a -> r a

-- | Banach (with Norm) laws form rules around size and direction of a number, with a potential crossing into another codomain.
--
-- > a == singleton zero || normalizeL2 a *. normL2 a == a
class (ExpField a, Normed (r a) a, MultiplicativeGroupModule r a) =>
      Banach r a where
  normalizeL1 :: r a -> r a
  normalizeL1 a = a ./ normL1 a

  normalizeL2 :: r a -> r a
  normalizeL2 a = a ./ normL2 a

  normalizeLp :: a -> r a -> r a
  normalizeLp p a = a ./ normLp p a

-- | the inner product of a representable over a semiring
--
-- > a <.> b == b <.> a
-- > a <.> (b +c) == a <.> b + a <.> c
-- > a <.> (s *. b + c) == s * (a <.> b) + a <.> c
-- (s0 *. a) <.> (s1 *. b) == s0 * s1 * (a <.> b)
class (Semiring a) =>
      Hilbert r a where
  infix 8 <.>
  (<.>) :: r a -> r a -> a

-- | tensorial type
type family (><) (a :: k1) (b :: k2) :: *

type instance Int >< Int = Int

type instance Integer >< Integer = Integer

type instance Double >< Double = Double

type instance Float >< Float = Float

type instance Natural >< Natural = Natural

type instance Int8 >< Int8 = Int8

type instance Int16 >< Int16 = Int16

type instance Int32 >< Int32 = Int32

type instance Int64 >< Int64 = Int64

type instance Word >< Word = Word

type instance Word8 >< Word8 = Word8

type instance Word16 >< Word16 = Word16

type instance Word32 >< Word32 = Word32

type instance Word64 >< Word64 = Word64

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
