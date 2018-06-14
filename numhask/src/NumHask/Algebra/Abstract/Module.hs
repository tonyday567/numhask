{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Algebra for Modules
module NumHask.Algebra.Abstract.Module
  ( AdditiveModule(..)
  , AdditiveGroupModule(..)
  , MultiplicativeModule(..)
  , MultiplicativeGroupModule(..)
  , Banach(..)
  , Hilbert(..)
  , type (><)
  , TensorProduct(..)
  ) where

import NumHask.Algebra.Abstract.Addition
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Metric
import NumHask.Algebra.Abstract.Multiplication
import NumHask.Algebra.Abstract.Ring
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
class (Addition a) =>
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
