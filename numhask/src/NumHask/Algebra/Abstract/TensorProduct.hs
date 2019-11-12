{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

-- | TensorProduct
module NumHask.Algebra.Abstract.TensorProduct
  ( TensorProduct(..)
  , TensorProduct'(..)
  , type (><)
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Natural
import Prelude (Double, Float, Int, Integer)

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
-- > a *. (b><c) == (a><b) .* c
-- > (a><b) .* c == a *. (b><c)
class TensorProduct a where
  infix 8 ><
  (><) :: a -> a -> (a >< a)
  outer :: a -> a -> (a >< a)
  outer = (><)
  timesleft :: a -> (a >< a) -> a
  timesright :: (a >< a) -> a -> a

-- | generalised outer product
--
-- > a><b + c><b == (a+c) >< b
-- > a><b + a><c == a >< (b+c)
-- > a *. (b><c) == (a><b) .* c
-- > (a><b) .* c == a *. (b><c)
class TensorProduct' a b where
  outer' :: a -> b -> (a >< b)
  timesleft' :: a -> (a >< b) -> b
  timesright' :: (a >< b) -> a -> b
