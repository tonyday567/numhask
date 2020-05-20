{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

-- | Metric classes
module NumHask.Analysis.Metric
  ( Signed(..)
  , Normed(..)
  , Metric(..)
  , Epsilon(..)
  , (~=)
  )
where

import qualified Prelude as P
import Prelude
  hiding ( Bounded(..)
  , Integral(..)
  , (-)
  , negate
  )

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Natural (Natural(..))
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Lattice

-- | 'signum' from base is not an operator replicated in numhask, being such a very silly name, and preferred is the much more obvious 'sign'.  Compare with 'Norm' where there is a change in codomain
--
-- > abs a * sign a == a
--
-- Generalising this class tends towards size and direction (abs is the size on the one-dim number line of a vector with its tail at zero, and sign is the direction, right?).
class (Multiplicative a) =>
  Signed a where
  sign :: a -> a
  abs :: a -> a

instance Signed Double where
  sign a
    | a == zero = zero
    | a > zero = one
    | otherwise = negate one
  abs = P.abs

instance Signed Float where
  sign a
    | a == zero = zero
    | a > zero = one
    | otherwise = negate one
  abs = P.abs

instance Signed Int where
  sign a
    | a == zero = zero
    | a > zero = one
    | otherwise = negate one
  abs = P.abs

instance Signed Integer where
  sign a
    | a == zero = zero
    | a > zero = one
    | otherwise = negate one
  abs = P.abs

instance Signed Natural where
  sign a
    | a == zero = zero
    | otherwise = one
  abs = id

instance Signed Int8 where
  sign a
    | a == zero = zero
    | a > zero = one
    | otherwise = negate one
  abs = P.abs

instance Signed Int16 where
  sign a
    | a == zero = zero
    | a > zero = one
    | otherwise = negate one
  abs = P.abs

instance Signed Int32 where
  sign a
    | a == zero = zero
    | a > zero = one
    | otherwise = negate one
  abs = P.abs

instance Signed Int64 where
  sign a
    | a == zero = zero
    | a > zero = one
    | otherwise = negate one
  abs = P.abs

instance Signed Word where
  sign a
    | a == zero = zero
    | otherwise = one
  abs = P.abs

instance Signed Word8 where
  sign a
    | a == zero = zero
    | otherwise = one
  abs = P.abs

instance Signed Word16 where
  sign a
    | a == zero = zero
    | otherwise = one
  abs = P.abs

instance Signed Word32 where
  sign a
    | a == zero = zero
    | otherwise = one
  abs = P.abs

instance Signed Word64 where
  sign a
    | a == zero = zero
    | otherwise = one
  abs = P.abs

-- | L1 and L2 norms are provided for potential speedups, as well as the generalized p-norm.
--
-- for p >= 1
--
-- > normLp p a >= zero
-- > normLp p zero == zero
--
-- Note that the Normed codomain can be different to the domain.
--
class (Additive a, Additive b) => Normed a b where
  normL1 :: a -> b
  normL2 :: a -> b

instance Normed Double Double where
  normL1 = P.abs
  normL2 = P.abs

instance Normed Float Float where
  normL1 = P.abs
  normL2 = P.abs

instance Normed Int Int where
  normL1 = P.abs
  normL2 = P.abs

instance Normed Integer Integer where
  normL1 = P.abs
  normL2 = P.abs

instance Normed Natural Natural where
  normL1 = P.abs
  normL2 = P.abs

instance Normed Int8 Int8 where
  normL1 = P.abs
  normL2 = P.abs

instance Normed Int16 Int16 where
  normL1 = P.abs
  normL2 = P.abs

instance Normed Int32 Int32 where
  normL1 = P.abs
  normL2 = P.abs

instance Normed Int64 Int64 where
  normL1 = P.abs
  normL2 = P.abs

instance Normed Word Word where
  normL1 = P.abs
  normL2 = P.abs

instance Normed Word8 Word8 where
  normL1 = P.abs
  normL2 = P.abs

instance Normed Word16 Word16 where
  normL1 = P.abs
  normL2 = P.abs

instance Normed Word32 Word32 where
  normL1 = P.abs
  normL2 = P.abs

instance Normed Word64 Word64 where
  normL1 = P.abs
  normL2 = P.abs

-- | distance between numbers using L1, L2 or Lp-norms
--
-- > distanceL2 a b >= zero
-- > distanceL2 a a == zero
-- > \a b c -> distanceL2 a c + distanceL2 b c - distanceL2 a b >= zero &&
-- >           distanceL2 a b + distanceL2 b c - distanceL2 a c >= zero &&
-- >           distanceL2 a b + distanceL2 a c - distanceL2 b c >= zero &&
class Metric a b where
  distanceL1 :: a -> a -> b
  distanceL2 :: a -> a -> b

instance Metric Double Double where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)

instance Metric Float Float where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)

instance Metric Int Int where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)

instance Metric Integer Integer where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)

instance Metric Natural Natural where
  distanceL1 a b = P.fromInteger $ normL1 (P.toInteger a - P.toInteger b)
  distanceL2 a b = P.fromInteger $ normL2 (P.toInteger a - P.toInteger b)

instance Metric Int8 Int8 where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)

instance Metric Int16 Int16 where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)

instance Metric Int32 Int32 where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)

instance Metric Int64 Int64 where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)

-- fixme: circular distance may be more appropriate
instance Metric Word Word where
  distanceL1 a b = P.fromInteger $ normL1 (P.toInteger a - P.toInteger b)
  distanceL2 a b = P.fromInteger $ normL2 (P.toInteger a - P.toInteger b)

instance Metric Word8 Word8 where
  distanceL1 a b = P.fromInteger $ normL1 (P.toInteger a - P.toInteger b)
  distanceL2 a b = P.fromInteger $ normL2 (P.toInteger a - P.toInteger b)

instance Metric Word16 Word16 where
  distanceL1 a b = P.fromInteger $ normL1 (P.toInteger a - P.toInteger b)
  distanceL2 a b = P.fromInteger $ normL2 (P.toInteger a - P.toInteger b)

instance Metric Word32 Word32 where
  distanceL1 a b = P.fromInteger $ normL1 (P.toInteger a - P.toInteger b)
  distanceL2 a b = P.fromInteger $ normL2 (P.toInteger a - P.toInteger b)

instance Metric Word64 Word64 where
  distanceL1 a b = P.fromInteger $ normL1 (P.toInteger a - P.toInteger b)
  distanceL2 a b = P.fromInteger $ normL2 (P.toInteger a - P.toInteger b)

class (Eq a, Additive a, Subtractive a, MeetSemiLattice a) =>
  Epsilon a where

  epsilon :: a
  epsilon = zero

  nearZero :: a -> Bool
  nearZero a = epsilon `meetLeq` a && epsilon `meetLeq` negate a

  aboutEqual :: a -> a -> Bool
  aboutEqual a b = nearZero $ a - b

infixl 4 ~=

(~=) :: (Epsilon a) => a -> a -> Bool
(~=) = aboutEqual

instance Epsilon Double where
  epsilon = 1e-14

instance Epsilon Float where
  epsilon = 1e-6

instance Epsilon Int

instance Epsilon Integer

instance Epsilon Int8

instance Epsilon Int16

instance Epsilon Int32

instance Epsilon Int64

instance Epsilon Word

instance Epsilon Word8

instance Epsilon Word16

instance Epsilon Word32

instance Epsilon Word64
