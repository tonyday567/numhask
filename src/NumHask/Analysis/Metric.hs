{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

-- | Metric classes
module NumHask.Analysis.Metric
  ( Signed (..),
    Normed (..),
    Metric (..),
    Epsilon (..),
    (~=),
    outBy,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural (..))
import NumHask.Algebra.Additive
import NumHask.Algebra.Lattice
import NumHask.Algebra.Multiplicative
import Prelude hiding
  ( (-),
    Bounded (..),
    Integral (..),
    negate,
    (*),
  )
import qualified Prelude as P

-- | 'signum' from base is not an operator replicated in numhask, being such a very silly name, and preferred is the much more obvious 'sign'.  Compare with 'Normed' where there is a change in codomain
--
-- > abs a * sign a == a
--
-- Generalising this class tends towards size and direction (abs is the size on the one-dim number line of a vector with its tail at zero, and sign is the direction, right?).
class
  (Multiplicative a) =>
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

-- | Cab be Normed
--
-- > norm a >= zero
-- > norm zero == zero
--
-- Note that the Normed codomain can be different to the domain.
class (Additive a, Additive b) => Normed a b where
  norm :: a -> b

instance Normed Double Double where
  norm = P.abs

instance Normed Float Float where
  norm = P.abs

instance Normed Int Int where
  norm = P.abs

instance Normed Integer Integer where
  norm = P.abs

instance Normed Natural Natural where
  norm = P.abs

instance Normed Int8 Int8 where
  norm = P.abs

instance Normed Int16 Int16 where
  norm = P.abs

instance Normed Int32 Int32 where
  norm = P.abs

instance Normed Int64 Int64 where
  norm = P.abs

instance Normed Word Word where
  norm = P.abs

instance Normed Word8 Word8 where
  norm = P.abs

instance Normed Word16 Word16 where
  norm = P.abs

instance Normed Word32 Word32 where
  norm = P.abs

instance Normed Word64 Word64 where
  norm = P.abs

-- | distance between numbers using L1 norm
--
-- > distance a b >= zero
-- > distance a a == zero
--
class Metric a b where
  distance :: a -> a -> b

instance Metric Double Double where
  distance a b = norm (a - b)

instance Metric Float Float where
  distance a b = norm (a - b)

instance Metric Int Int where
  distance a b = norm (a - b)

instance Metric Integer Integer where
  distance a b = norm (a - b)

instance Metric Natural Natural where
  distance a b = P.fromInteger $ norm (P.toInteger a - P.toInteger b)

instance Metric Int8 Int8 where
  distance a b = norm (a - b)

instance Metric Int16 Int16 where
  distance a b = norm (a - b)

instance Metric Int32 Int32 where
  distance a b = norm (a - b)

instance Metric Int64 Int64 where
  distance a b = norm (a - b)


-- fixme: circular distance may be more appropriate
instance Metric Word Word where
  distance a b = P.fromInteger $ norm (P.toInteger a - P.toInteger b)

instance Metric Word8 Word8 where
  distance a b = P.fromInteger $ norm (P.toInteger a - P.toInteger b)

instance Metric Word16 Word16 where
  distance a b = P.fromInteger $ norm (P.toInteger a - P.toInteger b)

instance Metric Word32 Word32 where
  distance a b = P.fromInteger $ norm (P.toInteger a - P.toInteger b)

instance Metric Word64 Word64 where
  distance a b = P.fromInteger $ norm (P.toInteger a - P.toInteger b)

-- | A small number, especially useful for approximate equality.
class
  (Eq a, Additive a, Subtractive a, MeetSemiLattice a) =>
  Epsilon a where
  epsilon :: a
  epsilon = zero

  nearZero :: a -> Bool
  nearZero a = epsilon `meetLeq` a && epsilon `meetLeq` negate a

  aboutEqual :: a -> a -> Bool
  aboutEqual a b = nearZero $ a - b

infixl 4 ~=

-- | About equal.
(~=) :: (Epsilon a) => a -> a -> Bool
(~=) = aboutEqual

-- | Errors in proofs tend to accumulate, and grow beyond epsilon.
--
-- > outBy 1 == (~=)
outBy :: (Epsilon a, Multiplicative a) => a -> a -> a -> Bool
outBy x a b = ((x * epsilon) `meetLeq` (a - b) && (x * epsilon) `meetLeq` negate (a - b))

-- | 1e-14
instance Epsilon Double where
  epsilon = 1e-14

-- | 1e-6
instance Epsilon Float where
  epsilon = 1e-6

-- | 0
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
