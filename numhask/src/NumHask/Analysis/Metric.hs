{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# OPTIONS_GHC -Wall #-}

-- | Metric classes
module NumHask.Analysis.Metric
  ( Signed(..)
  , Normed(..)
  , Metric(..)
  , Epsilon(..)
  , (≈)
  ) where

import qualified Prelude as P
import Prelude
       hiding (fromInteger, Bounded(..), Integral(..), (*), (/), (+), (-), abs, negate, sqrt, (**))

import Data.Complex (Complex(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Natural (Natural(..))
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Integral

-- | 'signum' from base is not an operator replicated in numhask, being such a very silly name, and preferred is the much more obvious 'sign'.  Compare with 'Norm' and 'Banach' where there is a change in codomain
--
-- > abs a * sign a == a
--
-- Generalising this class tends towards size and direction (abs is the size on the one-dim number line of a vector with its tail at zero, and sign is the direction, right?).
class (MultiplicativeUnital a) =>
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
class Normed a b where
  normL1 :: a -> b
  normL2 :: a -> b
  normLp :: b -> a -> b

instance Normed Double Double where
  normL1 = P.abs
  normL2 = P.abs
  normLp _ a = P.abs a

instance Normed Float Float where
  normL1 = P.abs
  normL2 = P.abs
  normLp _ a = P.abs a

instance Normed Int Int where
  normL1 = P.abs
  normL2 = P.abs
  normLp _ a = P.abs a

instance Normed Integer Integer where
  normL1 = P.abs
  normL2 = P.abs
  normLp _ a = P.abs a

instance (Multiplicative a, ExpField a, Normed a a) =>
         Normed (Complex a) a where
  normL1 (rx :+ ix) = normL1 rx + normL1 ix
  normL2 (rx :+ ix) = sqrt (rx * rx + ix * ix)
  normLp p (rx :+ ix) = (normL1 rx ** p + normL1 ix ** p) ** (one / p)

instance Normed Natural Natural where
  normL1 = P.abs
  normL2 = P.abs
  normLp _ a = P.abs a

instance Normed Int8 Int8 where
  normL1 = P.abs
  normL2 = P.abs
  normLp _ a = P.abs a

instance Normed Int16 Int16 where
  normL1 = P.abs
  normL2 = P.abs
  normLp _ a = P.abs a

instance Normed Int32 Int32 where
  normL1 = P.abs
  normL2 = P.abs
  normLp _ a = P.abs a

instance Normed Int64 Int64 where
  normL1 = P.abs
  normL2 = P.abs
  normLp _ a = P.abs a

instance Normed Word Word where
  normL1 = P.abs
  normL2 = P.abs
  normLp _ a = P.abs a

instance Normed Word8 Word8 where
  normL1 = P.abs
  normL2 = P.abs
  normLp _ a = P.abs a

instance Normed Word16 Word16 where
  normL1 = P.abs
  normL2 = P.abs
  normLp _ a = P.abs a

instance Normed Word32 Word32 where
  normL1 = P.abs
  normL2 = P.abs
  normLp _ a = P.abs a

instance Normed Word64 Word64 where
  normL1 = P.abs
  normL2 = P.abs
  normLp _ a = P.abs a

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
  distanceLp :: b -> a -> a -> b

instance Metric Double Double where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)
  distanceLp p a b = normLp p (a - b)

instance Metric Float Float where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)
  distanceLp p a b = normLp p (a - b)

instance Metric Int Int where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)
  distanceLp p a b = normLp p (a - b)

instance Metric Integer Integer where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)
  distanceLp p a b = normLp p (a - b)

instance (Multiplicative a, ExpField a, Normed a a) =>
         Metric (Complex a) a where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)
  distanceLp p a b = normLp p (a - b)

instance Metric Natural Natural where
  distanceL1 a b = fromInteger $ normL1 (toInteger a - toInteger b)
  distanceL2 a b = fromInteger $ normL2 (toInteger a - toInteger b)
  distanceLp p a b = fromInteger (normLp (toInteger p) (toInteger a - toInteger b))

instance Metric Int8 Int8 where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)
  distanceLp p a b = normLp p (a - b)

instance Metric Int16 Int16 where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)
  distanceLp p a b = normLp p (a - b)

instance Metric Int32 Int32 where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)
  distanceLp p a b = normLp p (a - b)

instance Metric Int64 Int64 where
  distanceL1 a b = normL1 (a - b)
  distanceL2 a b = normL2 (a - b)
  distanceLp p a b = normLp p (a - b)

-- fixme: circular distance may be more appropriate
instance Metric Word Word where
  distanceL1 a b = fromInteger $ normL1 (toInteger a - toInteger b)
  distanceL2 a b = fromInteger $ normL2 (toInteger a - toInteger b)
  distanceLp p a b = fromInteger (normLp (toInteger p) (toInteger a - toInteger b))

instance Metric Word8 Word8 where
  distanceL1 a b = fromInteger $ normL1 (toInteger a - toInteger b)
  distanceL2 a b = fromInteger $ normL2 (toInteger a - toInteger b)
  distanceLp p a b = fromInteger (normLp (toInteger p) (toInteger a - toInteger b))

instance Metric Word16 Word16 where
  distanceL1 a b = fromInteger $ normL1 (toInteger a - toInteger b)
  distanceL2 a b = fromInteger $ normL2 (toInteger a - toInteger b)
  distanceLp p a b = fromInteger (normLp (toInteger p) (toInteger a - toInteger b))

instance Metric Word32 Word32 where
  distanceL1 a b = fromInteger $ normL1 (toInteger a - toInteger b)
  distanceL2 a b = fromInteger $ normL2 (toInteger a - toInteger b)
  distanceLp p a b = fromInteger (normLp (toInteger p) (toInteger a - toInteger b))

instance Metric Word64 Word64 where
  distanceL1 a b = fromInteger $ normL1 (toInteger a - toInteger b)
  distanceL2 a b = fromInteger $ normL2 (toInteger a - toInteger b)
  distanceLp p a b = fromInteger (normLp (toInteger p) (toInteger a - toInteger b))

-- | todo: This should probably be split off into some sort of alternative Equality logic, but to what end?
class (Eq a, AdditiveUnital a) =>
      Epsilon a where
  nearZero :: a -> Bool
  nearZero a = a == zero

  aboutEqual :: a -> a -> Bool
  default aboutEqual :: AdditiveGroup a => a -> a -> Bool
  aboutEqual a b = nearZero $ a - b

  positive :: (Signed a) => a -> Bool
  positive a = a == abs a
  veryPositive :: (Signed a) => a -> Bool
  veryPositive a = P.not (nearZero a) && positive a
  veryNegative :: (Signed a) => a -> Bool
  veryNegative a = P.not (nearZero a P.|| positive a)

infixl 4 ≈

-- | todo: is utf perfectly acceptable these days?
(≈) :: (Epsilon a) => a -> a -> Bool
(≈) = aboutEqual

instance Epsilon Double where
  nearZero a = abs a <= (1e-12 :: Double)

instance Epsilon Float where
  nearZero a = abs a <= (1e-6 :: Float)

instance Epsilon Int

instance Epsilon Integer

instance (Epsilon a, AdditiveGroup a) => Epsilon (Complex a) where
  nearZero (rx :+ ix) = nearZero rx && nearZero ix
  aboutEqual a b = nearZero $ a - b

instance Epsilon Int8

instance Epsilon Int16

instance Epsilon Int32

instance Epsilon Int64

instance Epsilon Word

instance Epsilon Word8

instance Epsilon Word16

instance Epsilon Word32

instance Epsilon Word64

