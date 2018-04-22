{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

-- | Metric classes
module NumHask.Algebra.Metric
  ( Signed(..)
  , Normed(..)
  , Metric(..)
  , Epsilon(..)
  , (≈)
  ) where

import qualified Prelude as P
import Prelude
       hiding (Bounded(..), Integral(..), (*), (/), (+), (-), abs, negate, sqrt, (**))

import Data.Complex (Complex(..))
import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Multiplicative

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

-- | distance between numbers using L1, L2 or Lp-norms
--
-- > distanceL2 a b >= zero
-- > distanceL2 a a == zero
-- > \a b c -> distanceL2 a c + distanceL2 b c - distanceL2 a b >= zero &&
-- >           distanceL2 a b + distanceL2 b c - distanceL2 a c >= zero &&
-- >           distanceL2 a b + distanceL2 a c - distanceL2 b c >= zero &&
class (AdditiveGroup a, Normed a b) => Metric a b where
  distanceL1 :: a -> a -> b
  distanceL1 a b = normL1 (a - b)

  distanceL2 :: a -> a -> b
  distanceL2 a b = normL2 (a - b)

  distanceLp :: b -> a -> a -> b
  distanceLp p a b = normLp p (a - b)

instance Metric Double Double

instance Metric Float Float

instance Metric Int Int

instance Metric Integer Integer

instance (Multiplicative a, ExpField a, Normed a a) =>
         Metric (Complex a) a

-- | todo: This should probably be split off into some sort of alternative Equality logic, but to what end?
class (Eq a, AdditiveGroup a) =>
      Epsilon a where
  nearZero :: a -> Bool
  nearZero a = a == zero

  aboutEqual :: a -> a -> Bool
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

instance (Epsilon a) => Epsilon (Complex a) where
  nearZero (rx :+ ix) = nearZero rx && nearZero ix
  aboutEqual a b = nearZero $ a - b
