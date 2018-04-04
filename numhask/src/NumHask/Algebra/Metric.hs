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
       hiding (Bounded(..), Integral(..), (*), (+), (-), abs, negate, sqrt)

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

-- | Like Signed, except the codomain can be different to the domain.
class Normed a b where
  size :: a -> b

instance Normed Double Double where
  size = P.abs

instance Normed Float Float where
  size = P.abs

instance Normed Int Int where
  size = P.abs

instance Normed Integer Integer where
  size = P.abs

instance (Multiplicative a, ExpField a, Normed a a) =>
         Normed (Complex a) a where
  size (rx :+ ix) = sqrt (rx * rx + ix * ix)

-- | distance between numbers
--
-- > distance a b >= zero
-- > distance a a == zero
-- > \a b c -> distance a c + distance b c - distance a b >= zero &&
-- >           distance a b + distance b c - distance a c >= zero &&
-- >           distance a b + distance a c - distance b c >= zero &&
class Metric a b where
  distance :: a -> a -> b

instance Metric Double Double where
  distance a b = abs (a - b)

instance Metric Float Float where
  distance a b = abs (a - b)

instance Metric Int Int where
  distance a b = abs (a - b)

instance Metric Integer Integer where
  distance a b = abs (a - b)

instance (Multiplicative a, ExpField a, Normed a a) =>
         Metric (Complex a) a where
  distance a b = size (a - b)

-- | todo: This should probably be split off into some sort of alternative Equality logic, but to what end?
class (AdditiveGroup a) =>
      Epsilon a where
  nearZero :: a -> Bool
  aboutEqual :: a -> a -> Bool
  positive :: (Eq a, Signed a) => a -> Bool
  positive a = a == abs a
  veryPositive :: (Eq a, Signed a) => a -> Bool
  veryPositive a = P.not (nearZero a) && positive a
  veryNegative :: (Eq a, Signed a) => a -> Bool
  veryNegative a = P.not (nearZero a P.|| positive a)

infixl 4 ≈

-- | todo: is utf perfectly acceptable these days?
(≈) :: (Epsilon a) => a -> a -> Bool
(≈) = aboutEqual

instance Epsilon Double where
  nearZero a = abs a <= (1e-12 :: Double)
  aboutEqual a b = nearZero $ a - b

instance Epsilon Float where
  nearZero a = abs a <= (1e-6 :: Float)
  aboutEqual a b = nearZero $ a - b

instance Epsilon Int where
  nearZero a = a == zero
  aboutEqual a b = nearZero $ a - b

instance Epsilon Integer where
  nearZero a = a == zero
  aboutEqual a b = nearZero $ a - b

instance (Epsilon a) => Epsilon (Complex a) where
  nearZero (rx :+ ix) = nearZero rx && nearZero ix
  aboutEqual a b = nearZero $ a - b
