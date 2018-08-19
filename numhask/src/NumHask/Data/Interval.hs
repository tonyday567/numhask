{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

-- | Interval
module NumHask.Data.Interval
  ( Space(..)
  , Pos(..)
  , FieldSpace(..)
  , Interval(..)
  -- , CanInterval(..)
  -- , eps
  -- , whole
  -- , emptyInterval
  -- , singletonInterval
  -- , above
  -- , below
  -- , increasing
  -- , decreasing
  )
where

import Data.Data (Data)
import GHC.Generics (Generic, Generic1)
import GHC.Exts (fromString)
import qualified Prelude as P
import Prelude (Ord(..), Eq(..), Bool(..), Traversable, Foldable, Functor, Read, Show, ($), (.), otherwise, (||), (&&), not, maximum, minimum)
import Data.Bool (bool)
import NumHask.Algebra.Abstract as NH
import NumHask.Analysis.Metric
import NumHask.Data.Integral
-- import NumHask.Data.Rational
-- import Data.Distributive as D

class (Eq (Element s)) => Space s where
  type Element s :: *
  -- | the containing space of a Foldable
  space :: (Foldable f) => f (Element s) -> s
  space = P.foldr (\a x -> x `union` singleton a) nul

  infix 3 ...
  (...) :: Element s -> Element s -> s
  (...) a b = space [a,b]

  infixl 6 +/-
  (+/-) :: Element s -> Element s -> s

  -- | lower boundary
  lower :: s -> Element s
  -- | upper boundary
  upper :: s -> Element s

  -- | this differs from minimum as the lowest element can potentially not exist in the list
  lowest :: (Foldable f) => s -> f (Element s) -> Element s
  default lowest :: (Ord (Element s), Foldable f) => s -> f (Element s) -> Element s
  lowest _ = minimum

  -- | this differs from maximum as the lowest element can potentially not exist in the list
  highest :: (Foldable f) => s -> f (Element s) -> Element s
  default highest :: (Ord (Element s), Foldable f) => s -> f (Element s) -> Element s
  highest _ = maximum

  -- | distance between boundaries
  width :: s -> Element s

  -- | singleton space
  singleton :: Element s -> s

  -- | zero-width test
  singular :: s -> Bool
  singular s = lower s == upper s

  -- | determine whether an a is in the space
  element :: Element s -> s -> Bool
  default element :: (Ord (Element s), Eq s) => Element s -> s -> Bool
  element a s = bool (a >= lower s && a <= upper s) False (s==nul)

  -- | is a space contained within another?
  contains :: s -> s -> Bool
  default contains :: (Ord (Element s)) => s -> s -> Bool
  contains s0 s1 =
      ( lower s0 <= lower s1 &&
        upper s0 >= upper s1) ||
      ( lower s1 <= lower s0 &&
        upper s1 >= upper s0)

  -- | do two spaces disjoint?
  disjoint :: s -> s -> Bool
  default disjoint :: (Ord (Element s)) => s -> s -> Bool
  disjoint s0 s1 = above s0 s1 || below s0 s1

  -- | is one space completely above the other
  above :: s -> s -> Bool
  default above :: (Ord (Element s)) => s -> s -> Bool
  above s0 s1 =
    isNul s0 || isNul s1 ||
    lower s0 > lower s1 &&
    lower s0 > upper s1 &&
    upper s0 > lower s1 &&
    upper s0 > upper s1

  -- | is one space completely below the other
  below :: s -> s -> Bool
  default below :: (Ord (Element s)) => s -> s -> Bool
  below s0 s1 =
    isNul s0 || isNul s1 ||
    lower s0 < lower s1 &&
    lower s0 < upper s1 &&
    upper s0 < lower s1 &&
    upper s0 < upper s1

  -- | do two spaces intersect?
  intersects :: s -> s -> Bool
  intersects s0 s1 = not $ disjoint s0 s1
  -- | convex hull
  -- > a `contains` union a b && b `contains` union a b
  union :: s -> s -> s
  -- | null space, which can be interpreted as mempty
  nul :: s
  -- | is this a null space
  isNul :: s -> Bool

  eps ::
    ( Space s
    , Epsilon (Element s)
    , Subtractive (Element s)
    , Multiplicative (Element s))
    => Element s -> Element s -> s
  eps accuracy a = a +/- (accuracy * a * epsilon)

  whole ::
    ( Space s
    , BoundedField (Element s)
    ) => s
  whole = infinity ... negInfinity

  -- | lift a monotone function (increasing or decreasing) over a given interval
  monotone :: (Space a, Space b) => (Element a -> Element b) -> a -> b
  monotone f s = space [f (lower s), f (upper s)]

class (Space s, Field (Element s)) => FieldSpace s where
    -- | mid-point of the space
    mid :: s -> Element s
    mid s = (lower s + upper s)/(one+one)
    -- | project a data point from an old range to a new range
    project :: s -> s -> Element s -> Element s
    project s0 s1 p =
        ((p-lower s0)/(upper s0-lower s0)) * (upper s1-lower s1) + lower s1
    type Grid s :: *
    -- | create equally-spaced `a`s from a space
    grid :: Pos -> s -> Grid s -> [Element s]
    -- | create equally-spaced `Space a`s from a space
    gridSpace :: s -> Grid s -> [s]

-- | Pos suggests where points should be placed in forming a grid across a field space.
data Pos = OuterPos | InnerPos | LowerPos | UpperPos | MidPos deriving (Show, Generic, Eq)

data Interval a =
  Interval !a !a | SingletonInterval !a | EmptyInterval
  deriving ( Eq
           , Show
           , Read
           , Data
           , Generic
           , Generic1
           , Functor
           , Foldable
           , Traversable
           )

-- instance D.Distributive Interval where
--   distribute wc = fmap lower wc ... fmap upper wc where

instance (Subtractive a, Ord a) => Space (Interval a) where
  type Element (Interval a) = a

  lower (Interval l _) = l
  lower (SingletonInterval s) = s
  lower EmptyInterval = P.errorWithoutStackTrace "lower: empty space"

  upper (Interval _ u) = u
  upper (SingletonInterval s) = s
  upper EmptyInterval = P.errorWithoutStackTrace "upper: empty space"

  singleton = SingletonInterval
  nul = EmptyInterval
  isNul = (nul ==)

  union EmptyInterval b = b
  union a EmptyInterval = a
  union a b = Interval l u where
    l = lowest a [lower a, lower b]
    u = highest a [upper b, upper a]

  a +/- b = a - b ... a + b

  width s = upper s - lower s


{-
instance (Eq (f a), Functor f, Subtractive a, Ord a) => Space (Interval (f a)) where
  type Element (Interval (f a)) = f a

  lowest :: Interval (f a) -> g (f a) -> f a
  lowest _ xs = P.undefined

  highest _ xs = P.undefined

  below = P.undefined
  above = P.undefined
  disjoint = P.undefined
  contains = P.undefined
  element = P.undefined

-}

instance (Additive (Element s), Space s) => Magma (Sum s) where
  magma (Sum a) (Sum b)
    | isNul a = Sum b
    | isNul b = Sum a
    | otherwise =
      Sum $ (lower a + lower b) ... (upper a + upper b)

instance (Additive (Element s), Space s) =>
  Unital (Sum s) where
  unit = Sum $ singleton zero

instance (Additive (Element s), Space s) => Associative (Sum s)

instance (Additive (Element s), Space s) => Commutative (Sum s)

instance (Subtractive (Element s), Space s) => Invertible (Sum s) where
  inv (Sum a) = Sum $ negate (upper a) ... negate (lower a)

instance (Multiplicative (Element s), Space s) =>
  Magma (Product s) where
  magma (Product a) (Product b) =
    Product $ space
    [ lower a * lower b
    , lower a * upper b
    , upper a * lower b
    , upper a * upper b
    ]

instance (Multiplicative (Element s), Space s) =>
  Unital (Product s) where
  unit = Product $ singleton one

instance (Multiplicative (Element s), Space s) =>
  Absorbing (Product s) where
  absorb = Product $ singleton zero'

instance (Multiplicative (Element s), Space s) =>
  Commutative (Product s) where

instance (Multiplicative (Element s), Space s) =>
  Associative (Product s) where

instance
  ( Divisive (Element s)
  , Eq (Element s)
  , Epsilon (Element s)
  , BoundedField (Element s)
  , Space s) =>
  Invertible (Product s) where
  inv (Product a)
    | singleton zero `contains` a && not (singleton epsilon `contains` a) =
      Product (negInfinity ... recip (lower a))
    | singleton zero `contains` a && not (singleton (negate epsilon) `contains` a) =
      Product (infinity ... recip (lower a))
    | zero `element` a = Product whole
    | otherwise = Product (recip (lower a) ... recip (upper a))

instance (NH.Distributive (Element s), Space s) => NH.Distributive s

instance
  ( IntegralDomain (Element s)
  , Epsilon (Element s)
  , Space s
  , BoundedField (Element s)
  ) =>
  IntegralDomain s

instance
  ( Field (Element s)
  , Epsilon (Element s)
  , Space s
  , BoundedField (Element s)
  ) => Field s

instance
  ( Epsilon (Element s)
  , BoundedField (Element s)
  , Space s) =>
  UpperBoundedField s where
  isNaN a = isNaN (lower a) || isNaN (upper a)

instance
  ( Epsilon (Element s)
  , BoundedField (Element s)
  , Space s) =>
  LowerBoundedField s where

instance
  ( Epsilon (Element s)
  , BoundedField (Element s)
  , ExpField (Element s)
  , Space s) =>
  ExpField s where
  exp s = space [exp (lower s), exp (upper s)]
  log s = space [log (lower s), log (upper s)]

instance (Integral (Element s), Space s) => Integral s where
  divMod a b = (ld ... ud, lm ... um) where
    (ld, lm) = divMod (lower a) (lower b)
    (ud, um) = divMod (upper a) (upper b)

  quotRem a b = (ld ... ud, lm ... um) where
    (ld, lm) = quotRem (lower a) (lower b)
    (ud, um) = quotRem (upper a) (upper b)

instance (FromInteger (Element s), Space s) => FromInteger s where
  fromInteger a = singleton (fromInteger a)

instance (Additive (Element s), Subtractive (Element s), Multiplicative (Element s), Signed (Element s), Space s)
  => Signed s where
  sign a = space [sign (lower a), sign (upper a)]
  abs a
    | sign (lower a) == one = a
    | sign (upper a) == negate one = negate a
    | otherwise = space [zero, - lower a, upper a]

instance (a ~ Element s, Additive a, Subtractive a, Multiplicative a, Signed a, Metric a a, Space s)
  => Metric s a where
  distanceL1 a b = lower . abs $ (a - b)
  distanceL2 a b = lower . abs $ (a - b)
  distanceLp _ a b = lower . abs $ (a - b)

