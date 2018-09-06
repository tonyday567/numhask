{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module NumHask.Analysis.Space
  ( Space(..)
  , FieldSpace(..)
  , Pos(..)
  , monotone
  , whole
  , eps
  , widen
  , widenEps
  )

where

import Data.Bool
import NumHask.Algebra.Abstract
import NumHask.Analysis.Metric
import Prelude (Functor(..), Eq(..), Bool(..), Show, foldr1, Traversable(..), (.))

-- | a non-empty space
-- mathematics does not define a space, so library devs are free to experiment.
-- a space here is a continuous set of numbers
--
-- > a `contains` union a b && b `contains` union a b
--
class (Eq (Element s), Lattice (Element s), Subtractive (Element s)) => Space s where

  -- | the underlying element in the space
  type Element s :: *

  -- | the containing space of a Foldable
  space :: (Traversable f) => f (Element s) -> s
  space = foldr1 union . fmap singleton

  -- | create a space between two elements
  infix 3 ...
  (...) :: Element s -> Element s -> s
  (...) a b = space [a,b]

  -- | create a space centered on a plus or minus b
  infixl 6 +/-
  (+/-) :: Element s -> Element s -> s
  a +/- b = a - b ... a + b

  -- | is an element in the space
  infixl 7 |.|
  (|.|) :: Element s -> s -> Bool
  (|.|) a s = (a `joinLeq` lower s) && (upper s `meetLeq` a)

  -- | lower boundary
  lower :: s -> Element s

  -- | upper boundary
  upper :: s -> Element s

  -- | distance between boundaries
  width :: s -> Element s
  width s = upper s - lower s

  -- | singleton space
  singleton :: Element s -> s

  -- | zero-width test
  singular :: s -> Bool
  singular s = lower s == upper s

  -- | is a space contained within another?
  contains :: s -> s -> Bool
  contains s0 s1 =
    lower s1 |.| s0 &&
    upper s1 |.| s0

  -- | are two spaces disjoint?
  disjoint :: s -> s -> Bool
  disjoint s0 s1 = s0 |>| s1 || s0 |<| s1

  -- | is one space completely above the other
  infixl 7 |>|
  (|>|) :: s -> s -> Bool
  (|>|) s0 s1 =
    lower s0 `joinLeq` upper s1

  -- | is one space completely below the other
  infixl 7 |<|
  (|<|) :: s -> s -> Bool
  (|<|) s0 s1 =
    lower s1 `meetLeq` upper s0

  -- | the intersection of two spaces
  intersection :: s -> s -> s

  -- | the union of two spaces
  union :: s -> s -> s

-- | a space that can be divided neatly
--
-- > project o n (lower o) = lower n
-- > project o n (upper o) = upper n
-- > project a a x = x
--
class (Space s, Subtractive (Element s), Field (Element s)) => FieldSpace s where
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
data Pos = OuterPos | InnerPos | LowerPos | UpperPos | MidPos deriving (Show, Eq)

-- | lift a monotone function (increasing or decreasing) over a given space
monotone :: (Space a, Space b) => (Element a -> Element b) -> a -> b
monotone f s = space [f (lower s), f (upper s)]

-- | a big space for Fields
whole ::
  ( Space s
  , LowerBoundedField (Element s)
  , UpperBoundedField (Element s)
  ) => s
whole = infinity ... negInfinity

-- | a small space
eps ::
    ( Space s
    , Epsilon (Element s)
    , Multiplicative (Element s))
    => Element s -> Element s -> s
eps accuracy a = a +/- (accuracy * a * epsilon)

-- | widen a space
widen ::
    ( Space s )
    => Element s -> s -> s
widen a s = (lower s +/- a) `union` (upper s +/- a)

-- | widen by a small amount
widenEps ::
    ( Space s
    , Epsilon (Element s)
    , Multiplicative (Element s))
    => Element s -> s -> s
widenEps accuracy = widen (accuracy * epsilon)

