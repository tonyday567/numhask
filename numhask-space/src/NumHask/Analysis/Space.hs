{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- https://en.wikipedia.org/wiki/Interval_(mathematics)
module NumHask.Analysis.Space
  ( Space(..)
  , Spaceable
  , Union(..)
  , Intersection(..)
  , FieldSpace(..)
  , mid
  , project
  , Pos(..)
  , space1
  , (|.|)
  , memberOf
  , contains
  , disjoint
  , (|>|)
  , (|<|)
  , width
  , (+/-)
  , monotone
  , whole
  , negWhole
  , eps
  , widen
  , widenEps
  )

where

import Data.Bool
import NumHask.Algebra.Abstract
import NumHask.Analysis.Metric
import Prelude (Functor(..), Eq(..), Bool(..), Show, foldr1, Traversable(..), (.), Semigroup(..), Monoid(..))

type Spaceable a = (Eq a, JoinSemiLattice a, MeetSemiLattice a)

-- | a continuous set of numbers
-- mathematics does not define a space, so library devs are free to experiment.
--
-- > a `contains` union a b && b `contains` union a b
-- > lower a \/ upper a == lower a
-- > lower a /\ upper a == upper a
--
class (Spaceable (Element s)) => Space s where

  -- | the underlying element in the space
  type Element s :: *

  -- | lower boundary
  lower :: s -> Element s

  -- | upper boundary
  upper :: s -> Element s

  -- | space containing a single element
  singleton :: Element s -> s
  singleton s = s >.< s

  -- | the intersection of two spaces
  intersection :: s -> s -> s
  intersection a b = l >.< u where
      l = lower a /\ lower b
      u = upper a \/ upper b

  -- | the union of two spaces
  union :: s -> s -> s
  union a b = l >.< u where
    l = lower a \/ lower b
    u = upper a /\ upper b

  -- | Normalise a space so that
  -- > lower a \/ upper a == lower a
  -- > lower a /\ upper a == upper a
  norm :: s -> s
  norm s = lower s ... upper s

  -- | create a normalised space from two elements
  infix 3 ...
  (...) :: Element s -> Element s -> s
  (...) a b = (a\/b) >.< (a/\b)

  -- | create a space from two elements witjout normalising
  infix 3 >.<
  (>.<) :: Element s -> Element s -> s

newtype Union a = Union { getUnion :: a }

instance (Space a) => Semigroup (Union a) where
  (<>) (Union a) (Union b) = Union (a `union` b)

instance (BoundedJoinSemiLattice a, Space a) => Monoid (Union a) where
  mempty = Union bottom

newtype Intersection a = Intersection { getIntersection :: a }

instance (Space a) => Semigroup (Intersection a) where
  (<>) (Intersection a) (Intersection b) = Intersection (a `union` b)

instance (BoundedMeetSemiLattice a, Space a) => Monoid (Intersection a) where
  mempty = Intersection top

-- | a space that can be divided neatly
--
class (Space s, Subtractive (Element s), Field (Element s)) => FieldSpace s where
  type Grid s :: *

  -- | create equally-spaced elements across a space
  grid :: Pos -> s -> Grid s -> [Element s]

  -- | create equally-spaced spaces from a space
  gridSpace :: s -> Grid s -> [s]

-- | Pos suggests where points should be placed in forming a grid across a field space.
data Pos = OuterPos | InnerPos | LowerPos | UpperPos | MidPos deriving (Show, Eq)

-- | mid-point of the space
mid :: (Space s, Field (Element s)) => s -> Element s
mid s = (lower s + upper s)/two

-- | project a data point from one space to another, preserving relative position
--
-- > project o n (lower o) = lower n
-- > project o n (upper o) = upper n
-- > project a a x = x
--
project :: (Space s, Field (Element s), Subtractive (Element s)) => s -> s -> Element s -> Element s
project s0 s1 p =
  ((p-lower s0)/(upper s0-lower s0)) * (upper s1-lower s1) + lower s1

-- | the containing space of a non-empty Foldable
space1 :: (Space s, Traversable f) => f (Element s) -> s
space1 = foldr1 union . fmap singleton

-- | is an element in the space
infixl 7 |.|
(|.|) :: (Space s) => Element s -> s -> Bool
(|.|) a s = (a `joinLeq` lower s) && (upper s `meetLeq` a)

memberOf :: (Space s) => Element s -> s -> Bool
memberOf = (|.|)

-- | distance between boundaries
width :: (Space s, Subtractive (Element s)) => s -> Element s
width s = upper s - lower s

-- | create a space centered on a plus or minus b
infixl 6 +/-
(+/-) :: (Space s, Subtractive (Element s)) => Element s -> Element s -> s
a +/- b = a - b ... a + b

-- | is a space contained within another?
contains :: (Space s) => s -> s -> Bool
contains s0 s1 =
  lower s1 |.| s0 &&
  upper s1 |.| s0

-- | are two spaces disjoint?
disjoint :: (Space s) => s -> s -> Bool
disjoint s0 s1 = s0 |>| s1 || s0 |<| s1

-- | is one space completely above the other
infixl 7 |>|
(|>|) :: (Space s) => s -> s -> Bool
(|>|) s0 s1 =
  lower s0 `joinLeq` upper s1

-- | is one space completely below the other
infixl 7 |<|
(|<|) :: (Space s) => s -> s -> Bool
(|<|) s0 s1 =
  lower s1 `meetLeq` upper s0

-- | lift a monotone function (increasing or decreasing) over a given space
monotone :: (Space a, Space b) => (Element a -> Element b) -> a -> b
monotone f s = space1 [f (lower s), f (upper s)]

-- | a big, big space
whole ::
  ( Space s
  , BoundedJoinSemiLattice (Element s)
  , BoundedMeetSemiLattice (Element s)
  ) => s
whole = bottom ... top

-- | a negative space
negWhole ::
  ( Space s
  , BoundedJoinSemiLattice (Element s)
  , BoundedMeetSemiLattice (Element s)
  ) => s
negWhole = top >.< bottom

-- | a small space
eps ::
    ( Space s
    , Epsilon (Element s)
    , Multiplicative (Element s)
    )
    => Element s -> Element s -> s
eps accuracy a = a +/- (accuracy * a * epsilon)

-- | widen a space
widen ::
    ( Space s
    , Subtractive (Element s))
    => Element s -> s -> s
widen a s = (lower s - a) >.< (upper s + a)

-- | widen by a small amount
widenEps ::
    ( Space s
    , Epsilon (Element s)
    , Multiplicative (Element s))
    => Element s -> s -> s
widenEps accuracy = widen (accuracy * epsilon)
