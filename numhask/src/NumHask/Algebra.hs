{-# OPTIONS_GHC -Wall #-}

-- | Algebraic structure
module NumHask.Algebra
  ( -- $setup
    module NumHask.Algebra.Additive
  , module NumHask.Algebra.Basis
  , module NumHask.Algebra.Distribution
  , module NumHask.Algebra.Field
  , module NumHask.Algebra.Integral
  , module NumHask.Algebra.Magma
  , module NumHask.Algebra.Metric
  , module NumHask.Algebra.Module
  , module NumHask.Algebra.Multiplicative
  , module NumHask.Algebra.Ring
  , Complex(..)
  ) where

import Data.Complex (Complex(..))
import NumHask.Algebra.Additive
import NumHask.Algebra.Basis
import NumHask.Algebra.Distribution
import NumHask.Algebra.Field
import NumHask.Algebra.Integral
import NumHask.Algebra.Magma
import NumHask.Algebra.Metric
import NumHask.Algebra.Module
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring

-- $setup
-- >>> import NumHask.Algebra
-- >>> import Prelude (Bool(..), Double, Float, Int, Integer)
-- >>> import qualified Prelude as P
-- >>> import Prelude hiding (Bounded(..), Integral(..), (*), (**), (+), (-), (/), (^), (^^), abs, acos, acosh, asin, asinh, atan, atan2, atanh, ceiling, cos, cosh, exp, floor, fromInteger, fromIntegral, isNaN, log, logBase, negate, pi, product, recip, round, sin, sinh, sqrt, sum, tan, tanh, toInteger)
