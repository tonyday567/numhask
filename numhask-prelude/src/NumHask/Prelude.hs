{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

-- | A prelude for NumHask
module NumHask.Prelude
  ( -- * Backend
    -- $backend
    module Protolude
#if !MIN_VERSION_base(4,11,0)
  , (<>)
  , Semigroup
#endif
    -- RebindableSyntax removes all sorts of stuff that we then have to put back in
  , fromString
  , fail
  , ifThenElse
  , fromList
  , fromListN
    -- * Algebraic Heirarchy
    -- $instances
  , module NumHask.Algebra.Abstract.Action
  , module NumHask.Algebra.Abstract.Additive
  , module NumHask.Algebra.Abstract.Field
  , module NumHask.Algebra.Abstract.Group
  , module NumHask.Algebra.Abstract.Lattice
  , module NumHask.Algebra.Abstract.Module
  , module NumHask.Algebra.Abstract.Multiplicative
  , module NumHask.Algebra.Abstract.Ring
  , module NumHask.Algebra.Linear.Hadamard
  , module NumHask.Analysis.Metric
  , module NumHask.Data.Complex
  , module NumHask.Data.Integral
  , module NumHask.Data.LogField
  , module NumHask.Data.Rational
  , module NumHask.Data.Pair
  , module NumHask.Data.Positive
  , Natural(..)
  , module NumHask.Exception
  ) where
 
#if MIN_VERSION_base(4,11,0)
import Protolude hiding (Integral(..), Rep, Semiring(..), (*), (**), (+), (-), (/), (^), (^^), abs, acos, acosh, asin, asinh, atan, atan2, atanh, ceiling, cos, cosh, exp, floor, fromInteger, fromIntegral, even, odd, infinity, log, logBase, negate, pi, product, properFraction, recip, round, sin, sinh, sqrt, sum, tan, tanh, toInteger, trans, truncate, zero, fromRational, Ratio, reduce, gcd, subtract, Complex(..), Sum(..), Product(..), realPart, imagPart, polar, phase, mkPolar, magnitude, cis, toRational)
#else
import Protolude hiding (Integral(..), Rep, Semiring(..), (*), (**), (+), (-), (/), (^), (^^), abs, acos, acosh, asin, asinh, atan, atan2, atanh, ceiling, cos, cosh, exp, floor, fromInteger, fromIntegral, even, odd, infinity, log, logBase, negate, pi, product, properFraction, recip, round, sin, sinh, sqrt, sum, tan, tanh, toInteger, trans, truncate, zero, fromRational, Ratio(..), reduce, gcd, subtract, Complex(..), Sum(..), Product(..), realPart, imagPart, polar, phase, mkPolar, magnitude, cis, toRational, (<>), Semigroup)
import Data.Semigroup ((<>), Semigroup)
#endif

import Control.Monad (fail)
import Data.String
import GHC.Natural (Natural(..))
import NumHask.Algebra.Abstract.Action
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Lattice
import NumHask.Algebra.Abstract.Module
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Algebra.Linear.Hadamard
import NumHask.Analysis.Metric
import NumHask.Data.Complex
import NumHask.Data.Integral
import NumHask.Data.LogField
import NumHask.Data.Pair
import NumHask.Data.Positive
import NumHask.Data.Rational
import NumHask.Exception
import GHC.Exts

-- $backend
-- NumHask imports Protolude as the prelude and replaces much of the 'Num' heirarchy in base.
-- Usage of 'Semigroup' and 'Monoid' has been avoided to retain basic compatability.
-- $instances
-- Re-defines the numeric tower.
--
-- Instances for 'Int', 'Integer', 'Float', 'Double', 'Bool', 'Complex' and 'Natural'are supplied.
--

-- | rebindable syntax splats this, and I'm not sure where it exists in GHC land
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  x _ = x
ifThenElse False _ y = y

