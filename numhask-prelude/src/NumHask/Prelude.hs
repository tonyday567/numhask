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
    -- RebindableSyntax takes fromString and fail away so we need to put it back in
  , fromString
  , fail
    -- * Algebraic Heirarchy
    -- $instances
  , module NumHask.Algebra.Abstract.Additive
  , module NumHask.Algebra.Abstract.Field
  , module NumHask.Algebra.Abstract.Group
  , module NumHask.Algebra.Abstract.Homomorphism
  , module NumHask.Algebra.Abstract.Module
  , module NumHask.Algebra.Abstract.Multiplicative
  , module NumHask.Algebra.Abstract.Ring
  , module NumHask.Algebra.Linear.Hadamard
  , module NumHask.Analysis.Banach
  , module NumHask.Analysis.Metric
  , module NumHask.Data.Complex   
  , module NumHask.Data.Integral
  , module NumHask.Data.Interval
  , module NumHask.Data.LogField       
  , module NumHask.Data.Rational
  , Natural(..)
  ) where
 
#if MIN_VERSION_base(4,11,0)
import Protolude hiding (Integral(..), Rep, Semiring(..), (*), (**), (+), (-), (/), (^), (^^), abs, acos, acosh, asin, asinh, atan, atan2, atanh, ceiling, cos, cosh, exp, floor, fromInteger, fromIntegral, even, odd, infinity, log, logBase, negate, pi, product, properFraction, recip, round, sin, sinh, sqrt, sum, tan, tanh, toInteger, trans, truncate, zero, fromRational, Ratio(..), Rational, reduce, gcd, subtract, Complex(..), Sum(..), Product(..), isNaN, realPart, imagPart, polar, phase, mkPolar, magnitude, cis)
#else
import Protolude hiding (Integral(..), Rep, Semiring(..), (*), (**), (+), (-), (/), (^), (^^), abs, acos, acosh, asin, asinh, atan, atan2, atanh, ceiling, cos, cosh, exp, floor, fromInteger, fromIntegral, even, odd, infinity, log, logBase, negate, pi, product, properFraction, recip, round, sin, sinh, sqrt, sum, tan, tanh, toInteger, trans, truncate, zero, fromRational, Ratio(..), Rational, reduce, gcd, subtract, Complex(..), Sum(..), Product(..), isNaN, realPart, imagPart, polar, phase, mkPolar, magnitude, cis, (<>), Semigroup)
import Data.Semigroup ((<>), Semigroup)
#endif

import Control.Monad (fail)
import Data.String
import GHC.Natural (Natural(..))
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Homomorphism
import NumHask.Algebra.Abstract.Module
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Algebra.Linear.Hadamard
import NumHask.Analysis.Banach
import NumHask.Analysis.Metric
import NumHask.Data.Complex   
import NumHask.Data.Integral
import NumHask.Data.Interval
import NumHask.Data.LogField       
import NumHask.Data.Rational

-- $backend
-- NumHask imports Protolude as the prelude and replaces much of the 'Num' heirarchy in base.
-- Usage of 'Semigroup' and 'Monoid' has been avoided to retain basic compatability.
-- $instances
-- Re-defines the numeric tower.
--
-- Instances for 'Int', 'Integer', 'Float', 'Double', 'Bool', 'Complex' and 'Natural'are supplied.
--
