{-# OPTIONS_GHC -Wall #-}

-- | The basic algebraic class structure of a number.
--
-- > import NumHask.Algebra
-- > import Prelude hiding (Integral(..), (*), (**), (+), (-), (/), (^), (^^), abs, acos, acosh, asin, asinh, atan, atan2, atanh, ceiling, cos, cosh, exp, floor, fromInteger, fromIntegral, log, logBase, negate, pi, product, recip, round, sin, sinh, sqrt, sum, tan, tanh, toInteger, fromRational)
--
module NumHask.Algebra
  ( -- * Mapping from Num
    --
    -- $numMap
    module NumHask.Algebra.Group
  , module NumHask.Algebra.Basis
  , module NumHask.Algebra.Distribution
  , module NumHask.Algebra.Field
  , module NumHask.Algebra.Integral
  , module NumHask.Algebra.Group
  , module NumHask.Algebra.Metric
  , module NumHask.Algebra.Module
  , module NumHask.Algebra.Multiplicative
  , module NumHask.Algebra.Rational
  , module NumHask.Algebra.Ring
  , module NumHask.Data.Complex
  ) where

import NumHask.Algebra.Group
import NumHask.Data.Complex (Complex(..))
import NumHask.Algebra.Basis
import NumHask.Algebra.Distribution
import NumHask.Algebra.Field
import NumHask.Algebra.Integral
import NumHask.Algebra.Metric
import NumHask.Algebra.Module
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Rational
import NumHask.Algebra.Ring

-- $numMap
--
-- `Num` is a very old part of haskell, and a lot of different numeric concepts are tossed in there. The closest analogue in numhask is the `Ring` class, which combines the classical `+`, `-` and `*`, together with the distribution laws.
--
-- ![ring example](other/ring.svg)
--
-- No attempt is made, however, to reconstruct the particular combination of laws and classes that represent the old `Num`.  A rough mapping of `Num` to numhask classes follows:
--
-- > -- | Basic numeric class.
-- > class  Num a  where
-- >    {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
-- >
-- >    (+), (-), (*)       :: a -> a -> a
-- >    -- | Unary negation.
-- >    negate              :: a -> a
-- 
-- `+` is a function of the `Additive` class, 
-- `-` is a function of the `AdditiveGroup` class, and
-- `*` is a function of the `Multiplicative` class.
-- `negate` is specifically in the `AdditiveInvertible` class.  There are many useful constructions between negate and (-), involving cancellative properties.
--
-- >    -- | Absolute value.
-- >    abs                 :: a -> a
-- >    -- | Sign of a number.
-- >    -- The functions 'abs' and 'signum' should satisfy the law:
-- >    --
-- >    -- > abs x * signum x == x
-- >    --
-- >    -- For real numbers, the 'signum' is either @-1@ (negative), @0@ (zero)
-- >    -- or @1@ (positive).
-- >    signum              :: a -> a
--
-- `abs` is a function in the `Signed` class.  The concept of an absolute value of a number can include situations where the domain and codomain are different, and `size` as a function in the `Normed` class is supplied for these cases.
--
--  `sign` replaces `signum`, because signum is a heinous name.
--
-- >    -- | Conversion from an 'Integer'.
-- >    -- An integer literal represents the application of the function
-- >    -- 'fromInteger' to the appropriate value of type 'Integer',
-- >    -- so such literals have type @('Num' a) => a@.
-- >    fromInteger         :: Integer -> a
--
-- `fromInteger` is given its own class `FromInteger`
--


