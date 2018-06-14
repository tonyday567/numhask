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
    module NumHask.Algebra.Abstract.Group
  , module NumHask.Algebra.Linear.Hamard
  , module NumHask.Algebra.Abstract.Distribution
  , module NumHask.Algebra.Abstract.Field
  , module NumHask.Algebra.Abstract.Integral
  , module NumHask.Algebra.Abstract.Group
  , module NumHask.Analysis.Metric
  , module NumHask.Analysis.Banach
  , module NumHask.Algebra.Abstract.Module
  , module NumHask.Algebra.Abstract.Multiplicative
  , module NumHask.Data.Rational
  , module NumHask.Algebra.Abstract.Ring
  , module NumHask.Data.Complex
  ) where

import NumHask.Algebra.Linear.Hamard
import NumHask.Algebra.Abstract.Distribution
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Integral
import NumHask.Algebra.Abstract.Group
import NumHask.Analysis.Metric
import NumHask.Analysis.Banach
import NumHask.Algebra.Abstract.Module
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Data.Rational
import NumHask.Algebra.Abstract.Ring
import NumHask.Data.Complex (Complex(..))

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


