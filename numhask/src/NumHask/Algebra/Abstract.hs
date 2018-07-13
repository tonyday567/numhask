{-# OPTIONS_GHC -Wall #-}

-- | The abstract algebraic class structure of a number.
--
module NumHask.Algebra.Abstract
  ( -- * Mapping from Num
    --
    -- $numMap
    module NumHask.Algebra.Abstract.Group
  , module NumHask.Algebra.Abstract.Additive
  , module NumHask.Algebra.Abstract.Multiplicative
  , module NumHask.Algebra.Abstract.Ring
  , module NumHask.Algebra.Abstract.Field
  , module NumHask.Algebra.Abstract.Module
  )
where

import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Algebra.Abstract.Field
import NumHask.Algebra.Abstract.Module

-- $numMap
--
-- `Num` is a very old part of haskell, and a lot of different numeric concepts are tossed in there. The closest analogue in numhask is the `Ring` class, which magmaines the classical `+`, `-` and `*`, together with the Distributive  laws.
--
-- ![ring example](other/ring.svg)
--
-- No attempt is made, however, to reconstruct the particular magmaination of laws and classes that represent the old `Num`.  A rough mapping of `Num` to numhask classes follows:
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
