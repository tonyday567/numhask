{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK prune #-}

-- | Numeric classes.
module NumHask
  ( -- * Usage
    -- $setup

    -- * Overview
    -- $overview
    -- $pictures

    -- * Prelude Mappings
    -- $mapping
    -- $backend

    -- * Extensions
    -- $extensions

    -- * Exports
    module NumHask.Algebra.Additive,
    module NumHask.Algebra.Field,
    module NumHask.Algebra.Group,
    module NumHask.Algebra.Lattice,
    module NumHask.Algebra.Module,
    module NumHask.Algebra.Multiplicative,
    module NumHask.Algebra.Ring,
    module NumHask.Analysis.Metric,
    module NumHask.Data.Complex,
    module NumHask.Data.Integral,
    module NumHask.Data.LogField,
    module NumHask.Data.Rational,
    module NumHask.Data.Positive,
    module NumHask.Exception,
  )
where

import NumHask.Algebra.Additive
import NumHask.Algebra.Field
import NumHask.Algebra.Group
import NumHask.Algebra.Lattice
import NumHask.Algebra.Module
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Ring
import NumHask.Analysis.Metric
import NumHask.Data.Complex
import NumHask.Data.Integral
import NumHask.Data.LogField
import NumHask.Data.Positive
import NumHask.Data.Rational
import NumHask.Exception

-- $setup
--
-- >>> :set -XRebindableSyntax
-- >>> :set -XNegativeLiterals
-- >>> import NumHask.Prelude
-- >>> 1+1
-- 2

-- $extensions
--
-- [RebindableSyntax](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rebindable_syntax.html) and [NegativeLiterals](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/negative_literals.html) are both recommended for use with numhask. [LexicalNegation](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/lexical_negation.html) also looks sweet when it arrives.
--
-- As a replacement for the numerical classes, numhask clashes significantly with an unqualified import of the @Prelude@. Either numhask modules should be qualified, or prelude turned off with the NoImplicitPrelude extension, or with RebindableSyntax, which implies NoImplicitPrelude.
--
-- == defaulting
--
-- Without RebindableSyntax, numeric literals default as follows:
--
-- >>> :set -XNoRebindableSyntax
-- >>> :t 1
-- 1 :: Num p => p
--
-- >>> :t 1.0
-- 1.0 :: Fractional p => p
--
-- With RebindableSyntax (which also switches NoImplicitPrelude on) literal numbers change to the numhask types, 'FromInteger' and 'FromRational':
--
-- >>> :set -XRebindableSyntax
-- >>> :t 1
-- 1 :: FromInteger a => a
--
-- >>> :t 1.0
-- 1.0 :: FromRational a => a
--
-- >>> 1
-- 1
--
-- >>> 1.0
-- 1.0
--
-- It is recommended to switch on RebindableSyntax to avoid Num constraints being introduced due to literal defaulting. The extension is a tradeoff, however, and usage comes attached with other non-numeric changes that "NumHask.Prelude" attempts to counteract.
--
-- See See [haskell2010 Section 4.3.4](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-750004.3) for the nuts and bolts to defaulting.
--
-- The effect of [ExtendedDefaultRules](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/ghci.html#extension-ExtendedDefaultRules) in ghci or switched on as an extension also need to be understood. It can lead to unusual interactions with numerics and strange error messages at times because it adds @()@ and @[]@ to the start of the type defaulting list.
--
-- == Negatives
--
-- Without [NegativeLiterals](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/negative_literals.html), GHC and Haskell often reads a negative as subtraction rather than a minus.
--
-- > :set -XNoNegativeLiterals
-- > :t Point 1 -2
-- Point 1 -2
--   :: (Subtractive (Point a), FromInteger a,
--       FromInteger (a -> Point a)) =>
--      a -> Pair a
-- ...
--
-- > :set -XNegativeLiterals
-- > :t Point 1 -2
-- Point 1 -2 :: FromInteger a => Point a
--
-- > Point 1 -2
-- Point 1 -2
--
-- [LexicalNegation](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/lexical_negation.html) is coming soon as a valid replacement for NegativeLiterals and will tighten things up further.

-- $overview
-- numhask is largely a set of classes that can replace the 'GHC.Num.Num' class and it's descendents. Principles that have guided design include:
--
-- - __/balanced class density/__. The numeric heirarchy begins with addition and multiplication, choosing not to build from a 'Magma' base. Whilst not being as principled as other approaches, this circumvents the instance explosion problems of Haskell whilst maintaining clarity of class purpose.
--
-- - __/operator-first/__. In most cases, a class exists to define useful operators. The exceptions are 'Distributive', 'Ring' and 'Field', which are collections of operators representing major teleological fault lines.
--
-- - __/lawful/__. Most classes have laws associated with them that serve to relate class operators together in a meaningful way.
--
-- - __/low-impact/__. The library attempts to fit in with the rest of the Haskell ecosystem. It provides instances for common numbers: 'GHC.Num.Int', 'GHC.Num.Integer', 'GHC.Float.Double', 'GHC.Float.Float' and the Word classes. It avoids name (or idea) clashes with other popular libraries and adopts conventions in the <https://hackage.haskell.org/package/base/docs/Prelude.html current prelude> where they make sense.
--
-- - __/proof-of-concept/__. The library may be below industrial-strength depending on a definition of this term. At the same time, correspondence around improving the library is most welcome.

-- $pictures
--
-- The class heirarchy looks somewhat like this:
-- ![classes](other/nh.svg)
--
-- If the base started with magma, and the library tolerated clashing with 'Data.Semigroup' and 'Data.Monoid' in base, it would look like:
--
-- ![magma classes](other/nhmagma.svg)
--
-- These first two levels, contained in 'NumHask.Algebra.Group' can be considered "morally" super-classes.

-- $mapping
--
-- 'GHC.Num' is a very old part of haskell, and is virtually unchanged since it's specification in [haskell98](https://www.haskell.org/onlinereport/standard-prelude.html).
--
-- A deconstruction of 'GHC.Num.Num' and mapping to numhask.
--
-- > -- | Basic numeric class.
-- > class  Num a  where
-- >    {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
-- >
-- >    (+), (-), (*)       :: a -> a -> a
-- >    -- | Unary negation.
-- >    negate              :: a -> a
--
-- '(+)' is an operator of the 'Additive' class
--
-- '(-)' & 'negate' are functions in the 'Subtractive' class, and
--
-- '(*)' is an operator of the 'Multiplicative' class.
--
-- 'zero' and 'one' are also introduced to the numeric heirarchy.
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
-- 'abs' is a function in the 'NumHask.Analysis.Metric.Signed' class. The concept of an absolute value can also include situations where the domain and codomain are different, and 'norm' as a function in the 'NumHask.Analysis.Metric.Norm' class is supplied for these cases.
--
--  'NumHask.Analysis.Metric.sign' replaces 'GHC.Num.signum', because signum is simply a naming crime. 'NumHask.Analysis.Metric.basis' can also be seen as a generalisation of sign.
--
-- >    -- | Conversion from an 'Integer'.
-- >    -- An integer literal represents the application of the function
-- >    -- 'fromInteger' to the appropriate value of type 'Integer',
-- >    -- so such literals have type @('Num' a) => a@.
-- >    fromInteger         :: Integer -> a
--
-- 'FromInteger' becomes its own class and 'FromIntegral' is introduced to polymorphise the covariant.
--
-- Mappings from other areas of prelude include:\
--
-- 'GHC.Real.Integral' becomes 'Integral' and a polymorphic 'ToIntegral' is introduced.
--
-- 'GHC.Real.Fractional' is roughly synonymous to 'Field' together with a polymorphic 'FromRatio'.
--
-- 'GHC.Real.RealFrac' becomes the polymorphic 'QuotientField'
--
-- 'GHC.Float.Floating' is split into 'ExpField' and 'TrigField'
--
-- 'GHC.Float.RealFloat' is not attempted. Life is too short.

-- $backend
-- NumHask imports [protolude](https://hackage.haskell.org/package/protolude) as a base prelude with some minor tweaks.
