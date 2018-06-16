{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, GeneralizedNewtypeDeriving, DeriveFoldable, DeriveTraversable, GADTs #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses #-}
module NumHask.Data.LogField 
    (
    -- * @LogField@
    LogField()
    -- ** Isomorphism to normal-domain
    , logField
    , fromLogField
    -- ** Isomorphism to log-domain
    , logToLogField
    , logFromLogField
    -- ** Additional operations
    , accurateSum, accurateProduct
    , pow
    )where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Data.Data                      ( Data )

import           NumHask.Algebra.Abstract.Addition
import           NumHask.Algebra.Abstract.Group
import           NumHask.Algebra.Abstract.Multiplication
import           NumHask.Algebra.Abstract.Field
import           NumHask.Algebra.Abstract.Ring
import           NumHask.Algebra.Integral
import           NumHask.Data.Rational
import           NumHask.Analysis.Metric

import           Prelude                 hiding ( Num(..)
                                                , negate
                                                , sin
                                                , cos
                                                , sqrt
                                                , (/)
                                                , atan
                                                , pi
                                                , exp
                                                , log
                                                , recip
                                                , (**)
                                                , toInteger
                                                , Semigroup
                                                )
import qualified Data.Foldable                 as F

-- LogField is adapted from LogFloat
----------------------------------------------------------------
--                                                  ~ 2015.08.06
-- |
-- Module      :  Data.Number.LogFloat
-- Copyright   :  Copyright (c) 2007--2015 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  stable
-- Portability :  portable (with CPP, FFI)
-- Link        :  https://hackage.haskell.org/package/logfloat
----------------------------------------------------------------

----------------------------------------------------------------
--
-- | A @LogField@ is just a 'Field' with a special interpretation.
-- The 'LogField' function is presented instead of the constructor,
-- in order to ensure semantic conversion. At present the 'Show'
-- instance will convert back to the normal-domain, and hence will
-- underflow at that point. This behavior may change in the future.
--
-- Because 'logField' performs the semantic conversion, we can use
-- operators which say what we *mean* rather than saying what we're
-- actually doing to the underlying representation. That is,
-- equivalences like the following are true[1] thanks to type-class
-- overloading:
--
-- > logField (p + q) == logField p + logField q
-- > logField (p * q) == logField p * logField q
--
--
-- Performing operations in the log-domain is cheap, prevents
-- underflow, and is otherwise very nice for dealing with miniscule
-- probabilities. However, crossing into and out of the log-domain
-- is expensive and should be avoided as much as possible. In
-- particular, if you're doing a series of multiplications as in
-- @lp * LogField q * LogField r@ it's faster to do @lp * LogField
-- (q * r)@ if you're reasonably sure the normal-domain multiplication
-- won't underflow; because that way you enter the log-domain only
-- once, instead of twice. Also note that, for precision, if you're
-- doing more than a few multiplications in the log-domain, you
-- should use 'product' rather than using '(*)' repeatedly.
--
-- Even more particularly, you should /avoid addition/ whenever
-- possible. Addition is provided because sometimes we need it, and
-- the proper implementation is not immediately apparent. However,
-- between two @LogField@s addition requires crossing the exp\/log
-- boundary twice; with a @LogField@ and a 'Double' it's three
-- times, since the regular number needs to enter the log-domain
-- first. This makes addition incredibly slow. Again, if you can
-- parenthesize to do normal-domain operations first, do it!
--
-- [1] That is, true up-to underflow and floating point fuzziness.
-- Which is, of course, the whole point of this module.
newtype LogField a = LogField a
      deriving (Eq, Ord, Read, Data, Generic, Generic1, Functor, Foldable, Traversable)

----------------------------------------------------------------
-- To show it, we want to show the normal-domain value rather than
-- the log-domain value. Also, if someone managed to break our
-- invariants (e.g. by passing in a negative and noone's pulled on
-- the thunk yet) then we want to crash before printing the
-- constructor, rather than after.  N.B. This means the show will
-- underflow\/overflow in the same places as normal doubles since
-- we underflow at the @exp@. Perhaps this means we should show the
-- log-domain value instead.

instance (ExpField a, Show a) => Show (LogField a) where
    showsPrec p (LogField x) =
        let y = exp x in y `seq`
        showParen (p > 9)
            ( showString "LogField "
            . showsPrec 11 y
            )

----------------------------------------------------------------
-- | Constructor which does semantic conversion from normal-domain
-- to log-domain. Throws errors on negative and NaN inputs. If @p@
-- is non-negative, then following equivalence holds:
--
-- > logField p == logToLogField (log p)
logField :: (ExpField a) => a -> LogField a
{-# INLINE [0] logField #-}
logField = LogField . log


-- TODO: figure out what to do here, removed guards
-- | Constructor which assumes the argument is already in the
-- log-domain.
logToLogField :: a -> LogField a
logToLogField = LogField


-- | Semantically convert our log-domain value back into the
-- normal-domain. Beware of overflow\/underflow. The following
-- equivalence holds (without qualification):
--
-- > fromLogField == exp . logFromLogField
--
fromLogField :: ExpField a => LogField a -> a
{-# INLINE [0] fromLogField #-}
fromLogField (LogField x) = exp x


-- | Return the log-domain value itself without conversion.
logFromLogField :: LogField a -> a
logFromLogField (LogField x) = x


-- These are our module-specific versions of "log\/exp" and "exp\/log";
-- They do the same things but also have a @LogField@ in between
-- the logarithm and exponentiation. In order to ensure these rules
-- fire, we have to delay the inlining on two of the four
-- con-\/destructors.

{-# RULES
-- Out of log-domain and back in
"log/fromLogField"       forall x. log (fromLogField x) = logFromLogField x
-- TODO: Rewrite-rule too complicated
"LogField/fromLogField"  forall x. LogField (fromLogField x) = x

-- Into log-domain and back out
"fromLogField/LogField"  forall x. fromLogField (LogField x) = x
    #-}


log1p :: ExpField a => a -> a
{-# INLINE [0] log1p #-}
log1p x = log (one + x)

expm1 :: ExpField a => a -> a
{-# INLINE [0] expm1 #-}
expm1 x = exp x - one

{-# RULES
-- Into log-domain and back out
"expm1/log1p"    forall x. expm1 (log1p x) = x

-- Out of log-domain and back in
"log1p/expm1"    forall x. log1p (expm1 x) = x
    #-}

instance (ExpField a, LowerBoundedField a, Ord a) => Magma (Add (LogField a)) where
    (Add x@(LogField x')) `comb` (Add y@(LogField y'))
        | x == zero && y == zero = Add zero
        | x == zero     = Add y
        | y == zero     = Add x
        | x >= y          = Add $ LogField (x' + log1p (exp (y' - x')))
        | otherwise       = Add $ LogField (y' + log1p (exp (x' - y')))

instance (LowerBoundedField a, ExpField a, Ord a) => Unital (Add (LogField a)) where
    unit = Add $ LogField negInfinity

instance (LowerBoundedField a, ExpField a, Ord a) => Semigroup (Add (LogField a))

instance (LowerBoundedField a, ExpField a, Ord a) => Commutative (Add (LogField a))

instance (Magma (Add a), LowerBoundedField a, Eq a) => Magma (Mult (LogField a)) where
    (Mult (LogField x)) `comb ` (Mult (LogField y))
        | x == negInfinity || y == negInfinity  = Mult $ LogField negInfinity
        | otherwise                             = Mult $ LogField (x `plus` y)

instance (Unital (Add a), LowerBoundedField a, Eq a) => Unital (Mult (LogField a)) where
    unit = Mult $ LogField zero

instance (Semigroup (Add a), LowerBoundedField a, Eq a) => Semigroup (Mult (LogField a))

instance (Commutative (Add a), LowerBoundedField a, Eq a) => Commutative (Mult (LogField a))

instance (Invertible (Add a), LowerBoundedField a, Eq a) => Invertible (Mult (LogField a)) where
    inv (Mult (LogField x)) = Mult $ LogField $ neg x

instance (Multiplication (LogField a), Group (Add a), LowerBoundedField a, Eq a) => Group (Mult (LogField a))

instance (Magma (Add a), LowerBoundedField a, Eq a) => Absorbing (Mult (LogField a)) where
    absorb = Mult $ LogField negInfinity

instance (LowerBoundedField a, ExpField a, Ord a) => Distribution (LogField a)

-- unable to provide this instance because there is no Field (LogField a) instance
-- instance (Field (LogField a), ExpField a, LowerBoundedField a, Ord a) => ExpField (LogField a) where
--     exp (LogField x) = (LogField $ exp x)
--     log (LogField x) = (LogField $ log x)
--     (**) x (LogField y) = pow x $ exp y

instance (FromInteger a, ExpField a) => FromInteger (LogField a) where
    fromInteger = logField . fromInteger

instance (ToInteger a, ExpField a) => ToInteger (LogField a) where
    toInteger = toInteger . fromLogField

instance (FromRatio a, ExpField a) => FromRatio (LogField a) where
    fromRatio = logField . fromRatio

instance (ToRatio a, ExpField a) => ToRatio (LogField a) where
    toRatio = toRatio . fromLogField

instance (Epsilon a, ExpField a, LowerBoundedField a, Ord a) => Epsilon (LogField a) where
    nearZero (LogField x) = nearZero $ exp x
    aboutEqual (LogField x) (LogField y) = aboutEqual (exp x) (exp y) 


----------------------------------------------------------------
-- | /O(1)/. Compute powers in the log-domain; that is, the following
-- equivalence holds (modulo underflow and all that):
--
-- > LogField (p ** m) == LogField p `pow` m
--
-- /Since: 0.13/
pow :: (ExpField a, LowerBoundedField a, Ord a) => LogField a -> a -> LogField a
{-# INLINE pow #-}
infixr 8 `pow`
pow x@(LogField x') m 
    | x == zero && m == zero = LogField zero
    | x == zero              = x
    | otherwise              = LogField $ m * x'


-- Some good test cases:
-- for @logsumexp == log . accurateSum . map exp@:
--     logsumexp[0,1,0] should be about 1.55
-- for correctness of avoiding underflow:
--     logsumexp[1000,1001,1000]   ~~ 1001.55 ==  1000 + 1.55
--     logsumexp[-1000,-999,-1000] ~~ -998.45 == -1000 + 1.55
--
-- | /O(n)/. Compute the sum of a finite list of 'LogField's, being
-- careful to avoid underflow issues. That is, the following
-- equivalence holds (modulo underflow and all that):
--
-- > LogField . accurateSum == accurateSum . map LogField
--
-- /N.B./, this function requires two passes over the input. Thus,
-- it is not amenable to list fusion, and hence will use a lot of
-- memory when summing long lists.
{-# INLINE accurateSum #-}
accurateSum :: (ExpField a, Foldable f, Ord a) => f (LogField a) -> LogField a
accurateSum xs = LogField (theMax + log theSum)
  where
    LogField theMax = maximum xs

    -- compute @\log \sum_{x \in xs} \exp(x - theMax)@
    theSum = F.foldl' (\acc (LogField x) -> acc + exp (x - theMax)) zero xs

-- | /O(n)/. Compute the product of a finite list of 'LogField's,
-- being careful to avoid numerical error due to loss of precision.
-- That is, the following equivalence holds (modulo underflow and
-- all that):
--
-- > LogField . accurateProduct == accurateProduct . map LogField
{-# INLINE accurateProduct #-}
accurateProduct :: (ExpField a, Foldable f) => f (LogField a) -> LogField a
accurateProduct = LogField . fst . F.foldr kahanPlus (zero, zero)
  where
    kahanPlus (LogField x) (t, c) =
        let y  = x - c
            t' = t + y
            c' = (t' - t) - y
        in  (t', c')

-- This version *completely* eliminates rounding errors and loss
-- of significance due to catastrophic cancellation during summation.
-- <http://code.activestate.com/recipes/393090/> Also see the other
-- implementations given there. For Python's actual C implementation,
-- see math_fsum in
-- <http://svn.python.org/view/python/trunk/Modules/mathmodule.c?view=markup>
--
-- For merely *mitigating* errors rather than completely eliminating
-- them, see <http://code.activestate.com/recipes/298339/>.
--
-- A good test case is @msum([1, 1e100, 1, -1e100] * 10000) == 20000.0@
{-
-- For proof of correctness, see
-- <www-2.cs.cmu.edu/afs/cs/project/quake/public/papers/robust-arithmetic.ps>
def msum(xs):
    partials = [] # sorted, non-overlapping partial sums
    # N.B., the actual C implementation uses a 32 array, doubling size as needed
    for x in xs:
        i = 0
        for y in partials: # for(i = j = 0; j < n; j++)
            if abs(x) < abs(y):
                x, y = y, x
            hi = x + y
            lo = y - (hi - x)
            if lo != 0.0:
                partials[i] = lo
                i += 1
            x = hi
        # does an append of x while dropping all the partials after
        # i. The C version does n=i; and leaves the garbage in place
        partials[i:] = [x]
    # BUG: this last step isn't entirely correct and can lose
    # precision <http://stackoverflow.com/a/2704565/358069>
    return sum(partials, 0.0)
-}
