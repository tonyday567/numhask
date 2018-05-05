module Test.QuickCheck.Checkers.NumHask where

import Control.Applicative (liftA2)

import Test.QuickCheck hiding (generate)
import Test.QuickCheck.Random (QCGen, newQCGen)

-- import Test.QuickCheck.Gen      (Gen (..)) -- for rand
-- import Test.QuickCheck.Property (Prop(..)) -- for evaluate

import Test.QuickCheck.Utils

import Control.Arrow (first)
-- import Control.Arrow (Arrow,ArrowChoice,first,second,left,right,(>>>),arr)



{----------------------------------------------------------
    Misc
----------------------------------------------------------}

-- | Named test
type Test = (String,Property)

-- | Named batch of tests
type TestBatch = (String,[Test])

-- | Flatten a test batch for inclusion in another
unbatch :: TestBatch -> [Test]
unbatch (batchName,props) = map (first ((batchName ++ ": ")++)) props




-- | @f@ is its own inverse. See also 'inverse'.
involution :: (Show a, Arbitrary a, EqProp a) =>
              (a -> a) -> Property
involution f = f `inverseL` f

-- | @f@ is a left inverse of @g@.  See also 'inverse'.
inverseL :: (EqProp b, Arbitrary b, Show b) =>
            (a -> b) -> (b -> a) -> Property
f `inverseL` g = f . g =-= id

-- | @f@ is a left and right inverse of @g@.  See also 'inverseL'.
inverse :: ( EqProp a, Arbitrary a, Show a
           , EqProp b, Arbitrary b, Show b ) =>
           (a -> b) -> (b -> a) -> Property
f `inverse` g = f `inverseL` g .&. g `inverseL` f


{----------------------------------------------------------
    Generalized equality
----------------------------------------------------------}

infix  4 =-=

-- | Types of values that can be tested for equality, perhaps through
-- random sampling.
class EqProp a where (=-=) :: a -> a -> Property

-- | For 'Eq' types as 'EqProp' types
eq :: Eq a => a -> a -> Property
a `eq` a' = property (a == a')



-- Function equality
instance (Show a, Arbitrary a, EqProp b) => EqProp (a -> b) where
  f =-= f' = property (liftA2 (=-=) f f')
-- Alternative definition:
-- instance (Show a, Arbitrary a, EqProp b) => EqProp (a -> b) where
--   f =-= f' = property (probablisticPureCheck defaultConfig
--                                              (\x -> f x =-= g x))



-- Template: fill in with Eq types for a
--   instance EqProp a where (=-=) = eq
-- E.g.,

instance EqProp ()     where (=-=) = eq
instance EqProp Bool   where (=-=) = eq
instance EqProp Char   where (=-=) = eq
instance EqProp Int    where (=-=) = eq
instance EqProp Float  where (=-=) = eq
instance EqProp Double where (=-=) = eq
