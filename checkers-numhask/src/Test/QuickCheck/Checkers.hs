module Test.QuickCheck.Checkers where

import Control.Applicative (liftA2)

import Test.QuickCheck (Property, Arbitrary, Gen, Testable(..), property, forAll, (.&.), (==>), conjoin) 

import Control.Arrow (first)



{----------------------------------------------------------
    Misc
----------------------------------------------------------}

-- | Named test
type Test = (String, Property)

-- | Named batch of tests
type TestBatch = (String, [Test])

-- | Flatten a test batch for inclusion in another
unbatch :: TestBatch -> [Test]
unbatch (batchName, props) = map (first ((batchName ++ ": ")++)) props




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
class EqProp a where
  (=-=) :: a -> a -> Property

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





{----------------------------------------------------------
    Binary relationships
----------------------------------------------------------}

-- | Reflexive property: @a `rel` a@
reflexive :: (Arbitrary a, Show a) =>
             (a -> a -> Bool) -> Property
reflexive rel = property $ \ a -> a `rel` a

-- | Transitive property: @a `rel` b && b `rel` c ==> a `rel` c@.
-- Generate @a@ randomly, but use @gen a@ to generate @b@ and @gen b@ to
-- generate @c@.  @gen@ ought to satisfy @rel@ fairly often.
transitive :: (Arbitrary a, Show a) =>
              (a -> a -> Bool) -> (a -> Gen a) -> Property
transitive rel gen =
  property $ \ a ->
    forAll (gen a) $ \ b ->
      forAll (gen b) $ \ c ->
        (a `rel` b) && (b `rel` c) ==> (a `rel` c)

-- | Symmetric property: @a `rel` b ==> b `rel` a@.  Generate @a@
-- randomly, but use @gen a@ to generate @b@.  @gen@ ought to satisfy
-- @rel@ fairly often.
symmetric :: (Arbitrary a, Show a) =>
             (a -> a -> Bool) -> (a -> Gen a) -> Property
symmetric rel gen =
  property $ \ a ->
    forAll (gen a) $ \ b ->
      (a `rel` b) ==> (b `rel` a)

-- | Symmetric property: @a `rel` b && b `rel` a ==> a == b@.  Generate
-- @a@ randomly, but use @gen a@ to generate @b@.  @gen@ ought to satisfy
-- both @rel@ directions fairly often but not always.
antiSymmetric :: (Arbitrary a, Show a, Eq a) =>
                 (a -> a -> Bool) -> (a -> Gen a) -> Property
antiSymmetric rel gen =
  property $ \ a ->
    forAll (gen a) $ \ b ->
      (a `rel` b) && (b `rel` a) ==> a == b

-- | Combinator for a property relating two elements
binary :: (Show t, Testable prop) => Gen t -> (t -> t -> prop) -> Property
binary gen rel = forAll gen $ \a ->
  forAll gen $ \b -> rel a b

-- | Combinator for a property relating three elements
ternary :: (Show t, Testable prop) =>
     Gen t -> (t -> t -> t -> prop) -> Property
ternary gen rel = forAll gen $ \a ->
  forAll gen $ \b ->
    forAll gen $ \c -> rel a b c


-- | Associative property
associative :: (Show a, Eq a) => (a -> a -> a) -> Gen a -> Property
associative rel gen = ternary gen $ \a b c ->
  (a `rel` b) `rel` c == a `rel` (b `rel` c)
  

-- | Right identity z 
--
-- a `op` z == a
rightIdentity
  :: (Show a, Eq a) => t -> (a -> t -> a) -> Gen a -> Property
rightIdentity z op gen = forAll gen $ \a ->
  a `op` z == a

-- | Left identity z 
--
-- z `op` a == a
leftIdentity
  :: (Show a, Eq a) => t -> (t -> a -> a) -> Gen a -> Property
leftIdentity z op gen = forAll gen $ \a ->
  z `op` a == a

-- | Identity z
--
-- z `op` a == a `op` z == a
identity :: (Show a, Eq a) => a -> (a -> a -> a) -> Gen a -> Property
identity z op gen = conjoin [
    leftIdentity z op gen
  , rightIdentity z op gen
                            ]

-- | Monoid properties
--
-- * Identity element
-- * Binary associative operation
monoid :: (Show a, Eq a) => a -> (a -> a -> a) -> Gen a -> Property
monoid z rel gen = conjoin [
    identity z rel gen
  , associative rel gen
  ]
