module Test.QuickCheck.Checkers.NumHask where

import Test.QuickCheck hiding (generate)
import Test.QuickCheck.Random (QCGen, newQCGen)

import Test.QuickCheck.Gen      (Gen (..)) -- for rand
-- import Test.QuickCheck.Property (Prop(..)) -- for evaluate

import Test.QuickCheck.Utils




{----------------------------------------------------------
    Misc
----------------------------------------------------------}

-- | Named test
type Test = (String,Property)

-- | Named batch of tests
type TestBatch = (String,[Test])

-- -- | Flatten a test batch for inclusion in another
-- unbatch :: TestBatch -> [Test]
-- unbatch (batchName,props) = map (first ((batchName ++ ": ")++)) props
