module LibSpec where

import Test.Hspec
import Test.HUnit.NumHask (shouldBeAbout)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Test.HUnit.NumHask" $ do
    it "Tests Float values for approximate equality" $ 
      (1/3 :: Float) `shouldBeAbout` 0.33333334
    it "Tests Double values for approximate equality" $ 
      (1/3 :: Double) `shouldBeAbout` 0.3333333333333333
