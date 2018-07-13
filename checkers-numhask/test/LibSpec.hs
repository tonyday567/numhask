module LibSpec where

import Test.Hspec

-- import Test.QuickCheck.Checkers

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Test.HUnit.NumHask" $ do
    it "placeholder" $
      True `shouldBe` True
