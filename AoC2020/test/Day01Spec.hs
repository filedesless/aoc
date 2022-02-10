module Day01Spec where

import Day01
import Test.Hspec

spec :: Spec
spec = do
  describe "day01a" $ do
    it "returns true" $ do
      day01a "" `shouldBe` ""
  describe "day01b" $ do
    it "returns true" $ do
      day01b "" `shouldBe` ""