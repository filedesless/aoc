module Day01Spec (spec) where

import Day01 (day01a, day01b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "part 1" $
    it "should work with given examples" $ do
      day01a (unlines ["+1", "+1", "+1"]) `shouldBe` 3
      day01a (unlines ["+1", "+1", "-2"]) `shouldBe` 0
      day01a (unlines ["-1", "-2", "-3"]) `shouldBe` (-6)
  describe "part 2" $
    it "should work with given examples" $ do
      day01b (unlines ["+1", "-1"]) `shouldBe` 0
      day01b (unlines ["+3", "+3", "+4", "-2", "-4"]) `shouldBe` 10
      day01b (unlines ["-6", "+3", "+8", "+5", "-6"]) `shouldBe` 5
      day01b (unlines ["+7", "+7", "-2", "-7", "-4"]) `shouldBe` 14
