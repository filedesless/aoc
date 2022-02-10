module Day05Spec (spec) where

import Day05
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do {

  ; describe "day05a" $ do {
      ; it "should work with basic examples" $ do {
          ; day05a "dabAcCaCBAcCcaDA" `shouldBe` 10
          }
      }

  ; describe "day05b" $ do {
      ; it "should work with basic examples" $ do {
          ; day05b "dabAcCaCBAcCcaDA" `shouldBe` 4
          }
      }

  }
