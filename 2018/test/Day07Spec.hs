module Day07Spec (spec) where

import Day07
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

expected :: [(Char, Char)]
expected = [
  ('C', 'A'), ('C', 'F'), ('A', 'B'), ('A', 'D'),
  ('B', 'E'), ('D', 'E'), ('F', 'E')
  ]

spec :: Spec
spec = do {

  ; describe "parse" $ do {
      ; it "should work with basic examples" $ do {
          ; input <- readFile "input/07.test.txt"
          ; parse input `shouldBe` expected
          }
      }

  ; describe "day07a" $ do {
      ; it "should work with basic examples" $ do {
          ; input <- readFile "input/07.test.txt"
          ; day07a input `shouldBe` "CABDFE"
          }
      }

  ; describe "work" $ do {
      ; it "should work with basic examples" $ do {
          ; input <- readFile "input/07.test.txt"
          ; work (parse input) [] 0 [] 2 0 `shouldBe` 15
          }
      }

  }
