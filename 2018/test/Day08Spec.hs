module Day08Spec (spec) where

import Day08
import Test.Hspec (Spec, describe, it, shouldBe)
import Data.Tree
import Test.QuickCheck (property)

expected :: (Int, Tree [Int])
expected = (16,
            Node [1,1,2] [
               Node [10,11,12] [],
               Node [2] [
                   Node [99] [] ] ] )

spec :: Spec
spec = do {

  ; describe "readNumbers" $ do {
      ; it "should work with basic examples" $ do {
          ; readNumbers "0 1 99" `shouldBe` [0, 1, 99]
          }
      }

  ; describe "buildTree" $ do {
      ; it "should work for 1 node" $ do {
          ; buildTree [0, 1, 99] `shouldBe` (3, Node [99] [])
          }

      ; it "should work for nested nodes" $ do {
          ; buildTree [1, 1, 0, 1, 99, 2] `shouldBe` (6, Node [2] [Node [99] []])
          }

      ; it "should work for multiple nodes" $ do {
          ; input <- readFile "input/08.test.txt"
          ; buildTree (readNumbers input) `shouldBe` expected
          }
      }

  ; describe "day08a" $ do {
      ; it "should return 138 for example input" $ do {
          ; input <- readFile "input/08.test.txt"
          ; day08a input `shouldBe` 138
          }
      }

  ; describe "day08b" $ do {
      ; it "should return 66 for example input" $ do {
          ; input <- readFile "input/08.test.txt"
          ; day08b input `shouldBe` 66
          }
      }
  }
