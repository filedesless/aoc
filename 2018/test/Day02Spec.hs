module Day02Spec (spec) where

import Day02
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  let expected = [
             ("abcdef", (False, False)),
             ("bababc", (True, True)),
             ("abbcde", (True, False)),
             ("abcccd", (False, True)),
             ("aabcdd", (True, False)),
             ("aabcee", (True, False)),
             ("ababab", (False, True))
             ] in do
  describe "hasDuo" $
    it "should work with given examples" $ do
      map (hasN 2) (map fst expected) `shouldBe` map (fst . snd) expected

  describe "hasTrio" $
    it "should work with given examples" $ do
      map (hasN 3) (map fst expected) `shouldBe` map (snd . snd) expected

  describe "hamming" $
    it "should work with given examples" $ do
      hamming "abcde" "axcye" `shouldBe` 2
      hamming "fghij" "fguij" `shouldBe` 1
