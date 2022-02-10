module Day03Spec (spec) where

import Day03
import Test.Hspec (Spec, describe, it, shouldBe)
import Data.Function (on)
import Data.Tuple (swap)

inputClaims :: String
inputClaims = unlines [ "#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2" ]

expectedClaims :: [Claim]
expectedClaims = [
  Claim 1 (Point 1 3) (Point 4 4),
  Claim 2 (Point 3 1) (Point 4 4),
  Claim 3 (Point 5 5) (Point 2 2)
  ]

expectedOverlaps :: [ ((Int, Int), Bool) ]
expectedOverlaps = [ ((0, 1), True), ((0, 2), False), ((1, 2), False) ]

spec :: Spec
spec = do
    describe "claims" $
      it "should work with basic examples" $ do
        claims inputClaims `shouldBe` expectedClaims

    describe "overlap" $ do
      it "should work with basic examples" $ do
        map (uncurry (overlap `on` ((!!) expectedClaims)))
          (map fst expectedOverlaps) `shouldBe` (map snd expectedOverlaps)

      it "should also work with their opposite" $ do
        map (uncurry (overlap `on` ((!!) expectedClaims)))
          (map (swap . fst) expectedOverlaps) `shouldBe` (map snd expectedOverlaps)
