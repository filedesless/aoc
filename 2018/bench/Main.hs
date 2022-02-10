module Main (main) where

import Criterion.Main
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Day01( day01b_fast, day01b_slow )
import Day09( day09a, day09b )
import Day09Slow( day09a, day09b )

main :: IO ()
main = do
  day01Input <- readFile "input/01.txt"
  day09Input <- readFile "input/09.txt"
  defaultMain [
    bgroup "Day01" [ bench "day01b_intmap" $ whnf day01b_fast day01Input
                   , bench "day01b_map" $ whnf day01b_slow day01Input
                   ],
    bgroup "Day09" [ bench "day09b_deque" $ whnf Day09.day09b day09Input
                   , bench "day09b_STArray" $ whnf Day09Slow.day09b day09Input
                   ]
    ]
