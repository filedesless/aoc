module Main where

import Text.Printf (printf)
import Data.Text (justifyRight, unpack, pack)
import Day01 (day01a, day01b)
import Day02 (day02a, day02b)
import Day03 (day03a, day03b)
import Day04 (day04a, day04b)
import Day05 (day05a, day05b)
import Day06 (day06a, day06b)
import Day07 (day07a, day07b)
import Day08 (day08a, day08b)
import Day09 (day09a, day09b)

run :: (Show a) => Integer  -> String -> (String -> a) -> IO ()
run day part function =
  readFile ("input/" ++ unpack (justifyRight 2 '0' (pack (show day))) ++ ".txt")
  >>= printf "Day %02d %s: %s\n" day part . (show . function)

main :: IO ()
main = do
  run 1 "a)" day01a
  run 1 "b)" day01b
  run 2 "a)" day02a
  run 2 "b)" day02b
  run 3 "a)" day03a
  run 3 "b)" day03b
  run 4 "a)" day04a
  run 4 "b)" day04b
  run 5 "a)" day05a
  run 5 "b)" day05b
  run 6 "a)" day06a
  run 6 "b)" (day06b 10000)
  run 7 "a)" day07a
  run 7 "b)" day07b
  run 8 "a)" day08a
  run 8 "b)" day08b
  run 9 "a)" day09a
  run 9 "b)" day09b
