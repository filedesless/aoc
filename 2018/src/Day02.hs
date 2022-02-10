module Day02 where

import Data.List (find)

hasN :: Int -> String -> Bool
hasN n [] = False
hasN n (x:xs) =
  length [ y | y <- xs, x == y ] == (pred n) ||
  hasN n [ y | y <- xs, x /= y ]

day02a :: String -> Int
day02a s =
  length (filter (hasN 2) (lines s)) *
  length (filter (hasN 3) (lines s))

hamming :: String -> String -> Int
hamming s1 s2 = length [ x | (x, y) <- zip s1 s2, x /= y ]

day02b :: String -> String
day02b s =
  let pairs = [ (x, y) | x <- lst, y <- lst ] in
    case find (\(x, y) -> hamming x y == 1) pairs of
      Just (x, y) -> [ c1 | (c1, c2) <- zip x y, c1 == c2 ]
      Nothing -> error "Cannot find good box"
  where lst = lines s
