module Day01 where

import Data.Char
import Data.IntSet (IntSet)
import Data.Set (Set)
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

convert :: [String] -> [Int]
convert = map (read . filter (/= '+'))

day01a :: String -> Int
day01a = sum . convert . lines

takeUnseen :: IntSet -> [Int] -> Int
takeUnseen seen (x:xs)
  | IntSet.member x seen = x
  | otherwise = takeUnseen (IntSet.insert x seen) xs

takeUnseen' :: Set Int -> [Int] -> Int
takeUnseen' seen (x:xs)
  | Set.member x seen = x
  | otherwise = takeUnseen' (Set.insert x seen) xs

day01b :: String -> Int
day01b = day01b_fast
parse = scanl (+) 0 . cycle . convert . lines
day01b_fast = takeUnseen IntSet.empty . parse
day01b_slow = takeUnseen' Set.empty . parse
