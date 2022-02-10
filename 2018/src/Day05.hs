module Day05 where

import Data.List

react :: Char -> Char -> Bool
react x y = 32 == abs (fromEnum x - fromEnum y)

reactAll :: String -> String
reactAll (x:xs) = foldl fn [x] $ delete '\n' xs
  where fn [] v = [v]
        fn (h:t) v
          | react h v = t
          | otherwise = v : h : t

day05a :: String -> Int
day05a = length . reactAll

day05b :: String -> Int
day05b s = minimum possibilities
  where possibilities = map (length . reactAll . possibility) ['a'..'z']
        possibility l = [ c | c <- (delete '\n' s), not $ react l c, c /= l ]
