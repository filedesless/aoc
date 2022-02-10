module Day04 where

import Data.Time
import Data.List
import Data.List.Split
import Data.Function( on )

data Shift = Shift { guardId :: Int
                   , shiftDay :: Day
                   , sleepIntervals :: [(Int, Int)] -- [min,max[
                   } deriving (Show, Eq)

getSleepIntervals :: [String] -> [(Int, Int)]
getSleepIntervals sorted = [ (read fall, read wake) | (fall, wake) <- group2 xs ]
  where
    group2 [] = []
    group2 (fall:wake:res) = (fall, wake) : group2 res
    xs = [ drop 3 $ take 5 $ words line !! 1 |
           line <- sorted, " 00:" `isInfixOf` line,
           "falls" `isInfixOf` line || "wakes" `isInfixOf` line ]

getShifts' :: [String] -> [Shift]
getShifts' [] = []
getShifts' (h:t) = Shift gid day (getSleepIntervals today) : getShifts' after
  where
    gid = read $ splitOneOf "# " h !! 4
    day = if "00:" `isInfixOf` h then getDay h else succ (getDay h)
    getDay = extractDay . splitOn "-" . head . words
    extractDay [x, y, z] = fromGregorian (read $ drop 1 x) (read y) (read z)
    (today, after) = span (not . elem '#') t

getShifts :: String -> [Shift]
getShifts = getShifts' . sort . lines

shiftsOf :: [Shift] -> Int -> [Shift]
shiftsOf shifts id = filter ((==) id . guardId) shifts

day04a :: String -> Int
day04a s = minute * chosenId
  where
    minute = head $ maximumBy (compare `on` length) $ group minutes
    minutes = sort $ concatMap (\(l, u) -> [l..u-1]) $
      concatMap sleepIntervals $ shiftsOf shifts chosenId
    chosenId = fst $ maximumBy (compare `on` (sum . map (uncurry (flip (-)))) . snd) $
      map (\id -> (id, concatMap sleepIntervals $ shiftsOf shifts id)) ids
    ids = nub $ map guardId shifts
    shifts = getShifts s

day04b :: String -> Int
day04b s = chosenId * minute
  where
    (chosenId, (minute, frequency)) = maximumBy (compare `on` snd . snd) minutes
    minutes = [ (i, maximumBy (compare `on` snd) l) | (i, l) <- freqs ]
    freqs = [ (i, getFreqs sleeps) | (i, sleeps) <- pairs, not $ null sleeps ]
    getFreqs = map (\l -> (head l, length l)) . group . sort . concatMap (\(l, u) -> [l..u-1])
    pairs = map (\id -> (id, concatMap sleepIntervals $ shiftsOf shifts id)) ids
    ids = nub $ map guardId shifts
    shifts = getShifts s
