module Day06 where

import Data.List
import Data.List.Split( splitOn )
import Data.Function( on )

data Point = Pt Int Int deriving Eq
data Location = Loc { locChar :: Char, locPt :: Point } deriving Eq

dist :: Point -> Point -> Int
dist (Pt a b) (Pt c d) = ((+) `on` abs) (a - c) (b - d)

grid :: [Location] -> Point -> Char
grid locs (Pt x y)
  | Just loc <- find ((== Pt x y) . locPt) locs = locChar loc
  | length (filter ((== snd mdistance) . snd) distances) > 1 = '.'
  | otherwise = locChar $ fst mdistance
  where
    mdistance = minimumBy (compare `on` snd) distances
    distances = map (\(Loc c pt) -> (Loc c pt, dist pt (Pt x y))) locs

getLocs :: String -> [Location]
getLocs s = [ Loc c (Pt x y) | (c, (x, y)) <- zip (['a'..'z'] ++ ['A'..'Z']) (getTups s) ]

getTups :: String -> [(Int, Int)]
getTups s = [ (read x :: Int, read y :: Int) | (x:y:rest) <- map (splitOn ", " ) $ lines s ]

day06a :: String -> Int
day06a s = maximum $ map (\c -> length $ filter (== c) fullGrid) finites
  where
    fullGrid = map (grid locs) [ Pt x y | (x, y) <- numGrid ]
    numGrid = [ (x, y) | y <- [0..uy], x <- [0..ux] ]
    finites = charset \\ map (grid locs)
      [ Pt x y | (x, y) <- numGrid, x <= 0 || x >= ux || y <= 0 || y >= uy ]
    (ux, uy) = (fst $ maximumBy (compare `on` fst) tups, snd $ maximumBy (compare `on` snd) tups)
    (charset, locs, tups) = (map locChar locs, getLocs s, getTups s)

sdist :: [Location] -> Point -> Int
sdist locs pt = sum $ map (dist pt) (map locPt locs)

day06b :: Int -> String -> Int
day06b m s = length [ 1 | x <- [0..ux], y <- [0..uy], sdist locs (Pt x y) < m ]
  where
    (ux, uy) = (fst $ maximumBy (compare `on` fst) tups, snd $ maximumBy (compare `on` snd) tups)
    (locs, tups) = (getLocs s, getTups s)



