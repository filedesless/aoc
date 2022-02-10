module Day03 where

import Data.List
import Data.List.Split

data Point = Point Int Int deriving (Show, Eq, Ord)
data Claim = Claim Int Point Point deriving (Show, Eq)

getClaim :: String -> Claim
getClaim s = Claim cid pos dim
    where -- #1 @ 257,829: 10x23
      ws  = filter (not . null) $ splitOneOf "#@,:x " s
      cid = read $ head ws
      pos = Point (read (ws !! 1)) (read (ws !! 2))
      dim = Point (read (ws !! 3)) (read (ws !! 4))

claims :: String -> [Claim]
claims = map getClaim . lines

pointInClaim :: Point -> Claim -> Bool
pointInClaim (Point x1 y1) (Claim _ (Point x2 y2) (Point w2 h2)) =
  x1 >= x2 && x1 < x2 + w2 &&
  y1 >= y2 && y1 < y2 + h2

points :: Claim -> [Point]
points (Claim _ (Point x y) (Point w h)) =
  [ Point a b | a <- [x..x+w-1], b <- [y..y+h-1] ]

contours :: Claim -> [Point]
contours (Claim _ (Point x y) (Point w h)) =
  [ Point a b | a <- [x, xw], b <- [y..yh] ] ++
  [ Point a b | a <- [x..xw], b <- [y, yh] ]
  where (xw, yh) = (pred x + w, pred y + h)

overlap :: Claim -> Claim -> Bool
overlap claim1 claim2 =
  any (flip pointInClaim claim1) (contours claim2) ||
  any (flip pointInClaim claim2) (contours claim1)

nonOverlappingId :: [Claim] -> Int
nonOverlappingId xs =
  case find (\x -> not $ any (overlap x) (delete x xs)) xs of
    Just (Claim i _ _) -> i
    otherwise -> error "No free claim found"

day03a :: String -> Int
day03a = length . filter ((> 1) . length) . group . sort . concatMap points . claims

day03b :: String -> Int
day03b = nonOverlappingId . claims
