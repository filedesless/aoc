module Day09 where

import Data.Maybe
import qualified Deque as D

data Game = Game { getScores :: D.Deque Int
                 , getMarble :: D.Deque Int
                 } deriving (Show, Eq)

rot :: Int -> D.Deque a -> D.Deque a
rot n d = iterate (if n < 0 then D.shiftRight else D.shiftLeft) d !! (abs n)

turn :: Game -> Int -> Int -> Int -> Game
turn game@(Game scores marble) n m i
  | i == succ m = game
  | otherwise =
    let game'
          | mod i 23 == 0 = Game (rot 1 score') (D.tail lefty)
          | otherwise     = Game (rot 1 scores) (D.cons i (rot 2 marble))
    in turn game' n m (succ i)
  where
    score' = D.cons newval $ D.tail scores
    newval = sum $ catMaybes [Just i, D.head scores, D.head lefty]
    lefty = rot (-7) marble

highest :: Int -> Int -> Int
highest n m = foldr max 0 . getScores $ turn game n m 1
  where game = Game (D.fromList (take n (repeat 0))) (D.Deque [] [0])

getInput :: String -> (Int, Int)
getInput s = (read (Prelude.head w) :: Int, read (w !! 6) :: Int)
  where w = words s

day09a :: String -> Int
day09a = uncurry highest . getInput

day09b :: String -> Int
day09b = uncurry highest . fmap (* 100) . getInput
