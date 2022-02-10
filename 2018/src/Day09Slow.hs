module Day09Slow where

import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Control.Monad

-- doubly linked list ish
type Node = (Int, Int)

insertAfter :: STArray s Int Node -> Int -> Int -> ST s ()
insertAfter marbles pos val = do
  (c_p, c_n) <- readArray marbles pos
  writeArray marbles val (pos, c_n)
  writeArray marbles pos (c_p, val)
  ( _ , n_n) <- readArray marbles c_n
  writeArray marbles c_n (val, n_n)

remove :: STArray s Int Node -> Int -> ST s ()
remove marbles pos = do
  (c_p, c_n) <- readArray marbles pos
  (p_p,  _ ) <- readArray marbles c_p
  ( _ , n_n) <- readArray marbles c_n
  writeArray marbles c_p (p_p, c_n)
  writeArray marbles c_n (c_p, n_n)

play :: Int -> Int -> Array Int Int
play n m = runST $ do
  let lst = (1, 1) : (0, 0) : [ (0, 0) | _ <- [2..n] ]
  marbles <- newListArray (0, n) lst :: ST s (STArray s Int Node)
  players <- newArray (0, m) 0 :: ST s (STArray s Int Int)
  let turn pos i
        | mod i 23 == 0 = do
            s <- readArray marbles pos
            (prev, next) <- foldM (\(prev, next) _ -> readArray marbles prev) s [1..6]
            (new, _) <- readArray marbles next
            remove marbles prev
            let j = mod i m
            readArray players j >>= writeArray players j . (prev + i +)
            return new
        | otherwise = do
            (_, next) <- readArray marbles pos
            insertAfter marbles next i
            return i
  foldM_ turn 1 [2..n]
  freeze players

highest :: Int -> Int -> Int
highest m n = maximum $ play n m

getInput :: String -> (Int, Int)
getInput s = (read (Prelude.head w) :: Int, read (w !! 6) :: Int)
  where w = words s

day09a :: String -> Int
day09a = uncurry highest . getInput

day09b :: String -> Int
day09b = uncurry highest . fmap (* 100) . getInput
