module Day07 where

import Data.List
import Data.Maybe
import Debug.Trace

type GNode = Char
type GEdge = (GNode, GNode)
type Graph = [GEdge]

parse :: String -> Graph
parse s = [ (head (l !! 1), head (l !! 7)) | l <- map words $ lines s ]

doable :: Graph -> [GNode] -> [GNode]
doable graph done = sort $ nub $
      [ a | (a, _) <- graph,
        not $ elem a done,  not $ any ((== a). snd) graph ] ++
      [ b | (_, b) <- graph, not $ elem b done,
        all (\c -> elem c done) [ c | (c, d) <- graph, d == b ] ]

order :: Graph -> [GNode] -> [GNode]
order graph done = order' todo
  where order' [] = reverse done
        order' (h:_) = order graph (h : done)
        todo = doable graph done

work :: Graph -> [GNode] -> Int -> [(Int, (GNode, Int))] -> Int -> Int -> Int
work graph done tick working mw offset
  | null todo && null newwork = {- d $ -} tick
  | null todo = {- d $ -} work graph newdone (succ tick) newwork mw offset
  | length newwork < mw = {- d $ -} work graph done tick (schedule (head todo)) mw offset
  | otherwise = {- d $ -} work graph newdone (succ tick) newwork mw offset
  where
    newdone = nub $ done ++ [ task | (_, (task, _)) <- finished ]
    newwork = working \\ finished
    todo = doable graph newdone \\ [ task | (_, (task, _)) <- newwork ]
    schedule task = head [ (i, (task, tick)) | i <- [1..], all ((/= i) . fst) newwork ] : working
    finished = [ (i, (task, t)) | (i, (task, t)) <- working,
                 t + (fromEnum task - 64 + offset) == tick ]
    d x = trace ("====\ndone: " ++ done ++ "\ntick: " ++ show tick ++
                 "\nworking: " ++ show working ++ "\ntodo: " ++ show todo ++
                 "\nfinished: " ++ show finished) x

day07a :: String -> String
day07a s = order (parse s) []

day07b :: String -> Int
day07b s = work (parse s) [] 0 [] 5 60
