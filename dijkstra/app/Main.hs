module Main (main) where

import qualified Data.Map as Map
type Graph = Map.Map String [(String, Int)]
graph :: Map.Map String [(String, Int)]
graph = Map.fromList destinations
        where destinations = [("start", [("a", 6), ("b",2)]),
                              ("a", [("fin", 1)]),
                              ("b", [("a", 3),("fin", 5)])]

infinity :: Double
infinity = 1/0 :: Double

find_shortest_path :: Graph -> String 
find_shortets_path gr = ""
main :: IO ()
main = print "salom"
