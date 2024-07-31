module Dijkstra (dijkstra) where

import qualified Data.Map.Strict as Map
import qualified Data.IntSet as IntSet
import LeftistHeap (LeftistHeap, empty, insert, extractMin, isEmpty)
import Data.List (foldl')

type Graph = Map.Map Int [(Int, Double)]
type Distance = Double
type Vertex = Int
type DistMap = Map.Map Vertex Distance
type PredMap = Map.Map Vertex Vertex
type Visited = IntSet.IntSet
type Queue = LeftistHeap (Distance, Vertex)

dijkstra :: Graph -> Vertex -> (DistMap, PredMap)
dijkstra graph start = dijkstra' (insert (0, start) empty) Map.empty Map.empty IntSet.empty
  where
    dijkstra' :: Queue -> DistMap -> PredMap -> Visited -> (DistMap, PredMap)
    dijkstra' queue distMap predMap visited
      | isEmpty queue = (distMap, predMap)
      | IntSet.member u visited = dijkstra' restQueue distMap predMap visited
      | otherwise = dijkstra' newQueue newDistMap newPredMap newVisited
      where
        ((dist, u), restQueue) = extractMin queue
        neighbors = Map.findWithDefault [] u graph
        newVisited = IntSet.insert u visited
        (newQueue, newDistMap, newPredMap) = foldl' relax (restQueue, distMap, predMap) neighbors
        relax (q, dm, pm) (v, w) =
          let alt = dist + w
          in if alt < Map.findWithDefault (1 / 0) v dm
             then (insert (alt, v) q, Map.insert v alt dm, Map.insert v u pm)
             else (q, dm, pm)
