-- Find the shortest path from the specified source vertex to the end vertex and
-- use Graphviz to visually display the shortest path.
-- .\dijkstra graph.txt 1 4
-- (find the shortest path from vertex 1 to vertex 4)

import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import Dijkstra (dijkstra)
import Data.List (foldl', intercalate)
import Data.Maybe (fromJust, isNothing)

type Graph = Map.Map Int [(Int, Double)]

parseEdge :: String -> (Int, Int, Double)
parseEdge line = let [u, v, w] = words line in (read u, read v, read w)

buildGraph :: [(Int, Int, Double)] -> Graph
buildGraph edges = foldl' insertEdge Map.empty edges
  where
    insertEdge g (u, v, w) = Map.insertWith (++) u [(v, w)] g

-- Helper function to get the shortest path from predMap
getPath :: Int -> Int -> Map.Map Int Int -> Maybe [Int]
getPath start end predMap = reversePath end []
  where
    reversePath v path
      | v == start = Just (start : path)
      | otherwise = case Map.lookup v predMap of
                      Nothing -> Nothing
                      Just pred -> reversePath pred (v : path)

graphToDot :: Graph -> Map.Map Int Int -> Maybe [Int] -> String
graphToDot graph predMap path = "digraph G {\n" ++ unlines edges ++ "}"
  where
    edges = [show u ++ " -> " ++ show v ++ " [label=\"" ++ show w ++ "\"" ++ highlightEdge u v ++ "];"
            | (u, vs) <- Map.toList graph, (v, w) <- vs]
    highlightEdge u v = case path of
                          Just p -> if (u, v) `elem` zip p (tail p) then ", color=red" else ""
                          Nothing -> ""

main :: IO ()
main = do
  args <- getArgs
  let (filename, startVertex, endVertex) = case args of
        [f, s, e] -> (f, read s, read e)
        _ -> error "Usage: dijkstra <filename> <startVertex> <endVertex>"
  content <- readFile filename
  let edges = map parseEdge (lines content)
  let graph = buildGraph edges
  let (distMap, predMap) = dijkstra graph startVertex
  let path = getPath startVertex endVertex predMap
  case path of
    Nothing -> putStrLn "No path found"
    Just p -> writeFile "output.dot" (graphToDot graph predMap (Just p))
