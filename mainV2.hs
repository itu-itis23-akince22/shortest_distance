{-# LANGUAGE OverloadedStrings #-}

-- Import required Haskell modules
import Control.Exception (Exception, throw)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hSetEncoding, utf8, withFile)

-- Type aliases for clarity
type MID = String -- Machine-readable Freebase identifier

type Name = String -- Human-readable name

type Graph = Map.Map MID [MID] -- Directed graph represented as adjacency list

type MIDNameMap = Map.Map MID Name -- Mapping from MID to Name

-- Custom exception type for cases where no path exists
data NoPathException = NoPathException MID MID

-- Define how the NoPathException will be printed
instance Show NoPathException where
  show (NoPathException from to) = "No path between " ++ from ++ " and " ++ to

instance Exception NoPathException

-- Load MID-to-Name map from TSV file
-- Keeps only the first occurrence of each MID to ensure consistency
loadMIDNames :: FilePath -> IO MIDNameMap
loadMIDNames file = withFile file ReadMode $ \h -> do
  hSetEncoding h utf8 -- Ensure file is read using UTF-8 encoding
  content <- TIO.hGetContents h
  let entries = map (T.splitOn "\t") $ T.lines content
      pairs = [(T.unpack mid, T.unpack name) | [mid, name] <- entries]
      nameMap = foldl (\acc (k, v) -> Map.insertWith (\_ old -> old) k v acc) Map.empty pairs
  return nameMap

-- Load a directed graph from the Freebase TSV file
-- Each edge is constructed from the source and target MID
loadGraph :: FilePath -> IO Graph
loadGraph file = withFile file ReadMode $ \h -> do
  hSetEncoding h utf8 -- Ensure file is read using UTF-8 encoding
  content <- TIO.hGetContents h
  let entries = map (T.splitOn "\t") $ T.lines content
      edges = [(T.unpack src, T.unpack dst) | [src, _, dst] <- entries]
  return $ foldr (\(s, d) acc -> Map.insertWith (++) s [d] acc) Map.empty edges

-- Breadth-First Search algorithm to find the shortest path between two MIDs
-- Returns either a list of MIDs representing the path or a NoPathException
bfs :: Graph -> MID -> MID -> Either NoPathException [MID]
bfs graph start goal = bfs' Set.empty [(start, [start])]
  where
    bfs' _ [] = Left (NoPathException start goal) -- No more nodes to explore, no path found
    bfs' visited ((curr, path) : queue)
      | curr == goal = Right path -- Goal node reached, return path
      | Set.member curr visited = bfs' visited queue -- Skip already visited nodes
      | otherwise =
          let neighbors = fromMaybe [] (Map.lookup curr graph)
              newQueue = queue ++ [(n, path ++ [n]) | n <- neighbors, not (Set.member n visited)]
           in bfs' (Set.insert curr visited) newQueue -- Continue BFS with new queue

-- Convert list of MIDs to list of human-readable names using the name map
-- If a MID is not found in the map, fallback to the MID itself
midsToNames :: MIDNameMap -> [MID] -> [Name]
midsToNames nameMap = map (\mid -> fromMaybe mid (Map.lookup mid nameMap))

-- Main function: handles command-line arguments and executes the path search
main :: IO ()
main = do
  args <- getArgs
  case args of
    [start, end] -> do
      -- Load MID-to-name mappings and the graph
      nameMap <- loadMIDNames "mid2name.tsv"
      graph <- loadGraph "freebase.tsv"

      -- Get readable names for the start and end MIDs
      let nameStart = fromMaybe start (Map.lookup start nameMap)
          nameEnd = fromMaybe end (Map.lookup end nameMap)

      -- Inform the user about the source and destination
      putStrLn $ "Finding path from " ++ nameStart ++ " to " ++ nameEnd ++ "..."

      -- Execute BFS to find the shortest path
      case bfs graph start end of
        Right mids -> do
          let names = midsToNames nameMap mids
          putStrLn $ "Shortest distance: " ++ show (length mids - 1)
          putStrLn $ "Full path: " ++ concatMap (++ " -> ") (init names) ++ last names
        Left err -> print err -- Print exception if no path is found

    -- If arguments are invalid, show usage format
    _ -> putStrLn "Usage: pathfinder <startMID> <endMID>"