module BFS where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import Parser
import PDA

type Visited = Set (String, [Symbol])

configKey :: Config -> (String, [Symbol])
configKey c = (remaining c, stack c)

isAccepted :: Config -> Bool
isAccepted c = null (remaining c) && null (stack c)

bfs :: Grammar -> String -> Maybe [String]
bfs g input = go queue visited
  where
    initial = Config input [start g] [start g]
    queue = Seq.singleton initial
    visited = Set.singleton (configKey initial)
    go Empty _ = Nothing
    go (c :<| rest) visited
      | isAccepted c = Just (derivation c)
      | otherwise =
          let nexts = step g c
              new = filter (not . (`Set.member` visited) . configKey) nexts
              newVisited = foldr (Set.insert . configKey) visited new
              newQueue = foldl (|>) rest new
          in go newQueue newVisited