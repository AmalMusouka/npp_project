module Search where

import Data.Set (Set)
import qualified Data.Set as Set
import Parser
import PDA
import LeftistHeap

data Prioritized = Prioritized Int Config

instance Eq Prioritized where
  (Prioritized n1 _) == (Prioritized n2 _) = n1 == n2

instance Ord Prioritized where
  compare (Prioritized n1 _) (Prioritized n2 _) = compare n1 n2

unwrap :: Prioritized -> Config
unwrap (Prioritized _ c) = c

priority :: Config -> Prioritized
priority c = Prioritized (length (stack c)) c

type Visited = Set (String, [Symbol])

configKey :: Config -> (String, [Symbol])
configKey c = (remaining c, stack c)

isAccepted :: Config -> Bool
isAccepted c = null (remaining c) && null (stack c)

search :: Grammar -> String -> Maybe [String]
search g input = go queue visited
  where
    initial = Config input [start g] [[start g]]
    queue = insert (priority initial) Nil
    visited = Set.singleton (configKey initial)

    go heap visited
      | isEmpty heap = Nothing
      | otherwise =
          let Just p = findMin heap
              c = unwrap p
              rest = deleteMin heap
          in if isAccepted c
             then Just (derivation c)
             else
               let nexts = step g c
                   new = filter (not . (`Set.member` visited) . configKey) nexts
                   newVisited = foldr (Set.insert . configKey) visited new
                   newQueue = foldr (insert . priority) rest new
               in go newQueue newVisited