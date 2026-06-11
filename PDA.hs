module PDA where

import Data.Char (isUpper)
import Parser

data Config = Config
  { remaining :: String
  , stack     :: [Symbol]
  , history   :: [String]
  } deriving (Show, Eq, Ord)

sententialForm :: Config -> String
sententialForm c = unwords (stack c)

applyProduction :: Grammar -> Config -> [Config]
applyProduction g (Config inp (top:rest) hist)
  | isUpper (head top) =
      [ Config inp (rhs ++ rest) (hist ++ [unwords (rhs ++ rest)])
      | (lhs, rhs) <- productions g, lhs == top ]
applyProduction _ _ = []

matchTerminal :: Config -> [Config]
matchTerminal (Config (i:inp) (top:rest) hist)
  | not (isUpper (head top)) && [i] == top =
      [Config inp rest (hist ++ [inp])]
matchTerminal _ = []

step :: Grammar -> Config -> [Config]
step g c = applyProduction g c ++ matchTerminal c