module PDA where

import Data.Char (isUpper)
import Parser

data Config = Config
  { remaining  :: String
  , stack      :: [Symbol]
  , derivation :: [String]
  } deriving (Show, Eq, Ord)

applyProduction :: Grammar -> Config -> [Config]
applyProduction g (Config input (top : rest) deriv)
  | isUpper top =
      [ Config input (rhs ++ rest) (deriv ++ [[lhs] ++ "->" ++ rhs])
      | (lhs, rhs) <- productions g, lhs == top ]
applyProduction _ _ = [] -- when stack is empty

matchTerminal :: Config -> [Config]
matchTerminal (Config (i : input) (top : rest) deriv)
  | not (isUpper top) && i == top =
      [Config input rest deriv]
matchTerminal _ = []

step :: Grammar -> Config -> [Config]
step g c = applyProduction g c ++ matchTerminal c