module PDA where

import Data.Char (isUpper)
import Parser

data Config = Config
  { remaining  :: String
  , stack      :: [Symbol]
  , derivation :: [String]
  } deriving (Show, Eq, Ord)

applyProduction :: Grammar -> Config -> [Config]
applyProduction g (Config inp (top:rest) deriv)
  | isUpper (head top) =
      [ Config inp (rhs ++ rest) (deriv ++ [top ++ "->" ++ concat rhs])
      | (lhs, rhs) <- productions g, lhs == top ]
applyProduction _ _ = [] -- when stack is empty

matchTerminal :: Config -> [Config]
matchTerminal (Config (i:inp) (top:rest) deriv)
  | not (isUpper (head top)) && [i] == top =
      [Config inp rest deriv]
matchTerminal _ = []

step :: Grammar -> Config -> [Config]
step g c = applyProduction g c ++ matchTerminal c