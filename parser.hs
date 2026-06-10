module Grammar where

import Data.Char  (isUpper, isSpace)

type Symbol     = String
type Production = (Symbol, [Symbol])

data Grammar = Grammar
  { start       :: Symbol
  , productions :: [Production]
  } deriving (Show)

splitOnArrow :: String -> Maybe (String, String)
splitOnArrow [] = Nothing
splitOnArrow ('-':'>':rest) = Just ("", rest)
splitOnArrow (c:cs) =
  case splitOnArrow cs of
    Just (l, r) -> Just (c:l, r)
    Nothing     -> Nothing

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseRHS :: String -> [Symbol]
parseRHS [] = []
parseRHS (c:cs)
  | isSpace c = parseRHS cs
  | otherwise = [c] : parseRHS cs

parseProduction :: String -> Maybe Production
parseProduction line =
  case splitOnArrow line of
    Nothing     -> Nothing
    Just (l, r) ->
      let lhs = trim l
          rhs = parseRHS (trim r)
      in if null lhs then Nothing else Just (lhs, rhs)

parseGrammar :: String -> Either String Grammar
parseGrammar input =
  let nonEmpty = filter (not . null . trim) (lines input)
      parsed   = map parseProduction nonEmpty
  in case sequence parsed of
       Nothing            -> Left "Parse error: malformed production"
       Just []            -> Left "Empty grammar"
       Just prods@((s,_):_) -> Right $ Grammar s prods

prettyGrammar :: Grammar -> String
prettyGrammar g = unlines $ map prettyProd (productions g)
  where
    prettyProd (lhs, rhs) = lhs ++ " -> " ++ unwords rhs