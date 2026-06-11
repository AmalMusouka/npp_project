module Main where

import System.Environment (getArgs)
import Parser

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      content <- readFile file
      case parseGrammar content of
        Left err -> putStrLn $ "Error: " ++ err
        Right g  -> putStr $ prettyGrammar g
    _ -> putStrLn "Usage: runghc Main.hs <file.cfg>"