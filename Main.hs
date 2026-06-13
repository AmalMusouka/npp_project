module Main where

import System.Environment (getArgs)
import Parser
import BFS

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      content <- readFile file
      case parseGrammar content of
        Left err -> putStrLn $ "Error: " ++ err
        Right g  -> do
          putStr "Enter string: "
          input <- getLine
          case bfs g input of
            Nothing    -> putStrLn "No Match"
            Just steps -> do
              mapM_ putStrLn steps
              putStrLn "Match"
    _ -> putStrLn "Usage: runghc Main.hs <file.cfg>"
