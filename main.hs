main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  case parseGrammar content of
    Left err -> putStrLn $ "Error: " ++ err
    Right g  -> print g  -- replace with PDA construction later