#!/usr/bin/env stack
-- stack script --resolver lts-21.25

import qualified Parser as P

main :: IO ()
main = do
  let testInput = "import \"a\""
  putStrLn $ "Testing input: " ++ testInput
  case P.parseTypus testInput of
    Right result -> putStrLn $ "Success: " ++ show result
    Left err -> putStrLn $ "Error: " ++ err