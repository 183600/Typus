import Parser

testInput :: String
testInput = " @#$ malformed syntax @#$"

main :: IO ()
main = do
  putStrLn "Testing Syntax error handling..."
  putStrLn $ "Input: " ++ show testInput
  case parseTypus testInput of
    Right result -> do
      putStrLn "Parse succeeded (unexpected)!"
      putStrLn $ "Result: " ++ show result
    Left err -> do
      putStrLn "Parse failed (expected)!"
      putStrLn $ "Error: " ++ err