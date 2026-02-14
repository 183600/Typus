import Parser

testInput1 :: String
testInput1 = "//! ownership: on\nfunc a() { go func() {}() }"

testInput2 :: String
testInput2 = "package main\n//! ownership: on\nfunc a() { go func() {}() }"

main :: IO ()
main = do
  putStrLn "Testing Directive with concurrency (without package)..."
  putStrLn $ "Input: " ++ testInput1
  case parseTypus testInput1 of
    Right result -> do
      putStrLn "Parse succeeded!"
      putStrLn $ "Result: " ++ show result
    Left err -> do
      putStrLn "Parse failed!"
      putStrLn $ "Error: " ++ err
  
  putStrLn "\nTesting Directive with concurrency (with package)..."
  putStrLn $ "Input: " ++ testInput2
  case parseTypus testInput2 of
    Right result -> do
      putStrLn "Parse succeeded!"
      putStrLn $ "Result: " ++ show result
    Left err -> do
      putStrLn "Parse failed!"
      putStrLn $ "Error: " ++ err