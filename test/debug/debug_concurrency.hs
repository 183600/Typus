import Parser

testInput :: String
testInput = "//! ownership: on\nfunc a() { go func() {}() }"

main :: IO ()
main = do
  putStrLn "Testing Directive with concurrency..."
  putStrLn $ "Input: " ++ testInput
  case parseTypus testInput of
    Right result -> do
      putStrLn "Parse succeeded!"
      putStrLn $ "Result: " ++ show result
    Left err -> do
      putStrLn "Parse failed!"
      putStrLn $ "Error: " ++ show err