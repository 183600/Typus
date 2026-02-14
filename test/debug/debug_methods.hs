import Parser

testInput :: String
testInput = "//! ownership: on\nfunc (r a) Method() {}"

main :: IO ()
main = do
  putStrLn "Testing Directive with methods..."
  putStrLn $ "Input: " ++ testInput
  case parseTypus testInput of
    Right result -> do
      putStrLn "Parse succeeded!"
      putStrLn $ "Result: " ++ show result
    Left err -> do
      putStrLn "Parse failed!"
      putStrLn $ "Error: " ++ show err