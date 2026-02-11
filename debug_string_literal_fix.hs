import Utils

main :: IO ()
main = do
  putStrLn "=== Testing isCompleteStringLiteral fix ==="
  
  -- Test case: isCompleteStringLiteral with escape backslash
  let testStr = "\\"
  let withBackslash = "\"" ++ testStr ++ "\\\\\""
  let result = isCompleteStringLiteral withBackslash
  putStrLn $ "Input: " ++ show testStr
  putStrLn $ "With backslash: " ++ show withBackslash
  putStrLn $ "isCompleteStringLiteral result: " ++ show result
  putStrLn $ "Expected: True"
  putStrLn ""
  
  -- Test some other cases
  putStrLn "=== Testing other cases ==="
  let testCases = [
        ("\"\\\\\"", "escaped backslash"),
        ("\"\\\\\\\\\"", "escaped backslash + escaped quote"),
        ("\"\"", "single quote"),
        ("\"\"\"", "empty string"),
        ("\"\\\\\\\\\"\"", "double escaped backslash")
        ]
  mapM_ (\(str, desc) -> do
    putStrLn $ desc ++ ": " ++ show str ++ " -> " ++ show (isCompleteStringLiteral str)
    ) testCases