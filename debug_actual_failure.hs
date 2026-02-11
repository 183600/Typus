import Utils

main :: IO ()
main = do
  putStrLn "=== Testing the actual failure case ==="
  
  -- The actual failure case from the test output
  let s = "\""
  let withBackslash = "\"" ++ s ++ "\\\\"
  let result = isCompleteStringLiteral withBackslash
  
  putStrLn $ "s: " ++ show s
  putStrLn $ "withBackslash: " ++ show withBackslash
  putStrLn $ "withBackslash chars: " ++ show (map (\c -> (c, fromEnum c)) withBackslash)
  putStrLn $ "result: " ++ show result
  putStrLn $ "expected: True"
  
  putStrLn "\n=== Understanding this string ==="
  putStrLn $ "The string " ++ show withBackslash ++ " represents:"
  putStrLn $ "  First quote: \""
  putStrLn $ "  Second quote: \""
  putStrLn $ "  First backslash: \\"
  putStrLn $ "  Second backslash: \\"
  putStrLn $ "This is: empty string literal + two backslashes"
  putStrLn $ "This should NOT be a complete string literal"
  
  putStrLn "\n=== Testing what might be expected ==="
  putStrLn $ "Maybe the test expects different behavior?"
  putStrLn $ "Let's test isCompleteStringLiteral on various inputs:"
  
  let testCases = [
        ("\"", "single quote"),
        ("\"\"", "empty string literal"),
        ("\"\\\\", "empty string + backslash"),
        ("\"\\\\\"", "empty string + escaped backslash"),
        ("\"\\\"", "escaped quote")
        ]
  mapM_ (\(str, desc) -> do
    let result = isCompleteStringLiteral str
    putStrLn $ desc ++ ": " ++ show str ++ " -> " ++ show result
    ) testCases