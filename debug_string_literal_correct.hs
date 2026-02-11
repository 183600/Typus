import Utils

main :: IO ()
main = do
  putStrLn "=== Testing prop_is_complete_string_literal_escape_backslash correctly ==="
  
  let s = "\\"
  let withBackslash = "\"" ++ s ++ "\\\\"
  let result = isCompleteStringLiteral withBackslash
  
  putStrLn $ "s: " ++ show s
  putStrLn $ "withBackslash: " ++ show withBackslash
  putStrLn $ "withBackslash chars: " ++ show (map (\c -> (c, fromEnum c)) withBackslash)
  putStrLn $ "result: " ++ show result
  putStrLn $ "expected: True"
  
  -- Test what this string literal represents
  putStrLn "\n=== Understanding the string literal ==="
  putStrLn $ "The string literal " ++ show withBackslash ++ " represents:"
  putStrLn $ "  Start quote: \""
  putStrLn $ "  First backslash: \\ (escaped as \\\\)"
  putStrLn $ "  Second backslash: \\ (escaped as \\\\)"  
  putStrLn $ "  End quote: \""
  putStrLn $ "This should be a complete string literal containing: \\\\"

  putStrLn "\n=== Testing other cases ==="
  let testCases = [
        ("\"\\\\", "quote + backslash + backslash"),
        ("\"\\\\\"", "quote + backslash + backslash + quote")
        ]
  mapM_ (\(str, desc) -> do
    let result = isCompleteStringLiteral str
    putStrLn $ desc ++ ": " ++ show str ++ " -> " ++ show result
    ) testCases