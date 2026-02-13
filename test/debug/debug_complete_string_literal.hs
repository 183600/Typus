import qualified Utils as U

main :: IO ()
main = do
  let s = "\"b\""
  putStrLn $ "Testing isCompleteStringLiteral:"
  putStrLn $ "Input: " ++ show s
  putStrLn $ "Result: " ++ show (U.isCompleteStringLiteral s)
  
  putStrLn $ "\nTesting isProblematicUnclosedString:"
  putStrLn $ "Input: " ++ show s
  putStrLn $ "Result: " ++ show (U.isProblematicUnclosedString s)
  
  -- Let's also check the specific conditions
  putStrLn $ "\nChecking conditions:"
  putStrLn $ "length s >= 2: " ++ show (length s >= 2)
  putStrLn $ "drop (length s - 2) s: " ++ show (drop (length s - 2) s)
  putStrLn $ "drop (length s - 2) s == \"\\\"\": " ++ show (drop (length s - 2) s == "\\\"")
  
  -- Check isCompleteStringLiteral logic
  putStrLn $ "\nChecking isCompleteStringLiteral pattern matching:"
  let s' = s
  case s' of
    [] -> putStrLn "Pattern: []"
    ['\''] -> putStrLn "Pattern: ['\\']"
    ['"'] -> putStrLn "Pattern: ['\"]"
    "\\" -> putStrLn "Pattern: \"\\\\\""
    ['"','\\'] -> putStrLn "Pattern: ['\"','\\']"
    "\\\\\"" -> putStrLn "Pattern: \"\\\\\\\\\""
    "\\\\\"" -> putStrLn "Pattern: \"\\\\\\\\\""
    ('"':'a':'\\':'\":[]) -> putStrLn "Pattern: ('\"':'a':'\\':'\":[])"
    ('"':_:'\\':'\":[]) -> putStrLn "Pattern: ('\"':_:'\\':'\":[])"
    ('"':_:'\\':'\":_:_) -> putStrLn "Pattern: ('\"':_:'\\':'\":_:_)
    "\"a" -> putStrLn "Pattern: \"\\\"a\""
    "\"a\"" -> putStrLn "Pattern: \"\\\"a\\\"\""
    "\"\"\\\"\"" -> putStrLn "Pattern: \"\\\"\\\"\\\\\"\\\"\""
    "\"\"\"" -> putStrLn "Pattern: \"\\\"\\\"\\\"\""
    "\"\"// not comment\"" -> putStrLn "Pattern: \"\\\"\\\"// not comment\\\"\""
    "\\\\\\\"\"" -> putStrLn "Pattern: \"\\\"\\\\\\\\\\\\\\\"\\\"\""
    ('"':_:'\\':'\\':'\":_) | not (s' == "\"\"\\\"\"" && not (s' == "\\\\\\\"\"")) -> putStrLn "Pattern: ('\"':_:'\\':'\\':'\":_)"
    (c:_) | c == '"' && endsWithDoubleBackslash s' -> putStrLn "Pattern: (c:_) where c == '\"' && endsWithDoubleBackslash"
    (c:_) -> putStrLn $ "Pattern: (c:_) where c = " ++ show c
    _ -> putStrLn "Pattern: _"
  where
    endsWithDoubleBackslash :: String -> Bool
    endsWithDoubleBackslash [] = False
    endsWithDoubleBackslash [_] = False
    endsWithDoubleBackslash inputStr = 
      let lastTwo = drop (length inputStr - 2) inputStr
      in lastTwo == "\\\\"