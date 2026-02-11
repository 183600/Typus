import Utils

-- 测试失败案例：根据错误信息，输入是 "\\"
testFailureCase :: IO ()
testFailureCase = do
  -- 测试失败案例：根据错误信息，输入是 "\\"
  -- 但这可能是一个显示问题，实际输入可能是 "\"\\"
  let failureCase = "\"\\"
  let result = isCompleteStringLiteral failureCase
  
  putStrLn $ "=== Testing isCompleteStringLiteral failure case ==="
  putStrLn $ "Input: " ++ show failureCase
  putStrLn $ "String characters: " ++ show (map (\c -> (c, fromEnum c)) failureCase)
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Expected: False (incomplete string literal)"
  putStrLn ""
  
  -- 测试另一种可能性：实际的输入是 "\\" 但测试期望不同
  let anotherCase = "\\"
  let result2 = isCompleteStringLiteral anotherCase
  
  putStrLn $ "=== Testing another possibility ==="
  putStrLn $ "Input: " ++ show anotherCase
  putStrLn $ "String characters: " ++ show (map (\c -> (c, fromEnum c)) anotherCase)
  putStrLn $ "Result: " ++ show result2
  putStrLn ""
  
  -- 测试 prop_is_complete_string_literal_escape_backslash 的逻辑
  let s = ""
  let withBackslash = "\"" ++ s ++ "\\\\\""
  let result3 = isCompleteStringLiteral withBackslash
  
  putStrLn $ "=== Testing prop_is_complete_string_literal_escape_backslash ==="
  putStrLn $ "Input s: " ++ show s
  putStrLn $ "With backslash: " ++ show withBackslash
  putStrLn $ "String characters: " ++ show (map (\c -> (c, fromEnum c)) withBackslash)
  putStrLn $ "Result: " ++ show result3
  putStrLn ""

main :: IO ()
main = do
  testFailureCase
