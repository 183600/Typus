import Utils

-- 测试失败的测试用例
main :: IO ()
main = do
  putStrLn "Testing isCompleteStringLiteral function..."
  
  -- 测试 1: IsCompleteStringLiteral valid
  putStrLn "\n=== Test 1: IsCompleteStringLiteral valid ==="
  let test1_input = "\"hello\""
  let result1 = isCompleteStringLiteral test1_input
  putStrLn $ "Input: " ++ show test1_input
  putStrLn $ "Output: " ++ show result1
  putStrLn $ "Expected: True"
  putStrLn $ "Test 1 " ++ if result1 then "PASSED" else "FAILED"
  
  -- 测试 2: IsCompleteStringLiteral invalid
  putStrLn "\n=== Test 2: IsCompleteStringLiteral invalid ==="
  let test2_input = "\"hello\\"
  let result2 = isCompleteStringLiteral test2_input
  putStrLn $ "Input: " ++ show test2_input
  putStrLn $ "Output: " ++ show result2
  putStrLn $ "Expected: False"
  putStrLn $ "Test 2 " ++ if not result2 then "PASSED" else "FAILED"
  
  -- 测试 3: IsCompleteStringLiteral empty
  putStrLn "\n=== Test 3: IsCompleteStringLiteral empty ==="
  let test3_input = "\"\""
  let result3 = isCompleteStringLiteral test3_input
  putStrLn $ "Input: " ++ show test3_input
  putStrLn $ "Output: " ++ show result3
  putStrLn $ "Expected: True"
  putStrLn $ "Test 3 " ++ if result3 then "PASSED" else "FAILED"