import Utils

-- 测试失败的测试用例
main :: IO ()
main = do
  putStrLn "Testing removeComments strings with comments..."
  
  -- 测试字符串中包含注释
  putStrLn "\n=== Test strings with comments ==="
  let test1_input = "\"hello /* comment */\""
  let test1_result = removeComments test1_input
  let test1_expected = "\"hello\""
  putStrLn $ "Input: " ++ show test1_input
  putStrLn $ "Output: " ++ show test1_result
  putStrLn $ "Expected: " ++ show test1_expected
  putStrLn $ "Test 1 " ++ if test1_result == test1_expected then "PASSED" else "FAILED"
  
  -- 测试字符串中包含注释（空格）
  putStrLn "\n=== Test strings with comments (spaces) ==="
  let test2_input = "\"hello /* comment */\""
  let test2_result = removeComments test2_input
  let test2_expected = "\"hello\""
  putStrLn $ "Input: " ++ show test2_input
  putStrLn $ "Output: " ++ show test2_result
  putStrLn $ "Expected: " ++ show test2_expected
  putStrLn $ "Test 2 " ++ if test2_result == test2_expected then "PASSED" else "FAILED"