import Utils

-- 测试失败的测试用例
main :: IO ()
main = do
  putStrLn "Testing removeComments function..."
  
  -- 测试 2: RemoveComments strings with comments - 详细分析
  putStrLn "\n=== Test 2: RemoveComments strings with comments (detailed) ==="
  let test2_input = "\"hello /*\""
  let result2 = removeComments test2_input
  let expected2 = "\"hello\""
  
  putStrLn $ "Input: " ++ show test2_input
  putStrLn $ "Output: " ++ show result2
  putStrLn $ "Expected: " ++ show expected2
  putStrLn $ "Test 2 " ++ if result2 == expected2 then "PASSED" else "FAILED"
  
  -- 手动测试
  putStrLn "\nManual testing:"
  let manual1 = "\"hello"
  let manual_result1 = removeComments manual1
  putStrLn $ "Manual 1: " ++ show manual1 ++ " -> " ++ show manual_result1
  
  let manual2 = "\"hello "
  let manual_result2 = removeComments manual2
  putStrLn $ "Manual 2: " ++ show manual2 ++ " -> " ++ show manual_result2
  
  let manual3 = "\"hello /"
  let manual_result3 = removeComments manual3
  putStrLn $ "Manual 3: " ++ show manual3 ++ " -> " ++ show manual_result3
  
  let manual4 = "\"hello /*"
  let manual_result4 = removeComments manual4
  putStrLn $ "Manual 4: " ++ show manual4 ++ " -> " ++ show manual_result4
  
  -- 测试空格
  putStrLn "\nSpace testing:"
  let space1 = " "
  let space_result1 = removeComments space1
  putStrLn $ "Space 1: " ++ show space1 ++ " -> " ++ show space_result1
  
  let space2 = "\"hello \""
  let space_result2 = removeComments space2
  putStrLn $ "Space 2: " ++ show space2 ++ " -> " ++ show space_result2