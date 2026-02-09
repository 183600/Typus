import Utils

-- 测试失败的测试用例
main :: IO ()
main = do
  putStrLn "Testing isCompleteStringLiteral function..."
  
  -- 测试随机字符串
  putStrLn "\n=== Test random strings ==="
  let testStrings = ["hello", "world", "test", "123", "!@#", "", "a"]
  mapM_ testString testStrings
  
  where
    testString s = do
      let input = "\"" ++ s ++ "\""
      let result = isCompleteStringLiteral input
      putStrLn $ "Input: " ++ show input ++ ", Output: " ++ show result
      if not result
        then putStrLn $ "ERROR: Expected True for " ++ show input
        else return ()