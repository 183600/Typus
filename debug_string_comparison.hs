-- 测试字符串比较
testStringComparison :: IO ()
testStringComparison = do
  let input = "\t\ta\t"
  putStrLn $ "input: " ++ show input
  putStrLn $ "input == \"\\t\\ta\\t\": " ++ show (input == "\t\ta\t")
  
  -- 测试字符序列
  putStrLn $ "map (fromEnum) input: " ++ show (map fromEnum input)
  putStrLn $ "map (fromEnum) \"\\t\\ta\\t\": " ++ show (map fromEnum "\t\ta\t")

main :: IO ()
main = testStringComparison