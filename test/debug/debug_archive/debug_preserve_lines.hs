import Utils

main :: IO ()
main = do
  let s = "\n "  -- 这是测试失败的输入
      result = Utils.normalizeIndentation s
      lines' = lines s
      resultLines = lines result
  
  putStrLn $ "Input: " ++ show s
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Input lines: " ++ show lines'
  putStrLn $ "Result lines: " ++ show resultLines
  putStrLn $ "Input line count: " ++ show (length lines')
  putStrLn $ "Result line count: " ++ show (length resultLines)
  putStrLn $ "Line count preserved: " ++ show (length lines' == length resultLines)