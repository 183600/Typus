import qualified Utils as U

main :: IO ()
main = do
  let testInput = "a\t"
  putStrLn $ "Input: " ++ show testInput
  let result = U.normalizeIndentation testInput
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Expected: " ++ show "a "
  putStrLn $ "Test " ++ if result == "a " then "PASSED" else "FAILED"
  
  -- 让我们尝试直接调用map函数来测试转换
  let converted = map (\c -> if c == '\t' then ' ' else c) testInput
  putStrLn $ "Direct conversion: " ++ show converted