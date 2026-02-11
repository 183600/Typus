import qualified Utils as U

main :: IO ()
main = do
  let testInput = ""
      escaped = "\"" ++ testInput ++ "\\\"\""
      result = U.isCompleteStringLiteral escaped
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Escaped: " ++ show escaped
  putStrLn $ "isCompleteStringLiteral result: " ++ show result
  putStrLn $ "Expected: True"
  
  -- 测试其他情况
  let testCases = ["\"\\\"\"", "\"\\\\\"\"", "\"\\\\\\\"\"", "\"\\\\\\\\\"\""]
  mapM_ (\tc -> putStrLn $ tc ++ " -> " ++ show (U.isCompleteStringLiteral tc)) testCases