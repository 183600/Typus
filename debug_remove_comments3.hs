-- 导入修复后的Utils模块
import Utils

main :: IO ()
main = do
  let input = "//\""
  let result = removeComments input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Input quotes: " ++ show (length $ filter (== '"') input)
  putStrLn $ "Result quotes: " ++ show (length $ filter (== '"') result)
  
  -- 测试更多案例
  let testCases = 
        [ "//\""
        , "\"//\""
        , "code // comment"
        , "\"code // not comment\""
        , "/* block comment */"
        , "\"/* not comment */\""
        ]
  
  putStrLn "\n--- Test Cases ---"
  mapM_ (\tc -> do
            let r = removeComments tc
            putStrLn $ "Input:  " ++ show tc
            putStrLn $ "Output: " ++ show r
            putStrLn "") testCases