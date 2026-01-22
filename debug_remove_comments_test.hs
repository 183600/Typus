import Utils

-- 测试函数
testRemoveComments :: String -> IO ()
testRemoveComments input = do
  let result = removeComments input
  let countQuotes s' = length $ filter (== '"') s'
  let originalQuotes = countQuotes input
  let resultQuotes = countQuotes result
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Original quotes: " ++ show originalQuotes
  putStrLn $ "Result quotes: " ++ show resultQuotes
  putStrLn $ "Test passes: " ++ show (originalQuotes == resultQuotes)
  putStrLn ""

-- 辅助函数
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (length needle) (drop i haystack) | i <- [0..length haystack - length needle]]
isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

main :: IO ()
main = do
  putStrLn "Testing removeComments function:"
  putStrLn ""
  
  -- 测试失败案例
  testRemoveComments "//\""
  
  -- 其他测试案例
  testRemoveComments "hello // world"
  testRemoveComments "\"hello // world\""
  testRemoveComments "\"code // comment\" more"
  testRemoveComments "\"//\""
  testRemoveComments "\"/* comment */\""
  testRemoveComments "code /* comment */ more"