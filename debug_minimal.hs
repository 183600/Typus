-- 最小的测试用例
goInString :: String -> String
goInString [] = []
goInString ('\\':x:xs) = '\\' : x : goInString xs
goInString ('/':'/':xs) = '/' : '/' : goInString xs
goInString ('/':'*':xs) = '/' : '*' : goInString xs
goInString ('*':'/':xs) = '*' : '/' : goInString xs
goInString ('"':xs) = '"' : []
goInString ('\n':xs) = '\n' : []
goInString (c:cs) = c : goInString cs

goNormal :: String -> String
goNormal [] = []
goNormal ('"':xs) = '"' : goInString xs
goNormal (c:cs) = c : goNormal cs

-- 测试函数
main :: IO ()
main = do
  let input = "a//\""
  putStrLn $ "Input: " ++ show input
  let result = goNormal input
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Input quotes: " ++ show (length $ filter (== '"') input)
  putStrLn $ "Result quotes: " ++ show (length $ filter (== '"') result)