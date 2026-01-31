-- 带调试信息的测试
goInString :: String -> String
goInString [] = []
goInString ('\\':x:xs) = '\\' : x : goInString xs
goInString ('/':'/':xs) = '/' : '/' : goInString xs
goInString ('/':'*':xs) = '/' : '*' : goInString xs
goInString ('*':'/':xs) = '*' : '/' : goInString xs
goInString ('"':xs) = '"' : goNormal xs
goInString ('\n':xs) = '\n' : goNormal xs
goInString (c:cs) = c : goInString cs

goNormal :: String -> String
goNormal [] = []
goNormal ('"':xs) = '"' : goInString xs
goNormal ('\'':xs) = '\'' : goInChar xs
goNormal ('/':'/':xs) = skipLine xs
goNormal ('/':'*':xs) = skipBlock xs 0
goNormal ('/':xs) = '/' : goNormal xs
goNormal (c:cs) = c : goNormal cs

goInChar :: String -> String
goInChar [] = []
goInChar ('\\':x:xs) = '\\' : x : goInChar xs
goInChar ('\'':xs) = '\'' : goNormal xs
goInChar (c:cs) = c : goInChar cs

skipLine :: String -> String
skipLine [] = []
skipLine ('\n':xs) = '\n' : goNormal xs
skipLine (_:xs) = skipLine xs

skipBlock :: String -> Int -> String
skipBlock [] _ = []
skipBlock ('*':'/':xs) 0 = goNormal xs
skipBlock (c:cs) depth = skipBlock cs depth

-- 测试函数
main :: IO ()
main = do
  let input = "a//\""
  putStrLn $ "Input: " ++ show input
  let result = goNormal input
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Input quotes: " ++ show (length $ filter (== '"') input)
  putStrLn $ "Result quotes: " ++ show (length $ filter (== '"') result)
  
  putStrLn "\nTesting goInString directly:"
  let stringInput = "//\""
  putStrLn $ "String input: " ++ show stringInput
  let stringResult = goInString stringInput
  putStrLn $ "String result: " ++ show stringResult