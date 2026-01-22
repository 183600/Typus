import System.IO

-- 测试 goInString 函数
goInString :: String -> String
goInString [] = []
goInString ('\n':xs) = '\n' : []  -- 简化版本，结束字符串字面量
-- 在字符串中，保留所有字符包括注释标记
goInString ('/':'/':xs) = '/' : '/' : goInString xs  -- 保留 //
goInString ('/':'*':xs) = '/' : '*' : goInString xs  -- 保留 /*
goInString ('*':'/':xs) = '*' : '/' : goInString xs  -- 保留 */
goInString ('\\':x:xs) = '\\' : x : goInString xs  -- 保留转义字符，包括转义引号
goInString ('"':xs) = '"' : []  -- 简化版本，结束字符串字面量
goInString (c:cs) = c : goInString cs

-- 测试 goNormal 函数
goNormal :: String -> String
goNormal [] = []
goNormal ('"':xs) = '"' : goInString xs
goNormal ('\'':xs) = '\'' : []  -- 简化版本，结束字符串字面量
goNormal ('/':'/':xs) = []  -- 简化版本，跳过行注释
goNormal ('/':'*':xs) = []  -- 简化版本，跳过块注释
goNormal ('/':xs) = '/' : goNormal xs  -- 处理单个/的情况
goNormal (c:cs) = c : goNormal cs

testGoNormal :: String -> IO ()
testGoNormal input = do
  let result = goNormal input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Result: " ++ show result
  putStrLn ""

testGoInString :: String -> IO ()
testGoInString input = do
  let result = goInString input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Result: " ++ show result
  putStrLn ""

main :: IO ()
main = do
  putStrLn "Testing goInString function:"
  testGoInString "//\""
  
  putStrLn "Testing goNormal function:"
  testGoNormal "//\""
  testGoNormal "\"//\""