import Utils

-- 模拟removeComments的关键部分
goNormal :: String -> String
goNormal [] = []
goNormal ('"':xs) = '"' : goInString xs
goNormal ('\'':xs) = '\'' : goInChar xs
goNormal ('/':'/':'*':xs) = '/' : skipBlock xs 0  -- 处理 //+/* 的情况
goNormal ('/':'/':xs) = skipLine xs
goNormal ('/':'*':xs) = skipBlock xs 0
goNormal ('/':xs) = '/' : goNormal xs  -- 处理单个/的情况
goNormal (c:cs) = c : goNormal cs

-- 跳过行注释直到换行，保留换行
skipLine :: String -> String
skipLine [] = []
skipLine ('\n':xs) = '\n' : goNormal xs
skipLine (_:xs) = skipLine xs

-- 跳过块注释直到 */，支持嵌套
skipBlock :: String -> Int -> String
skipBlock [] _depth = []  -- 注释未闭合，返回空
skipBlock ('\n':xs) _depth = '\n' : skipBlock xs _depth  -- 保留换行
skipBlock ('/':'*':xs) depth = skipBlock xs (depth + (1 :: Int))  -- 嵌套块注释
skipBlock ('*':'/':xs) 0 = goNormal xs  -- 最外层注释结束
skipBlock ('*':'/':xs) depth = skipBlock xs (depth - (1 :: Int))  -- 内层注释结束
skipBlock ('\\':x:xs) depth = '\\' : x : skipBlock xs depth  -- 保留转义字符
skipBlock (_:xs) _depth = skipBlock xs _depth  -- 跳过其他字符

-- 字符串字面量（保留内容与转义）
goInString :: String -> String
goInString [] = []  -- 非严格：未闭合字符串
goInString ('\n':xs) = '\n' : goNormal xs  -- 换行时结束字符串字面量
-- 在字符串中，保留所有字符包括注释标记
goInString ('/':'/':xs) = '/' : '/' : goInString xs  -- 保留 //
goInString ('/':'*':xs) = '/' : '*' : goInString xs  -- 保留 /*
goInString ('*':'/':xs) = '*' : '/' : goInString xs  -- 保留 */
goInString ('\\':x:xs) = '\\' : x : goInString xs
goInString ('"':xs) = '"' : goNormal xs
goInString (c:cs) = c : goInString cs

-- 字符字面量（保留内容与转义）
goInChar :: String -> String
goInChar [] = []  -- 非严格：未闭合字符，返回到正常模式
goInChar ('\n':xs) = '\n' : goNormal xs  -- 换行时结束字符字面量
goInChar ('\\':x:xs) = '\\' : x : goInChar xs
goInChar ('\'':xs) = '\'' : goNormal xs
-- 在字符字面量中，保留所有字符包括注释标记
goInChar ('/':'/':xs) = '/' : '/' : goInChar xs  -- 保留 //
goInChar ('/':'*':xs) = '/' : '*' : goInChar xs  -- 保留 /*
goInChar ('*':'/':xs) = '*' : '/' : goInChar xs  -- 保留 */
goInChar (c:cs) = c : goInChar cs

main :: IO ()
main = do
  let input = "//\""
  putStrLn $ "Input: " ++ show input
  putStrLn $ "First two chars: " ++ show (take 2 input)
  putStrLn $ "Matches // pattern: " ++ show (take 2 input == "//")
  let result = goNormal input
  putStrLn $ "Result: " ++ show result