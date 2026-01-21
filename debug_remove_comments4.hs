import Data.List (isPrefixOf)

-- 简化版的removeComments函数，用于调试
removeCommentsDebug :: String -> String
removeCommentsDebug s = goNormal s
  where
    -- 通用的注释处理函数
    goNormal [] = []
    goNormal ('"':xs) = '"' : goInString xs
    goNormal ('\'':xs) = '\'' : goInChar xs
    goNormal ('/':'/':xs) = skipLine xs
    goNormal ('/':'*':xs) = skipBlock xs 0
    goNormal (c:cs) = c : goNormal cs

    -- 跳过行注释直到换行，保留换行
    skipLine [] = []
    skipLine ('\n':xs) = '\n' : goNormal xs
    skipLine (_:xs) = skipLine xs

    -- 跳过块注释直到 */，支持嵌套
    skipBlock [] _depth = []  -- 注释未闭合，返回空
    skipBlock ('\n':xs) _depth = '\n' : skipBlock xs _depth  -- 保留换行
    skipBlock ('/':'*':xs) depth = skipBlock xs (depth + (1 :: Int))  -- 嵌套块注释
    skipBlock ('*':'/':xs) 0 = goNormal xs  -- 最外层注释结束
    skipBlock ('*':'/':xs) depth = skipBlock xs (depth - (1 :: Int))  -- 内层注释结束
    skipBlock (_:xs) _depth = skipBlock xs _depth  -- 跳过其他字符
    
    -- 字符串字面量（保留内容与转义）
    goInString [] = []  -- 非严格：未闭合字符串
    goInString ('\n':xs) = '\n' : goNormal xs  -- 换行时结束字符串字面量
    goInString ('\\':x:xs) = '\\' : x : goInString xs
    goInString ('"':xs) = '"' : goNormal xs  -- 字符串结束，继续正常处理
    goInString (c:cs) = c : goInString cs

    -- 字符字面量（保留内容与转义）
    goInChar [] = []  -- 非严格：未闭合字符，返回到正常模式
    goInChar ('\n':xs) = '\n' : goNormal xs  -- 换行时结束字符字面量
    goInChar ('\\':x:xs) = '\\' : x : goInChar xs
    goInChar ('\'':xs) = '\'' : goNormal xs  -- 字符结束，继续正常处理
    goInChar (c:cs) = c : goInChar cs

main :: IO ()
main = do
    let testCase = "\"string // not comment\" // real comment"
        result = removeCommentsDebug testCase
    
    putStrLn $ "Input: " ++ show testCase
    putStrLn $ "Output: " ++ show result
    putStrLn $ "Expected: \"string // not comment\" "