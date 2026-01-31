import Data.List (isInfixOf)
import Data.Char (isSpace)

-- 简化的 goInString 函数
goInString :: String -> String
goInString [] = []
goInString ('\n':xs) = '\n' : []  -- 简化版本，直接返回
goInString ('/':'/':xs) = '/' : '/' : goInString xs  -- 保留 //
goInString ('/':'*':xs) = '/' : '*' : goInString xs  -- 保留 /*
goInString ('*':'/':xs) = '*' : '/' : goInString xs  -- 保留 */
goInString ('\\':x:xs) = '\\' : x : goInString xs  -- 保留转义字符，包括转义引号
goInString ('"':xs) = '"' : []  -- 简化版本，直接返回
goInString (c:cs) = c : goInString cs

-- 测试函数
main :: IO ()
main = do
  let testCases = 
        [ "//\\\""
        , "a//\\\""
        , "a//\\\"b"
        ]
  
  mapM_ (\testCase -> do
    putStrLn $ "Input: " ++ show testCase
    let result = goInString testCase
    putStrLn $ "Result: " ++ show result
    putStrLn ""
    ) testCases