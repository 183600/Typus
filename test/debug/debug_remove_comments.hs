import System.IO
import Data.List (isInfixOf)

-- 简化版的 removeComments 函数，只关注块注释部分
removeCommentsSimple :: String -> String
removeCommentsSimple = goNormal
  where
    goNormal [] = []
    goNormal ('/':'*':xs) = skipBlock xs
    goNormal (c:cs) = c : goNormal cs

    skipBlock = go 1
      where
        go 1 ('*':'/':xs) = goNormal xs  -- 最外层注释结束，返回正常处理
        go n ('*':'/':xs) = go (n-1) xs  -- 内层注释结束，减少嵌套层级
        go n ('/':'*':xs) = go (n+1) xs  -- 遇到新的嵌套注释，增加嵌套层级
        go n ('\n':xs) = '\n' : go n xs  -- 保留换行，保持当前嵌套层级
        go n (c:xs) = go n xs  -- 跳过其他字符，保持当前嵌套层级
        go 1 [] = []  -- 最外层注释未闭合，返回空
        go n [] = []  -- 内层注释未闭合，返回空

-- 测试函数
testNestedComments :: IO ()
testNestedComments = do
    putStrLn "Testing removeComments with nested comments..."
    
    -- 测试用例: "" "" "a" -> "/* outer /* inner */a"
    let input = "/* outer /* inner */a"
    let result = removeCommentsSimple input
    let expectedContains = "a"
    
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected to contain: " ++ show expectedContains
    putStrLn $ "Actually contains 'a': " ++ show (isInfixOf "a" result)
    
    -- 手动分析这个字符串应该如何处理
    putStrLn "\nManual analysis:"
    putStrLn $ "Input string: " ++ input
    putStrLn "Positions: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24"
    putStrLn $ "Chars:     " ++ map (\c -> if c == ' ' then '_' else c) (take 25 input)
    
    -- 分析注释结构
    putStrLn "\nComment structure analysis:"
    putStrLn "Position 0-1: /* (start outer comment)"
    putStrLn "Position 2-7: outer "
    putStrLn "Position 8-9: /* (start inner comment)"
    putStrLn "Position 10-15: inner "
    putStrLn "Position 16-17: */ (end inner comment)"
    putStrLn "Position 18: a (this should be OUTSIDE all comments)"
    putStrLn "Position 19-20: ???"
    
    -- 测试其他简单情况
    putStrLn "\nTesting simpler cases:"
    
    let simple1 = "/* comment */text"
    let result1 = removeCommentsSimple simple1
    putStrLn $ "Simple1: " ++ show simple1 ++ " -> " ++ show result1
    
    let simple2 = "/* /* */text"
    let result2 = removeCommentsSimple simple2
    putStrLn $ "Simple2: " ++ show simple2 ++ " -> " ++ show result2

main :: IO ()
main = testNestedComments