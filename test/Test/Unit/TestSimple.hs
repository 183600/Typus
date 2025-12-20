-- 简化版的removeComments
removeCommentsSimple :: String -> String
removeCommentsSimple = goNormal
  where
    goNormal [] = []
    goNormal ('/':'*':xs) = go 1 xs  -- 开始块注释
    goNormal (c:cs) = c : goNormal cs
    
    go 0 xs = goNormal xs  -- 注释结束
    go n [] = []  -- 未闭合注释
    go n ('/':'*':xs) = go (n+1) xs  -- 嵌套注释
    go n ('*':'/':xs) = go (n-1) xs  -- 注释结束
    go n (_:xs) = go n xs  -- 跳过其他字符

main :: IO ()
main = do
    let test1 = "/*a*/b"
    putStrLn $ "Test 1: " ++ show test1
    putStrLn $ "Result: " ++ show (removeCommentsSimple test1)
    
    let test2 = "/* outer /* inner */a"
    putStrLn $ "Test 2: " ++ show test2
    putStrLn $ "Result: " ++ show (removeCommentsSimple test2)