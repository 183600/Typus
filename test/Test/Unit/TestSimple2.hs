-- 简化版的removeComments，带调试
removeCommentsDebug :: String -> String
removeCommentsDebug = goNormal
  where
    goNormal [] = []
    goNormal ('/':'*':xs) = skipBlock xs
    goNormal (c:cs) = c : goNormal cs
    
    skipBlock [] = []
    skipBlock ('*':'/':xs) = "END:" ++ goNormal xs  -- 添加标记
    skipBlock ('\n':xs) = '\n' : skipBlock xs
    skipBlock (c:cs) = skipBlock cs

main :: IO ()
main = do
    let test1 = "/*a*/b"
    putStrLn $ "Test 1: " ++ show test1
    putStrLn $ "Result: " ++ show (removeCommentsDebug test1)
    
    let test2 = "/* outer /* inner */a"
    putStrLn $ "Test 2: " ++ show test2
    putStrLn $ "Result: " ++ show (removeCommentsDebug test2)