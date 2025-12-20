import Utils (removeComments)

-- 简化版的removeComments用于调试
removeCommentsDebug :: String -> String
removeCommentsDebug = goNormal
  where
    goNormal [] = []
    goNormal ('/':'/':xs) = skipLine xs
    goNormal ('/':'*':xs) = skipBlock xs
    goNormal ('"':xs) = '\"' : goInString xs
    goNormal ('\'':xs) = '\'' : goInChar xs
    goNormal (c:cs) = c : goNormal cs

    skipLine [] = []
    skipLine ('\n':xs) = '\n' : goNormal xs
    skipLine (_:xs) = skipLine xs

    skipBlock [] = []
    skipBlock xs = skipBlockAtLevel 1 xs
    
    skipBlockAtLevel 0 xs = goNormal xs
    skipBlockAtLevel n [] = []
    skipBlockAtLevel n ('/':'*':xs) = skipBlockAtLevel (n+1) xs
    skipBlockAtLevel n ('*':'/':xs) = skipBlockAtLevel (n-1) xs
    skipBlockAtLevel n ('\n':xs) = '\n' : skipBlockAtLevel n xs
    skipBlockAtLevel n (_:xs) = skipBlockAtLevel n xs

    goInString [] = []
    goInString ('\n':xs) = '\n' : goNormal xs
    goInString ('\\':x:xs) = '\\' : x : goInString xs
    goInString ('"':xs) = '\"' : goNormal xs
    goInString (c:cs) = c : goInString cs

    goInChar [] = []
    goInChar ('\\':x:xs) = '\\' : x : goInChar xs
    goInChar ('\'':xs) = '\'' : goNormal xs
    goInChar (c:cs) = c : goInChar cs

main :: IO ()
main = do
    let test1 = "/* outer /* inner */a"
    putStrLn $ "Test 1: " ++ show test1
    putStrLn $ "Original Result: " ++ show (removeComments test1)
    putStrLn $ "Debug Result: " ++ show (removeCommentsDebug test1)