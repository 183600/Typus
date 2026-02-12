import Utils (isProblematicUnclosedString)

main :: IO ()
main = do
    let properlyUnclosed = "\""  -- 包含转义引号的不完整字符串
    
    putStrLn $ "properlyUnclosed: " ++ show properlyUnclosed
    putStrLn $ "isProblematicUnclosedString properlyUnclosed: " ++ show (isProblematicUnclosedString properlyUnclosed)
    
    putStrLn $ ""
    putStrLn $ "Test expects:"
    putStrLn $ "isProblematicUnclosedString properlyUnclosed: True"