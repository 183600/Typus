import Utils (isProblematicUnclosedString)

main :: IO ()
main = do
    let closed = "\"\""  -- 空字符串字面量
    let unclosed = "\""   -- 单个引号
    
    putStrLn $ "closed: " ++ show closed
    putStrLn $ "isProblematicUnclosedString closed: " ++ show (isProblematicUnclosedString closed)
    putStrLn $ ""
    putStrLn $ "unclosed: " ++ show unclosed
    putStrLn $ "isProblematicUnclosedString unclosed: " ++ show (isProblematicUnclosedString unclosed)
    
    putStrLn $ ""
    putStrLn $ "Test expects:"
    putStrLn $ "isProblematicUnclosedString closed: False"
    putStrLn $ "isProblematicUnclosedString unclosed: True"