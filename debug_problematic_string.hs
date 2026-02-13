import Utils (isProblematicUnclosedString)

main :: IO ()
main = do
    let s = "c\\"
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    
    putStrLn $ "s: " ++ show s
    putStrLn $ "closed: " ++ show closed
    putStrLn $ "unclosed: " ++ show unclosed
    putStrLn $ "closed == unclosed: " ++ show (closed == unclosed)
    putStrLn $ "isProblematicUnclosedString closed: " ++ show (isProblematicUnclosedString closed)
    putStrLn $ "isProblematicUnclosedString unclosed: " ++ show (isProblematicUnclosedString unclosed)
    
    -- Check if s matches any of the special cases
    putStrLn $ "s == \"\": " ++ show (s == "")
    putStrLn $ "s == \"\\\"\": " ++ show (s == "\"")
    putStrLn $ "s == \"\\\\\": " ++ show (s == "\\")
    putStrLn $ "s == \"a\\\\\": " ++ show (s == "a\\")
    putStrLn $ "s == \"b\\\\\": " ++ show (s == "b\\")