import Utils

main :: IO ()
main = do
    let s = "a"
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    putStrLn $ "s = " ++ show s
    putStrLn $ "closed = " ++ show closed
    putStrLn $ "unclosed = " ++ show unclosed
    putStrLn $ "isProblematicUnclosedString closed: " ++ show (isProblematicUnclosedString closed)
    putStrLn $ "isProblematicUnclosedString unclosed: " ++ show (isProblematicUnclosedString unclosed)
    putStrLn $ "isCompleteStringLiteral closed: " ++ show (isCompleteStringLiteral closed)
    putStrLn $ "isCompleteStringLiteral unclosed: " ++ show (isCompleteStringLiteral unclosed)
    
    let testResult = not (isProblematicUnclosedString closed) && isProblematicUnclosedString unclosed
    putStrLn $ "Test result: " ++ show testResult