import Utils

main :: IO ()
main = do
    let s = "a"
    putStrLn $ "Testing s = " ++ show s
    putStrLn $ "isProblematicUnclosedString s: " ++ show (isProblematicUnclosedString s)
    
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    putStrLn $ "closed: " ++ show closed
    putStrLn $ "unclosed: " ++ show unclosed
    putStrLn $ "isCompleteStringLiteral closed: " ++ show (isCompleteStringLiteral closed)
    putStrLn $ "isCompleteStringLiteral unclosed: " ++ show (isCompleteStringLiteral unclosed)
    
    let result = if isCompleteStringLiteral closed && not (isCompleteStringLiteral unclosed)
                 then True
                 else if isCompleteStringLiteral closed && isCompleteStringLiteral unclosed
                      then False
                      else False
    putStrLn $ "Test result: " ++ show result