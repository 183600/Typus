module Main where

import Utils

main :: IO ()
main = do
    let s = "A\\"
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    
    putStrLn $ "Testing s = " ++ show s
    putStrLn $ "Closed: " ++ show closed ++ " (length: " ++ show (length closed) ++ ")"
    putStrLn $ "Unclosed: " ++ show unclosed ++ " (length: " ++ show (length unclosed) ++ ")"
    putStrLn $ "Are they equal? " ++ show (closed == unclosed)
    putStrLn $ "isCompleteStringLiteral closed: " ++ show (isCompleteStringLiteral closed)
    putStrLn $ "isCompleteStringLiteral unclosed: " ++ show (isCompleteStringLiteral unclosed)
    putStrLn $ "isProblematicUnclosedString closed: " ++ show (isProblematicUnclosedString closed)
    putStrLn $ "isProblematicUnclosedString unclosed: " ++ show (isProblematicUnclosedString unclosed)
    
    -- 检查测试期望
    putStrLn $ "\nTest expects for s = \"A\\\\\":"
    putStrLn $ "not (isProblematicUnclosedString closed) && isProblematicUnclosedString unclosed"
    putStrLn $ "Actual result: " ++ show (not (isProblematicUnclosedString closed) && isProblematicUnclosedString unclosed)