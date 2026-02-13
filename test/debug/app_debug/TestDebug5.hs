module Main where

import Utils

main :: IO ()
main = do
    let s = "A\\"
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    
    putStrLn $ "Testing s = " ++ show s
    putStrLn $ "Closed: " ++ show closed
    putStrLn $ "Unclosed: " ++ show unclosed
    putStrLn $ "isProblematicUnclosedString closed: " ++ show (isProblematicUnclosedString closed)
    putStrLn $ "isProblematicUnclosedString unclosed: " ++ show (isProblematicUnclosedString unclosed)
    
    -- 测试期望
    putStrLn $ "\nTest expects:"
    putStrLn $ "not (isProblematicUnclosedString closed) && isProblematicUnclosedString unclosed"
    putStrLn $ "Actual result: " ++ show (not (isProblematicUnclosedString closed) && isProblematicUnclosedString unclosed)