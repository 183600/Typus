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
    putStrLn $ "isCompleteStringLiteral closed: " ++ show (isCompleteStringLiteral closed)
    putStrLn $ "isCompleteStringLiteral unclosed: " ++ show (isCompleteStringLiteral unclosed)
    putStrLn $ "isProblematicUnclosedString closed: " ++ show (isProblematicUnclosedString closed)
    putStrLn $ "isProblematicUnclosedString unclosed: " ++ show (isProblematicUnclosedString unclosed)