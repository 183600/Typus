module Main where

import Utils

main :: IO ()
main = do
    let testCases = ["\"", "%", " "]
    
    putStrLn "Testing with detailed info:"
    mapM_ runTest testCases
  where
    runTest s = do
        let withEscape = "\"" ++ s ++ "\\\""
        putStrLn $ "Input: " ++ show s
        putStrLn $ "WithEscape: " ++ show withEscape
        putStrLn $ "Length: " ++ show (length withEscape)
        putStrLn $ "First 2 chars: " ++ show (take 2 withEscape)
        putStrLn $ "Last 2 chars: " ++ show (drop (length withEscape - 2) withEscape)
        putStrLn $ "isComplete: " ++ show (isCompleteStringLiteral withEscape)
        putStrLn $ "isProblematic: " ++ show (isProblematicUnclosedString withEscape)
        putStrLn ""