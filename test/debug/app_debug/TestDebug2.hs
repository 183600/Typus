module Main where

import Utils

main :: IO ()
main = do
    let testCases = ["\"", "%", "\SYN", "\SUB", "]", "#", "\1073968", " ", "+"]
    
    putStrLn "Testing isCompleteStringLiteral and isProblematicUnclosedString:"
    mapM_ runTest testCases
  where
    runTest s = do
        let withEscape = "\"" ++ s ++ "\\\""
        let isComplete = isCompleteStringLiteral withEscape
        let isProblematic = isProblematicUnclosedString withEscape
        putStrLn $ "Input: " ++ show s ++ ", WithEscape: " ++ show withEscape ++ ", isComplete: " ++ show isComplete ++ ", isProblematic: " ++ show isProblematic