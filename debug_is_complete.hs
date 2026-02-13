#!/usr/bin/env runhaskell

import Utils

-- 测试 isCompleteStringLiteral
main :: IO ()
main = do
  let testCases = [
        ("\"a\"", "Double quote + a + double quote"),
        ("\"\"", "Empty string"),
        ("\"\\\"\"", "Double quote + escaped quote"),
        ("\"\\\\\"", "Double quote + escaped backslash")
        ]
  
  putStrLn "Testing isCompleteStringLiteral:"
  mapM_ (\(input, desc) -> do
    putStrLn $ desc ++ ": " ++ show input ++ " -> " ++ show (isCompleteStringLiteral input)
    ) testCases