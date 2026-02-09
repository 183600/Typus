#!/usr/bin/env runhaskell

-- 导入模块
import Utils (isCompleteStringLiteral, isProblematicUnclosedString)

-- 测试函数
main :: IO ()
main = do
    putStrLn "Testing isCompleteStringLiteral:"
    putStrLn $ "isCompleteStringLiteral \"a\": " ++ show (isCompleteStringLiteral "a")
    putStrLn $ "isCompleteStringLiteral \"\\\"a\\\"\": " ++ show (isCompleteStringLiteral "\"a\"")
    putStrLn $ "isCompleteStringLiteral \"'a\": " ++ show (isCompleteStringLiteral "'a")
    putStrLn $ "isCompleteStringLiteral \"\\\"a\": " ++ show (isCompleteStringLiteral "\"a")
    
    putStrLn "\nTesting isProblematicUnclosedString:"
    putStrLn $ "isProblematicUnclosedString \"\": " ++ show (isProblematicUnclosedString "")