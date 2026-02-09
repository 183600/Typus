#!/usr/bin/env runhaskell

import qualified Utils
import Data.List (isInfixOf)

-- 重新测试 RemoveComments strings with comments
testRemoveCommentsCase :: IO ()
testRemoveCommentsCase = do
    -- 失败案例：str = "", comment = "a"
    let str = ""
    let comment = "a"
    let stringWithComment = "\"" ++ str ++ " /* " ++ comment ++ " */\""
    let result = Utils.removeComments stringWithComment
    let commentStr = "/* " ++ comment ++ " */"
    let quotedStr = "\"" ++ str ++ "\""
    
    putStrLn "=== RemoveComments strings with comments 失败案例 ==="
    putStrLn $ "str = " ++ show str
    putStrLn $ "comment = " ++ show comment
    putStrLn $ "stringWithComment = " ++ show stringWithComment
    putStrLn $ "result = " ++ show result
    putStrLn $ "commentStr = " ++ show commentStr
    putStrLn $ "quotedStr = " ++ show quotedStr
    putStrLn $ "注释应该被移除: " ++ show (not (commentStr `isInfixOf` result))
    putStrLn $ "字符串内容应该保留: " ++ show (quotedStr `isInfixOf` result)
    
    -- 测试期望
    let expected = "\"" ++ str ++ "\""
    putStrLn $ "期望结果: " ++ show expected
    putStrLn $ "实际结果符合期望: " ++ show (result == expected)
    putStrLn ""

-- 测试 isCompleteStringLiteral invalid
testIsCompleteStringLiteralInvalid :: IO ()
testIsCompleteStringLiteralInvalid = do
    -- 失败案例："\""
    let input = "\""
    putStrLn "=== isCompleteStringLiteral invalid 失败案例 ==="
    putStrLn $ "input = " ++ show input
    putStrLn $ "isCompleteStringLiteral input = " ++ show (Utils.isCompleteStringLiteral input)
    putStrLn $ "期望结果: False (因为这是不完整的字符串字面量)"
    putStrLn $ "实际结果符合期望: " ++ show (not (Utils.isCompleteStringLiteral input))
    putStrLn ""

main :: IO ()
main = do
    testRemoveCommentsCase
    testIsCompleteStringLiteralInvalid