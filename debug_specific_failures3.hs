#!/usr/bin/env runhaskell

import qualified Utils
import Data.List (isInfixOf)

-- 测试具体的失败案例
main :: IO ()
main = do
    putStrLn "=== 具体失败案例分析 ==="
    
    -- 案例1: isCompleteStringLiteral valid 失败案例 
    let input1 = "\\"
    putStrLn $ "案例1: isCompleteStringLiteral " ++ show input1 ++ " = " ++ show (Utils.isCompleteStringLiteral input1)
    
    -- 案例2: isCompleteStringLiteral invalid 失败案例 
    let input2 = "\""
    putStrLn $ "案例2: isCompleteStringLiteral " ++ show input2 ++ " = " ++ show (Utils.isCompleteStringLiteral input2)
    
    -- 案例3: isProblematicUnclosedString 失败案例 
    putStrLn $ "案例3: isProblematicUnclosedString " ++ show input2 ++ " = " ++ show (Utils.isProblematicUnclosedString input2)
    
    -- 案例4: RemoveComments strings with comments 失败案例 "" 和 "a"
    let str = ""
    let comment = "a"
    let stringWithComment = "\"" ++ str ++ " /* " ++ comment ++ " */\""
    let result = Utils.removeComments stringWithComment
    let commentStr = "/* " ++ comment ++ " */"
    let quotedStr = "\"" ++ str ++ "\""
    putStrLn $ "案例4: RemoveComments"
    putStrLn $ "  输入: " ++ show stringWithComment
    putStrLn $ "  结果: " ++ show result
    putStrLn $ "  注释被移除: " ++ show (not (commentStr `isInfixOf` result))
    putStrLn $ "  字符串内容保留: " ++ show (quotedStr `isInfixOf` result)
    
    -- 分析转义字符
    putStrLn "\n=== 转义字符分析 ==="
    let testStrings = ["\\", "\"", "\\\"", "'", "\\'", "\"\\", "'\\\""]
    mapM_ (\s -> putStrLn $ show s ++ " -> isComplete: " ++ show (Utils.isCompleteStringLiteral s) ++ ", isProblematic: " ++ show (Utils.isProblematicUnclosedString s)) testStrings