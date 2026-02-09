module Main where

import Utils
import Data.List (isInfixOf)

main :: IO ()
main = do
    putStrLn "=== 测试所有失败案例 ==="
    
    -- 1. RemoveComments strings with comments - 失败案例: "\\" 和 ""
    putStrLn "\n1. RemoveComments strings with comments:"
    let str = "\\"
    let comment = ""
    let validStr = not ('\"' `elem` str) && not ('\'' `elem` str)
    let validComment = not ('\"' `elem` comment) && not ('\'' `elem` comment)
    
    putStrLn $ "str = " ++ show str ++ ", comment = " ++ show comment
    putStrLn $ "validStr = " ++ show validStr ++ ", validComment = " ++ show validComment
    
    if not (validStr && validComment) || null str && null comment
    then putStrLn "测试应该通过 (property True)"
    else do
        let stringWithComment = "\"" ++ str ++ " /* " ++ comment ++ " */\""
        let result = removeComments stringWithComment
        let commentStr = "/* " ++ comment ++ " */"
        let quotedStr = "\"" ++ str ++ "\""
        
        putStrLn $ "stringWithComment = " ++ show stringWithComment
        putStrLn $ "result = " ++ show result
        putStrLn $ "quotedStr `isInfixOf` result = " ++ show (quotedStr `isInfixOf` result)
    
    -- 2. IsCompleteStringLiteral invalid - 失败案例: "\"a
    putStrLn "\n2. IsCompleteStringLiteral invalid:"
    let test2 = "\"a"
    let result2 = isCompleteStringLiteral test2
    putStrLn $ "isCompleteStringLiteral " ++ show test2 ++ " = " ++ show result2 ++ " (期望: False)"
    
    -- 3. IsCompleteStringLiteral escaped quotes - 失败案例: \"
    putStrLn "\n3. IsCompleteStringLiteral escaped quotes:"
    let test3 = "\""
    let result3 = isCompleteStringLiteral test3
    putStrLn $ "isCompleteStringLiteral " ++ show test3 ++ " = " ++ show result3 ++ " (期望: True)"
    
    -- 4. IsProblematicUnclosedString - 失败案例: ""
    putStrLn "\n4. IsProblematicUnclosedString:"
    let test4 = ""
    let result4 = isProblematicUnclosedString test4
    putStrLn $ "isProblematicUnclosedString " ++ show test4 ++ " = " ++ show result4 ++ " (期望: True)"