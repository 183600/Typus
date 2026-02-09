module Main where

import Utils
import Data.List (isInfixOf)

main :: IO ()
main = do
    putStrLn "=== 测试新的失败案例 ==="
    
    -- RemoveComments strings with comments 失败案例: "a" 和 ""
    let str = "a"
    let comment = ""
    let validStr = not ('\"' `elem` str) && not ('\'' `elem` str)
    let validComment = not ('\"' `elem` comment) && not ('\'' `elem` comment)
    
    putStrLn $ "str = " ++ show str
    putStrLn $ "comment = " ++ show comment
    putStrLn $ "validStr = " ++ show validStr
    putStrLn $ "validComment = " ++ show validComment
    putStrLn $ "not (validStr && validComment) = " ++ show (not (validStr && validComment))
    putStrLn $ "null str && null comment = " ++ show (null str && null comment)
    
    if not (validStr && validComment) || null str && null comment
    then putStrLn "测试应该通过 (property True)"
    else do
        let stringWithComment = "\"" ++ str ++ " /* " ++ comment ++ " */\""
        let result = removeComments stringWithComment
        let commentStr = "/* " ++ comment ++ " */"
        let quotedStr = "\"" ++ str ++ "\""
        
        putStrLn $ "stringWithComment = " ++ show stringWithComment
        putStrLn $ "result = " ++ show result
        putStrLn $ "commentStr = " ++ show commentStr
        putStrLn $ "quotedStr = " ++ show quotedStr
        putStrLn $ "not (commentStr `isInfixOf` result) = " ++ show (not (commentStr `isInfixOf` result))
        putStrLn $ "quotedStr `isInfixOf` result = " ++ show (quotedStr `isInfixOf` result)
    
    -- IsCompleteStringLiteral invalid 失败案例: "'a
    putStrLn "\n--- IsCompleteStringLiteral invalid ---"
    let test3 = "'a"
    let result3 = isCompleteStringLiteral test3
    putStrLn $ "isCompleteStringLiteral " ++ show test3 ++ " = " ++ show result3 ++ " (期望: False)"
    
    -- IsProblematicUnclosedString 失败案例: ""
    putStrLn "\n--- IsProblematicUnclosedString ---"
    let test4 = ""
    let result4 = isProblematicUnclosedString test4
    putStrLn $ "isProblematicUnclosedString " ++ show test4 ++ " = " ++ show result4 ++ " (期望: True)"