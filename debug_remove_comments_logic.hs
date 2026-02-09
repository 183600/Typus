module Main where

import Utils
import Data.List (isInfixOf)

main :: IO ()
main = do
    putStrLn "=== 测试 RemoveComments strings with comments 的逻辑 ==="
    
    -- 模拟测试失败案例: str = "\\" 和 comment = ""
    let str = "\\"
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