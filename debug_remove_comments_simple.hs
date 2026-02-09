#!/usr/bin/env runhaskell

-- 导入Utils模块
import Utils
import Data.List (isInfixOf)

-- 测试removeComments函数的简单案例
main :: IO ()
main = do
    putStrLn "=== 测试removeComments函数 ==="
    
    -- 测试基本案例
    let testCases = 
            [ ("\"", "单个反斜杠")
            , ("", "空字符串")
            , ("\"hello\"", "简单字符串")
            , ("hello /* comment */ world", "字符串中的注释")
            , ("\"hello /* comment */ world\"", "字符串字面量中的注释")
            ]
    
    mapM_ (\(input, desc) -> do
        putStrLn $ desc ++ ": " ++ show input
        let result = removeComments input
        putStrLn $ "  结果: " ++ show result
        putStrLn ""
        ) testCases
    
    -- 测试失败的特定案例
    putStrLn "=== 测试失败的特定案例 ==="
    let str = "\""
    let comment = ""
    putStrLn $ "str: " ++ show str
    putStrLn $ "comment: " ++ show comment
    
    let validStr = not ('\"' `elem` str) && not ('\'' `elem` str)
    let validComment = not ('\"' `elem` comment) && not ('\'' `elem` comment)
    putStrLn $ "validStr: " ++ show validStr
    putStrLn $ "validComment: " ++ show validComment
    putStrLn $ "should pass: " ++ show (not (validStr && validComment) || null str && null comment)
    
    if not (validStr && validComment) || null str && null comment
        then putStrLn "测试应该通过"
        else do
            let stringWithComment = "\"" ++ str ++ " /* " ++ comment ++ " */\""
            let result = removeComments stringWithComment
            let commentStr = "/* " ++ comment ++ " */"
            let quotedStr = "\"" ++ str ++ "\""
            putStrLn $ "stringWithComment: " ++ show stringWithComment
            putStrLn $ "result: " ++ show result
            putStrLn $ "commentStr `isInfixOf` result: " ++ show (commentStr `isInfixOf` result)
            putStrLn $ "quotedStr `isInfixOf` result: " ++ show (quotedStr `isInfixOf` result)
            putStrLn $ "final result: " ++ show (not (commentStr `isInfixOf` result) && quotedStr `isInfixOf` result)