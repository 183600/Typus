#!/usr/bin/env runhaskell

import qualified Utils
import Data.List (isInfixOf)

-- 测试RemoveComments strings with comments
testRemoveComments :: IO ()
testRemoveComments = do
    let str = ""
    let comment = "a"
    let stringWithComment = "\"" ++ str ++ " /* " ++ comment ++ " */\""
    let result = Utils.removeComments stringWithComment
    let commentStr = "/* " ++ comment ++ " */"
    let quotedStr = "\"" ++ str ++ "\""
    putStrLn $ "Input: " ++ show stringWithComment
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Comment removed: " ++ show (not (commentStr `isInfixOf` result))
    putStrLn $ "String preserved: " ++ show (quotedStr `isInfixOf` result)
    putStrLn ""

-- 测试isCompleteStringLiteral
testIsCompleteStringLiteral :: IO ()
testIsCompleteStringLiteral = do
    let testCases = 
            [ ("\\", "单个反斜杠")
            , ("\"", "单个双引号")
            , ("\\\"", "转义双引号")
            ]
    mapM_ (\(input, desc) -> do
        putStrLn $ desc ++ ": " ++ show input ++ " -> " ++ show (Utils.isCompleteStringLiteral input)
        ) testCases
    putStrLn ""

-- 测试isProblematicUnclosedString
testIsProblematicUnclosedString :: IO ()
testIsProblematicUnclosedString = do
    let testCases = 
            [ ("\"", "单个双引号")
            , ("'", "单个单引号")
            ]
    mapM_ (\(input, desc) -> do
        putStrLn $ desc ++ ": " ++ show input ++ " -> " ++ show (Utils.isProblematicUnclosedString input)
        ) testCases
    putStrLn ""

main :: IO ()
main = do
    putStrLn "=== Testing RemoveComments ==="
    testRemoveComments
    
    putStrLn "=== Testing isCompleteStringLiteral ==="
    testIsCompleteStringLiteral
    
    putStrLn "=== Testing isProblematicUnclosedString ==="
    testIsProblematicUnclosedString