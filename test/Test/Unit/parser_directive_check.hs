{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import SourceLocation (Located, locatedValue)

directiveValue :: Maybe (Located a) -> Maybe a
directiveValue = fmap locatedValue

main :: IO ()
main = do
    putStrLn "Testing file directive parsing in Parser module..."
    
    -- 测试包含文件指令的代码
    let testCode = unlines [
            "//! ownership: on",
            "//! dependent_types: off",
            "",
            "package main",
            "",
            "func main() {",
            "    println(\"Hello, World!\")",
            "}"
            ]
    
    case parseTypus testCode of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right ast -> do
            putStrLn "Parsed successfully!"
            let dirs = tfDirectives ast
                ownershipVal = directiveValue (fdOwnership dirs)
                dependentVal = directiveValue (fdDependentTypes dirs)
            putStrLn $ "Ownership directive: " ++ show ownershipVal
            putStrLn $ "Dependent types directive: " ++ show dependentVal
            
            -- 验证指令是否正确解析
            case (ownershipVal, dependentVal) of
                (Just True, Just False) -> putStrLn "✓ File directives parsed correctly!"
                _ -> putStrLn "✗ File directives not parsed correctly"
