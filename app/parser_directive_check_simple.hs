module Main where

import Parser

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
    
    putStrLn "Parsing the code..."
    case parseTypus testCode of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right ast -> do
            putStrLn "Parsed successfully!"
            let dirs = tfDirectives ast
            putStrLn $ "Ownership directive: " ++ show (fdOwnership dirs)
            putStrLn $ "Dependent types directive: " ++ show (fdDependentTypes dirs)
            
            -- 验证指令是否正确解析
            case (fdOwnership dirs, fdDependentTypes dirs) of
                (Just True, Just False) -> putStrLn "✓ File directives parsed correctly!"
                _ -> putStrLn "✗ File directives not parsed correctly"