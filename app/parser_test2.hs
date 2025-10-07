module Main where

import Parser (parseTypus)

main :: IO ()
main = do
    putStrLn "Testing Parser.parseTypus function..."
    
    -- 测试一个简单的有效代码
    let simpleCode = unlines [
            "package main",
            "",
            "func main() {",
            "    println(\"Hello, World!\")",
            "}"
            ]
    
    case parseTypus simpleCode of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right ast -> putStrLn $ "Parsed successfully: " ++ show ast