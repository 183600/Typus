#!/usr/bin/env runhaskell

-- 详细调试 isCompleteStringLiteral 函数
import Data.Char (isSpace, isPrint)

-- 完整的 isCompleteStringLiteral 函数
isCompleteStringLiteral :: String -> Bool
isCompleteStringLiteral str = 
  case str of
    [] -> False
    ['\''] -> False
    ['"'] -> False
    "\\" -> False
    ['"','\\'] -> False
    "\"\\\"\"" -> True
    ('"':c:'\\':'\"':[]) -> True
    ('"':c:'\\':'\"':xs) -> False
    (c:rest) | c == '"' -> last str == '"'
              | otherwise -> False

main :: IO ()
main = do
    putStrLn "=== 详细调试 isCompleteStringLiteral ==="
    
    -- 测试用例：输入 "b"
    let s = "b"
    let escaped = "\"" ++ s ++ "\\\"\""
    
    putStrLn $ "输入字符串: " ++ show s
    putStrLn $ "构造的转义字符串: " ++ show escaped
    putStrLn $ "字符串长度: " ++ show (length escaped)
    putStrLn $ "字符串的每个字符: " ++ show (zip escaped [0..])
    
    -- 逐步匹配
    putStrLn "\n=== 逐步匹配过程 ==="
    putStrLn $ "1. 空字符串: " ++ show (null escaped)
    putStrLn $ "2. ['\'']: " ++ show (escaped == "'")
    putStrLn $ "3. ['\"']: " ++ show (escaped == "\"")
    putStrLn $ "4. \"\\\\\": " ++ show (escaped == "\\")
    putStrLn $ "5. ['\"','\\\\']: " ++ show (escaped == "\"\\")
    putStrLn $ "6. \"\\\\\\\"\\\"\": " ++ show (escaped == "\"\\\"\"")
    
    putStrLn $ "\n7. 模式 ('\\\"':c:'\\\\':'\\\"':[]):"
    case escaped of
        ('"':c:'\\':'\"':[]) -> putStrLn $ "   匹配！c=" ++ show c ++ ", 结果: True"
        _ -> putStrLn $ "   不匹配"
    
    putStrLn $ "\n8. 模式 ('\\\"':c:'\\\\':'\\\"':xs):"
    case escaped of
        ('"':c:'\\':'\"':xs) -> putStrLn $ "   匹配！c=" ++ show c ++ ", xs=" ++ show xs ++ ", 结果: False"
        _ -> putStrLn $ "   不匹配"
    
    putStrLn $ "\n9. 通用规则 (c:rest) | c == '\"':"
    case escaped of
        (c:rest) -> putStrLn $ "   c=" ++ show c ++ ", last str == '\"': " ++ show (last escaped == '"')
        [] -> putStrLn $ "   空字符串"
    
    putStrLn $ "\n=== 最终结果 ==="
    putStrLn $ "isCompleteStringLiteral 结果: " ++ show (isCompleteStringLiteral escaped)
    
    -- 分析
    putStrLn "\n=== 分析 ==="
    putStrLn $ "字符串 \"" ++ escaped ++ "\" 的实际内容是: " ++ show (init $ tail escaped)
    putStrLn $ "这表示一个包含字符 " ++ show s ++ " 和 \" 的字符串"
    putStrLn $ "作为一个字符串字面量，它以引号开头和结尾，应该是完整的"