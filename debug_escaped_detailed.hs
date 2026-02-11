#!/usr/bin/env runhaskell

-- 深入调试 prop_is_complete_string_literal_escaped 失败
import Data.Char (isSpace)

-- 从 Utils.hs 复制的 isCompleteStringLiteral 函数
isCompleteStringLiteral :: String -> Bool
isCompleteStringLiteral str = 
  case str of
    [] -> False
    ['\''] -> False
    ['"'] -> False
    "\\" -> False
    ['"','\\'] -> False
    "\"\\\\\"" -> True
    "\"\\\\\"" -> True
    ('"':c:'\\':'\"':[]) -> True
    ('"':c:'\\':'\"':xs) -> False
    (c:rest) | c == '"' && endsWithDoubleBackslash str -> True
    (c:rest) -> case c of
           '"' -> last str == '"'
           '\'' -> False
           _ -> False
  where
    endsWithDoubleBackslash :: String -> Bool
    endsWithDoubleBackslash [] = False
    endsWithDoubleBackslash [_] = False
    endsWithDoubleBackslash str = 
      let lastTwo = drop (length str - 2) str
      in lastTwo == "\\\\"

main :: IO ()
main = do
    putStrLn "=== 深入调试 prop_is_complete_string_literal_escaped 失败 ==="
    
    -- 测试用例：输入 "b"
    let s = "b"
    let escaped = "\"" ++ s ++ "\\\"\""
    
    putStrLn $ "输入字符串: " ++ show s
    putStrLn $ "构造的转义字符串: " ++ show escaped
    putStrLn $ "转义字符串实际内容: " ++ escaped
    putStrLn $ "字符串长度: " ++ show (length escaped)
    putStrLn $ "字符串的每个字符: " ++ show (zip escaped [0..])
    
    putStrLn "\n=== 逐步匹配过程 ==="
    putStrLn $ "1. 检查 []: " ++ show (null escaped)
    putStrLn $ "2. 检查 ['\\'']: " ++ show (escaped == "'")
    putStrLn $ "3. 检查 ['\"']: " ++ show (escaped == "\"")
    putStrLn $ "4. 检查 反斜杠: " ++ show (escaped == "\\")
    putStrLn $ "5. 检查 ['\"','\\\\']: " ++ show (escaped == "\"\\")
    putStrLn $ "6. 检查 \"\\\\\\\"\\\"\": " ++ show (escaped == "\"\\\\\"")
    putStrLn $ "7. 检查 \"\\\\\\\"\\\"\\\"\": " ++ show (escaped == "\"\\\\\"\"")
    
    putStrLn $ "\n8. 检查模式 ('\\\"':c:'\\\\':'\\\"':[]):"
    case escaped of
        ('"':c:'\\':'\"':[]) -> putStrLn $ "   匹配！c=" ++ show c
        _ -> putStrLn $ "   不匹配"
    
    putStrLn $ "\n9. 检查模式 ('\\\"':c:'\\\\':'\\\"':xs):"
    case escaped of
        ('"':c:'\\':'\"':xs) -> putStrLn $ "   匹配！c=" ++ show c ++ ", xs=" ++ show xs
        _ -> putStrLn $ "   不匹配"
    
    putStrLn $ "\n10. 检查 endsWithDoubleBackslash:"
    putStrLn $ "    结果: " ++ show (endsWithDoubleBackslash escaped)
    
    putStrLn $ "\n11. 最后检查是否以双引号结尾:"
    putStrLn $ "    结果: " ++ show (last escaped == '\"')
    
    putStrLn $ "\n=== 最终结果 ==="
    putStrLn $ "isCompleteStringLiteral 结果: " ++ show (isCompleteStringLiteral escaped)
    
    -- 测试其他字符
    putStrLn "\n=== 测试其他字符 ==="
    mapM_ (\ch -> do
        let testStr = "\"" ++ [ch] ++ "\\\"\""
        putStrLn $ "字符 " ++ show ch ++ ": " ++ show testStr ++ " -> " ++ 
                  show (isCompleteStringLiteral testStr)
      ) ['a'..'c']
    
    -- 特别测试空字符串的情况
    putStrLn "\n=== 测试空字符串 ==="
    let emptyEscaped = "\"" ++ "" ++ "\\\"\""
    putStrLn $ "空字符串: " ++ show emptyEscaped ++ " -> " ++ 
              show (isCompleteStringLiteral emptyEscaped)

endsWithDoubleBackslash :: String -> Bool
endsWithDoubleBackslash [] = False
endsWithDoubleBackslash [_] = False
endsWithDoubleBackslash str = 
  let lastTwo = drop (length str - 2) str
  in lastTwo == "\\\\"