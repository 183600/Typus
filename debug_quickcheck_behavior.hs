#!/usr/bin/env runhaskell

-- 导入必要的模块
import Utils
import Test.QuickCheck

-- 手动实现测试属性来观察行为
prop_is_complete_string_literal_invalid_debug :: String -> Property
prop_is_complete_string_literal_invalid_debug s =
  let validS = take 50 s
      stringWithoutEndQuote = "\"" ++ validS ++ "\\"  -- 添加反斜杠确保字符串不完整
      stringWithoutEndQuoteSingle = "'" ++ validS ++ "\\"  -- 单引号版本
  in if null validS
     then property (not (isCompleteStringLiteral "\"\\") &&  -- 反斜杠后没有引号，应该是不完整的
                 not (isCompleteStringLiteral "'\\"))      -- 单引号版本同理
     else property (not (isCompleteStringLiteral stringWithoutEndQuote) &&
                 not (isCompleteStringLiteral stringWithoutEndQuoteSingle))

prop_is_complete_string_literal_escaped_quotes_debug :: String -> Property
prop_is_complete_string_literal_escaped_quotes_debug s =
  let validS = take 50 s
      -- 确保字符串不为空，包含转义引号和闭合引号
      stringWithEscapedQuotes = if null validS 
                               then "\"\\\"\""  -- 最小有效字符串
                               else "\"" ++ validS ++ "\\\"" ++ validS ++ "\""
  in if null validS
     then property $ isCompleteStringLiteral "\"\\\"\""  -- 最小有效字符串应该是完整的
     else property $ isCompleteStringLiteral stringWithEscapedQuotes

prop_is_problematic_unclosed_string_debug :: String -> Property
prop_is_problematic_unclosed_string_debug s =
  let validS = take 30 s
      -- 确保字符串以引号开头，后跟反斜杠，并且不是完整的字符串字面量
      problematicString = "\"\\\"" ++ validS  -- 不添加结尾引号，确保不完整
  in if null validS
     then property $ isProblematicUnclosedString "\"\\\""  -- 包含转义引号但不完整的字符串
     else property $ isProblematicUnclosedString problematicString

-- 测试函数
main :: IO ()
main = do
    putStrLn "=== 手动测试QuickCheck属性 ==="
    
    -- 测试空字符串情况
    putStrLn "测试空字符串情况:"
    putStrLn $ "  prop_is_complete_string_literal_invalid \"\": 计算中..."
    putStrLn $ "  prop_is_complete_string_literal_escaped_quotes \"\": 计算中..."
    putStrLn $ "  prop_is_problematic_unclosed_string \"\": 计算中..."
    
    putStrLn "\n测试特定字符串:"
    let testStrings = ["a", "hello", "\"", "\\", "\"\\", "\"\\\"", "\"\\\"\""]
    
    mapM_ (\s -> do
        putStrLn $ "\n字符串: " ++ show s
        putStrLn $ "  isCompleteStringLiteral: " ++ show (isCompleteStringLiteral s)
        putStrLn $ "  isProblematicUnclosedString: " ++ show (isProblematicUnclosedString s)
        
        -- 计算属性结果
        let validS = take 50 s
        let stringWithoutEndQuote = "\"" ++ validS ++ "\\"
        let stringWithoutEndQuoteSingle = "'" ++ validS ++ "\\"
        let stringWithEscapedQuotes = if null validS 
                                    then "\"\\\"\""  
                                    else "\"" ++ validS ++ "\\\"" ++ validS ++ "\""
        let problematicString = "\"\\\"" ++ take 30 s
        
        putStrLn $ "  属性计算:"
        putStrLn $ "    invalid属性: " ++ show (if null validS 
                                              then not (isCompleteStringLiteral "\"\\") && not (isCompleteStringLiteral "'\\")
                                              else not (isCompleteStringLiteral stringWithoutEndQuote) && not (isCompleteStringLiteral stringWithoutEndQuoteSingle))
        putStrLn $ "    escaped属性: " ++ show (if null validS 
                                              then isCompleteStringLiteral "\"\\\"\""
                                              else isCompleteStringLiteral stringWithEscapedQuotes)
        putStrLn $ "    problematic属性: " ++ show (if null validS 
                                                   then isProblematicUnclosedString "\"\\\""
                                                   else isProblematicUnclosedString problematicString)
        ) testStrings