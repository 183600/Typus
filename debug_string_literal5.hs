import Utils
import Test.QuickCheck

-- | 测试isCompleteStringLiteral对无效字符串的处理
prop_is_complete_string_literal_invalid :: String -> Property
prop_is_complete_string_literal_invalid s =
  let validS = take 50 s
      stringWithoutEndQuote = "\"" ++ validS ++ "\\"  -- 添加反斜杠确保字符串不完整
      stringWithoutEndQuoteSingle = "'" ++ validS ++ "\\"  -- 单引号版本
  in if null validS
     then property (not (isCompleteStringLiteral "\"\\") &&  -- 反斜杠后没有引号，应该是不完整的
                 not (isCompleteStringLiteral "'\\"))      -- 单引号版本同理
     else property (not (isCompleteStringLiteral stringWithoutEndQuote) &&
                 not (isCompleteStringLiteral stringWithoutEndQuoteSingle))

main :: IO ()
main = do
    putStrLn "Testing prop_is_complete_string_literal_invalid..."
    
    -- Test the null string case
    putStrLn $ "Null string case: testing..."
    
    -- Test the specific case from the error message
    let specificInput = "\""
        specificResult = isCompleteStringLiteral specificInput
    putStrLn $ "Specific input " ++ show specificInput ++ ": " ++ show specificResult
    putStrLn $ "Expected: False"
    
    -- Run the property test with a specific seed that might trigger the failure
    putStrLn "\nRunning property test..."
    quickCheckWith stdArgs { maxSuccess = 100 } prop_is_complete_string_literal_invalid