import Utils
import Test.QuickCheck
import Control.Monad (forM_)

-- 测试 prop_is_complete_string_literal_invalid 的行为
testProp :: IO ()
testProp = do
  putStrLn "测试 prop_is_complete_string_literal_invalid 的行为..."
  
  -- 手动测试一些输入
  let testInputs = ["", "a", "hello", "'", "\"", "\"\\", "'\\"]
  
  forM_ testInputs $ \s -> do
    let validS = take 50 s
    let stringWithoutEndQuote = "\"" ++ validS ++ "\\"
    let stringWithoutEndQuoteSingle = "'" ++ validS ++ "\\"
    
    putStrLn $ "\n输入 s = " ++ show s
    putStrLn $ "  validS = " ++ show validS
    
    if null validS
    then do
      let test1 = isCompleteStringLiteral "\"\\"
      let test2 = isCompleteStringLiteral "'\\"
      putStrLn $ "  测试 \"\\\\\" -> " ++ show test1 ++ " (期望 False)"
      putStrLn $ "  测试 \"'\\\\\" -> " ++ show test2 ++ " (期望 False)"
      putStrLn $ "  结果: " ++ show (not test1 && not test2)
    else do
      let test1 = isCompleteStringLiteral stringWithoutEndQuote
      let test2 = isCompleteStringLiteral stringWithoutEndQuoteSingle
      putStrLn $ "  测试 " ++ show stringWithoutEndQuote ++ " -> " ++ show test1 ++ " (期望 False)"
      putStrLn $ "  测试 " ++ show stringWithoutEndQuoteSingle ++ " -> " ++ show test2 ++ " (期望 False)"
      putStrLn $ "  结果: " ++ show (not test1 && not test2)

-- 测试 prop_is_problematic_unclosed_string 的行为
testProp2 :: IO ()
testProp2 = do
  putStrLn "\n测试 prop_is_problematic_unclosed_string 的行为..."
  
  -- 手动测试一些输入
  let testInputs = ["", "a", "hello"]
  
  forM_ testInputs $ \s -> do
    let validS = take 30 s
    let problematicString = "\"\\\"" ++ validS
    
    putStrLn $ "\n输入 s = " ++ show s
    putStrLn $ "  validS = " ++ show validS
    
    if null validS
    then do
      let result = isProblematicUnclosedString "\"\\\""
      putStrLn $ "  测试 \"\\\\\\\"\" -> " ++ show result ++ " (期望 True)"
    else do
      let result = isProblematicUnclosedString problematicString
      putStrLn $ "  测试 " ++ show problematicString ++ " -> " ++ show result ++ " (期望 True)"

-- 直接测试 QuickCheck 生成的数据
testQuickCheckGenerated :: IO ()
testQuickCheckGenerated = do
  putStrLn "\n直接测试 QuickCheck 可能生成的数据..."
  
  let testInputs = ["'", "\"", "\"\\", "'\\", "\"\\\"", "'\\\"", "\\", ""]
  
  forM_ testInputs $ \input -> do
    let result1 = isCompleteStringLiteral input
    let result2 = isProblematicUnclosedString input
    
    putStrLn $ "\n输入: " ++ show input
    putStrLn $ "  isCompleteStringLiteral: " ++ show result1
    putStrLn $ "  isProblematicUnclosedString: " ++ show result2
    
    -- 检查是否可能是测试失败的根源
    if input == "'"
    then do
      putStrLn $ "  这是测试失败中提到的输入！"
      putStrLn $ "  如果测试期望 isCompleteStringLiteral \"'\" = False，实际是 " ++ show result1
      putStrLn $ "  如果测试期望 isProblematicUnclosedString \"'\" = True，实际是 " ++ show result2
    else return ()

main :: IO ()
main = do
  testProp
  testProp2
  testQuickCheckGenerated